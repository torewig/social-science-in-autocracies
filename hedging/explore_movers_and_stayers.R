# ---------------------------------------------------------------------------
# EXPLORATORY. Movers against stayers.
#
# An author's origin is the affiliation country in their earliest observed
# year. A paper is "away" when its affiliation country differs from the
# origin. Treatment is academic freedom in the ORIGIN country in that year:
# the environment the stayer is still in and the mover has left.
#
#   outcome ~ origin_freedom * away | author + year
#
# The main effect is how stayers respond to conditions at home. The
# interaction is whether people who left follow the same trend. If caution is
# imposed by the environment, the interaction should offset the main effect.
# If it is training or habituation, movers should keep tracking home.
#
# "Writes about origin country" is included because it is the one topic
# outcome that stays meaningful after a move: affiliation changes, but whether
# you still study the place you came from does not depend on where you sit.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
papers <- readRDS(file.path(probe_dir, "topic_avoidance_papers.rds"))

con <- dbConnect(duckdb())
country_mentions <- as.data.table(dbGetQuery(con, sprintf("
  SELECT DISTINCT ut, iso3 AS mentioned_iso3 FROM read_parquet('%s/document_country.parquet')", probe_dir)))
hedging <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(in_power_clause AS INTEGER) AS in_power_clause,
         CAST(n_words AS INTEGER) AS n_words, CAST(n_hedge_core AS INTEGER) AS n_hedge_core
  FROM read_parquet('%s/document_clause_hedging.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

# ── Origin and move status ─────────────────────────────────────────────────
setorder(papers, daisng_id, year)
origin <- papers[, .(origin_iso3 = iso3[which.min(year)],
                     first_year = min(year)), by = daisng_id]
papers <- merge(papers, origin, by = "daisng_id")
papers[, away := as.integer(iso3 != origin_iso3)]

author_type <- papers[, .(n_papers = .N, moved = max(away), n_countries = uniqueN(iso3)),
                      by = daisng_id]
cat(sprintf("authors %d | movers %d | authors with 2+ papers %d | movers with 2+ papers %d\n",
            nrow(author_type), sum(author_type$moved), sum(author_type$n_papers >= 2),
            author_type[n_papers >= 2 & moved == 1, .N]))
cat(sprintf("movers observed both at origin and away: %d\n",
            papers[, .(both = uniqueN(away)), by = daisng_id][both == 2, .N]))

# ── Outcome: still writing about the origin country ────────────────────────
writes_origin <- merge(country_mentions, papers[, .(ut, origin_iso3)], by = "ut")
writes_origin <- writes_origin[mentioned_iso3 == origin_iso3, .(writes_about_origin = 1L), by = ut]
papers <- merge(papers, writes_origin, by = "ut", all.x = TRUE)
papers[is.na(writes_about_origin), writes_about_origin := 0L]

# ── Treatment: academic freedom in the origin country, that year ───────────
freedom <- unique(papers[, .(origin_iso3 = iso3, year, origin_freedom = v2xca_academ)])
freedom <- freedom[, .(origin_freedom = mean(origin_freedom)), by = .(origin_iso3, year)]
papers <- merge(papers, freedom, by = c("origin_iso3", "year"), all.x = TRUE)

# ── Hedging contrast, where both sides exist ───────────────────────────────
wide <- dcast(hedging, ut ~ in_power_clause, value.var = c("n_words", "n_hedge_core"), fill = 0L)
setnames(wide, c("n_words_0","n_words_1","n_hedge_core_0","n_hedge_core_1"),
         c("w_other","w_power","h_other","h_power"))
wide <- wide[w_power >= 1 & w_other >= 1]
wide[, hedge_contrast := 1000 * h_power / w_power - 1000 * h_other / w_other]
papers <- merge(papers, wide[, .(ut, hedge_contrast)], by = "ut", all.x = TRUE)

estimable <- papers[!is.na(origin_freedom)]
cat(sprintf("\nestimation sample: %d papers | %d authors | %d origin countries\n",
            nrow(estimable), uniqueN(estimable$daisng_id), uniqueN(estimable$origin_iso3)))
cat(sprintf("papers written away from origin: %d (%.1f%%)\n",
            sum(estimable$away), 100 * mean(estimable$away)))

outcomes <- c(writes_about_origin = "writes_about_origin",
              any_power_clause    = "has_power_clause",
              power_proximity     = "power_proximity_score",
              hedging_contrast    = "hedge_contrast")

cat("\n", sprintf("%-20s %14s %14s %12s", "outcome", "origin freedom", "x away", "n"), "\n")
cat(strrep("-", 66), "\n")
for (label in names(outcomes)) {
  y <- outcomes[[label]]
  fit <- feols(as.formula(sprintf("%s ~ origin_freedom * away | daisng_id + year", y)),
               estimable[!is.na(get(y))], cluster = ~origin_iso3)
  terms <- c("origin_freedom", "origin_freedom:away")
  if (!all(terms %in% names(coef(fit)))) { cat(sprintf("%-20s not estimable\n", label)); next }
  cat(sprintf("%-20s %+7.4f (p%.3f) %+7.4f (p%.3f) %10d\n", label,
              coef(fit)[terms[1]], pvalue(fit)[terms[1]],
              coef(fit)[terms[2]], pvalue(fit)[terms[2]], nobs(fit)))
}

cat("\n--- movers only, before vs after the move ---\n")
movers <- estimable[daisng_id %in% papers[, .(both = uniqueN(away)), by = daisng_id][both == 2, daisng_id]]
cat(sprintf("movers observed on both sides: %d authors, %d papers\n",
            uniqueN(movers$daisng_id), nrow(movers)))
for (label in names(outcomes)) {
  y <- outcomes[[label]]
  fit <- feols(as.formula(sprintf("%s ~ away | daisng_id + year", y)),
               movers[!is.na(get(y))], cluster = ~origin_iso3)
  if (!("away" %in% names(coef(fit)))) { cat(sprintf("%-20s not estimable\n", label)); next }
  cat(sprintf("%-20s away %+7.4f (se %.4f, p %.3f) n=%d\n", label,
              coef(fit)["away"], se(fit)["away"], pvalue(fit)["away"], nobs(fit)))
}
saveRDS(estimable, file.path(probe_dir, "mover_stayer_papers.rds"))
