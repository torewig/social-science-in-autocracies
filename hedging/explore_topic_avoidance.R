# ---------------------------------------------------------------------------
# EXPLORATORY. Topic avoidance at the author level.
#
# Everything before this conditioned on the author already writing about
# power. Here the outcome is whether they do at all, and how close to
# political subject matter the paper sits. Every paper contributes, and the
# outcomes are near-deterministic per paper rather than noisy rates from small
# denominators, which is the fix for the noise problem the HRD boundary test
# exposed.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
vdem_path <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                    "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")

con <- dbConnect(duckdb())
power_flag <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(has_power_clause AS INTEGER) AS has_power_clause
  FROM read_parquet('%s/document_power_flag.parquet')", probe_dir)))
topic_score <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, power_proximity_score, CAST(n_noun_tokens AS INTEGER) AS n_noun_tokens
  FROM read_parquet('%s/document_topic_score.parquet')", probe_dir)))
power_country <- as.data.table(dbGetQuery(con, sprintf("
  SELECT DISTINCT ut, mentioned_iso3 FROM read_parquet('%s/power_sentence_country.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))[, .(ut, iso3, year, field)]
authors <- fread("/home/martigso/Dropbox/postdoc/liberal_socialscience/data/author.csv",
                 select = c("ut", "daisng_id"))

papers <- merge(documents, power_flag, by = "ut")
papers <- merge(papers, topic_score, by = "ut", all.x = TRUE)
papers <- merge(papers, authors[!is.na(daisng_id)], by = "ut")

referent <- merge(power_country, documents[, .(ut, iso3)], by = "ut")
domestic <- referent[mentioned_iso3 == iso3, .(has_domestic_power = 1L), by = ut]
foreign  <- referent[mentioned_iso3 != iso3, .(has_foreign_power = 1L), by = ut]
papers <- merge(merge(papers, domestic, by = "ut", all.x = TRUE), foreign, by = "ut", all.x = TRUE)
papers[is.na(has_domestic_power), has_domestic_power := 0L]
papers[is.na(has_foreign_power), has_foreign_power := 0L]

vdem <- as.data.table(readRDS(vdem_path))
treatments <- c(academic_freedom = "v2xca_academ",
                academic_exchange = "v2cafexch",
                academic_research = "v2cafres")
vdem <- vdem[, c(.(iso3 = country_text_id, year = year), .SD), .SDcols = unname(treatments)]
for (v in unname(treatments)) set(vdem, j = v, value = scale(vdem[[v]])[, 1])
papers <- merge(papers, vdem, by = c("iso3", "year"))

cat(sprintf("papers %d | authors %d | countries %d\n",
            nrow(papers), uniqueN(papers$daisng_id), uniqueN(papers$iso3)))
cat(sprintf("share with any power clause %.3f | domestic %.3f | foreign %.3f\n",
            mean(papers$has_power_clause), mean(papers$has_domestic_power),
            mean(papers$has_foreign_power)))

outcomes <- c(any_power_clause = "has_power_clause",
              domestic_power = "has_domestic_power",
              foreign_power = "has_foreign_power",
              power_proximity = "power_proximity_score")

cat("\n", sprintf("%-20s %-22s %-24s", "outcome", "treatment", "author + year FE"), "\n")
cat(strrep("-", 72), "\n")
for (outcome_label in names(outcomes)) {
  for (treatment_label in names(treatments)) {
    y <- outcomes[[outcome_label]]; x <- treatments[[treatment_label]]
    fit <- feols(as.formula(sprintf("%s ~ %s | daisng_id + year", y, x)),
                 papers[!is.na(get(y)) & !is.na(get(x))], cluster = ~iso3)
    star <- if (pvalue(fit)[x] < .01) "**" else if (pvalue(fit)[x] < .05) "*" else
            if (pvalue(fit)[x] < .1) "." else " "
    cat(sprintf("%-20s %-22s %+8.5f (p %.3f)%s n=%d\n", outcome_label, treatment_label,
                coef(fit)[x], pvalue(fit)[x], star, nobs(fit)))
  }
}

cat("\nNegative means: less academic freedom goes with LESS of this outcome.\n")
cat("Baseline rates above give the scale for the binary outcomes.\n")
saveRDS(papers, file.path(probe_dir, "topic_avoidance_papers.rds"))
