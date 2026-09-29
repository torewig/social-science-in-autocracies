# ---------------------------------------------------------------------------
# Clause-level hedging contrast, with a three-way referent split.
#
# The earlier placebo lumped together papers about a foreign country and
# papers that name no country at all. Only the first is a real placebo: a
# paper about someone else's government has power clauses to hedge and no
# reason for the author to be careful. Papers naming no country may simply
# have less at stake in every clause.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)

hedging_dir <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging"
source(file.path(hedging_dir, "probe_panel_helpers.R"))

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
vdem_path <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                    "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")

MIN_WORDS_PER_SIDE <- 20L

con <- dbConnect(duckdb())
hedging <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(in_power_clause AS INTEGER) AS in_power_clause,
         CAST(n_words AS INTEGER) AS n_words,
         CAST(n_hedge_core AS INTEGER) AS n_hedge_core,
         CAST(n_booster_core AS INTEGER) AS n_booster_core
  FROM read_parquet('%s/document_clause_hedging.parquet')", probe_dir)))
country_mentions <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, iso3 AS mentioned_iso3, CAST(n_name_mentions AS INTEGER) AS n_name_mentions
  FROM read_parquet('%s/document_country.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

wide <- dcast(hedging, ut ~ in_power_clause,
              value.var = c("n_words", "n_hedge_core", "n_booster_core"), fill = 0L)
setnames(wide,
         c("n_words_0", "n_words_1", "n_hedge_core_0", "n_hedge_core_1",
           "n_booster_core_0", "n_booster_core_1"),
         c("n_words_other", "n_words_power", "n_hedge_other", "n_hedge_power",
           "n_booster_other", "n_booster_power"))

usable <- wide$n_words_power >= MIN_WORDS_PER_SIDE & wide$n_words_other >= MIN_WORDS_PER_SIDE
wide[, hedge_contrast_clause := fifelse(usable,
  1000 * n_hedge_power / n_words_power - 1000 * n_hedge_other / n_words_other, NA_real_)]
wide[, booster_contrast_clause := fifelse(usable,
  1000 * n_booster_power / n_words_power - 1000 * n_booster_other / n_words_other, NA_real_)]

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))
wide <- merge(wide, documents[, .(ut, year, iso3, field)], by = "ut")
rm(documents); invisible(gc())

# Three-way referent split
own <- merge(country_mentions, wide[, .(ut, iso3)], by = "ut")
own <- own[mentioned_iso3 == iso3, .(names_own = 1L), by = ut]
any_country <- country_mentions[, .(names_any = 1L), by = ut]
wide <- merge(wide, own, by = "ut", all.x = TRUE)
wide <- merge(wide, any_country, by = "ut", all.x = TRUE)
wide[is.na(names_own), names_own := 0L]
wide[is.na(names_any), names_any := 0L]
wide[, referent := fifelse(names_own == 1, "own country",
                   fifelse(names_any == 1, "foreign country only", "no country named"))]

vdem <- load_vdem(vdem_path)

cat("\n############ CLAUSE-LEVEL HEDGING CONTRAST ############\n")
report("All documents", wide, "hedge_contrast_clause", vdem)
for (group in c("own country", "foreign country only", "no country named")) {
  report(sprintf("hedging | %s", group), wide[referent == group], "hedge_contrast_clause", vdem)
}

cat("\n############ BOOSTING, SAME SPLIT (specificity check) ############\n")
for (group in c("own country", "foreign country only")) {
  report(sprintf("boosting | %s", group), wide[referent == group], "booster_contrast_clause", vdem)
}

saveRDS(wide, file.path(probe_dir, "document_clause_hedging_measures.rds"))
