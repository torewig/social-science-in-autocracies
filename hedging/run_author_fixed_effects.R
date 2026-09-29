# ---------------------------------------------------------------------------
# Author fixed-effects estimation of SPEC_frozen_2026-09-11.md.
#
# Primary: does the same author hedge power clauses more, relative to their
# own other clauses, when academic freedom in their country is lower?
#
# This is the composition test. A null here means the country-year association
# found earlier is about who publishes, not about how anyone writes.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(fixest)

hedging_dir <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging"
source(file.path(hedging_dir, "probe_panel_helpers.R"))

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
vdem_path <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                    "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")
author_path <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/data/author.csv"

con <- dbConnect(duckdb())
hedging <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(in_power_clause AS INTEGER) AS in_power_clause,
         CAST(n_words AS INTEGER) AS n_words,
         CAST(n_hedge_core AS INTEGER) AS n_hedge_core,
         CAST(n_booster_core AS INTEGER) AS n_booster_core
  FROM read_parquet('%s/document_clause_hedging.parquet')", probe_dir)))
country_mentions <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, iso3 AS mentioned_iso3 FROM read_parquet('%s/document_country.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

wide <- dcast(hedging, ut ~ in_power_clause,
              value.var = c("n_words", "n_hedge_core", "n_booster_core"), fill = 0L)
setnames(wide,
         c("n_words_0", "n_words_1", "n_hedge_core_0", "n_hedge_core_1",
           "n_booster_core_0", "n_booster_core_1"),
         c("n_words_other", "n_words_power", "n_hedge_other", "n_hedge_power",
           "n_booster_other", "n_booster_power"))

# Primary inclusion: at least one word on each side.
wide <- wide[n_words_power >= 1 & n_words_other >= 1]
wide[, hedge_contrast := 1000 * n_hedge_power / n_words_power -
                         1000 * n_hedge_other / n_words_other]
wide[, booster_contrast := 1000 * n_booster_power / n_words_power -
                           1000 * n_booster_other / n_words_other]
wide[, meets_20_word_rule := n_words_power >= 20 & n_words_other >= 20]

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))
wide <- merge(wide, documents[, .(ut, year, iso3, field)], by = "ut")
rm(documents); invisible(gc())

authors <- fread(author_path, select = c("ut", "daisng_id"))
wide <- merge(wide, authors[!is.na(daisng_id)], by = "ut")

own <- merge(country_mentions, wide[, .(ut, iso3)], by = "ut")[mentioned_iso3 == iso3,
        .(names_own = 1L), by = ut]
any_country <- country_mentions[, .(names_any = 1L), by = ut]
wide <- merge(merge(wide, own, by = "ut", all.x = TRUE), any_country, by = "ut", all.x = TRUE)
wide[is.na(names_own), names_own := 0L][is.na(names_any), names_any := 0L]
wide[, referent := fifelse(names_own == 1, "own country",
                   fifelse(names_any == 1, "foreign country only", "no country named"))]

vdem <- load_vdem(vdem_path)
panel <- merge(wide, vdem, by = c("iso3", "year"))
panel <- panel[!is.na(v2xca_academ) & !is.na(hedge_contrast)]

# ── Effective sample, as the spec requires before anything is interpreted ──
within_author <- panel[, .(documents = .N,
                           freedom_range = max(v2xca_academ) - min(v2xca_academ)),
                       by = daisng_id]
informative_authors <- within_author[documents >= 2 & freedom_range > 0]
cat("############ EFFECTIVE SAMPLE ############\n")
cat(sprintf("documents: %d | authors: %d\n", nrow(panel), uniqueN(panel$daisng_id)))
cat(sprintf("authors with 2+ documents: %d\n", nrow(within_author[documents >= 2])))
cat(sprintf("authors with 2+ documents AND within-author variation in academic freedom: %d\n",
            nrow(informative_authors)))
cat(sprintf("documents contributed by those authors: %d\n",
            panel[daisng_id %in% informative_authors$daisng_id, .N]))

estimate <- function(label, data, outcome, formula_text) {
  if (nrow(data) < 100) { cat(sprintf("\n%-52s too few documents (%d)\n", label, nrow(data))); return(invisible(NULL)) }
  fit <- try(feols(as.formula(sprintf(formula_text, outcome)), data, cluster = ~iso3), silent = TRUE)
  if (inherits(fit, "try-error") || !("v2xca_academ" %in% names(coef(fit)))) {
    cat(sprintf("\n%-52s not estimable\n", label)); return(invisible(NULL))
  }
  cat(sprintf("\n%-52s beta %+.4f (se %.4f, p %.3f) | docs %d\n",
              label, coef(fit)["v2xca_academ"], se(fit)["v2xca_academ"],
              pvalue(fit)["v2xca_academ"], nobs(fit)))
  invisible(fit)
}

author_year <- "%s ~ v2xca_academ | daisng_id + year"

cat("\n############ PRIMARY ############\n")
estimate("hedging contrast, author + year FE", panel, "hedge_contrast", author_year)

cat("\n############ SECONDARY ############\n")
for (group in c("own country", "foreign country only", "no country named")) {
  estimate(sprintf("hedging | %s", group), panel[referent == group], "hedge_contrast", author_year)
}
estimate("BOOSTER PLACEBO, author + year FE", panel, "booster_contrast", author_year)
estimate("movers only (2+ countries)",
         panel[daisng_id %in% panel[, .(n = uniqueN(iso3)), by = daisng_id][n >= 2, daisng_id]],
         "hedge_contrast", author_year)

cat("\n############ ROBUSTNESS ############\n")
estimate("20 words per side", panel[meets_20_word_rule == TRUE], "hedge_contrast", author_year)
estimate("+ log GDP and log population", panel, "hedge_contrast",
         "%s ~ v2xca_academ + log_gdppc + log_pop | daisng_id + year")
estimate("country + year FE (no author FE)", panel, "hedge_contrast",
         "%s ~ v2xca_academ | iso3 + year")
estimate("author + year FE, field absorbed", panel, "hedge_contrast",
         "%s ~ v2xca_academ | daisng_id + year + field")

saveRDS(panel, file.path(probe_dir, "author_panel.rds"))
