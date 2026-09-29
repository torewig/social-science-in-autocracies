# ---------------------------------------------------------------------------
# Does the within-document power-vs-other contrast track regime?
#
# Input:  ~/wos_parsed/hedging_probe/document_contrast.parquet, written by
#         probe_clause_measure.sql
# Asks:   (1) do the pooled baselines survive taking the agent phrase out of
#             the power classifier,
#         (2) does the within-document contrast differ by regime,
#         (3) does it move with academic freedom within country,
#         (4) does it behave differently from the whole-document level, which
#             is what the earlier country-level measures used,
#         (5) does it fire in fields where the theory says it should not.
#
# Nothing here is a finished measure. It is a probe of whether the design is
# worth building properly.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(countrycode)
library(fixest)

probe_dir     <- "/home/martigso/wos_parsed/hedging_probe"
address_path  <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/data/wosc_address.csv"
jsc_path      <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/data/jsc.csv"
vdem_path     <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                        "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")

MIN_WORDS_PER_SIDE <- 20L   # denominator floor for the hedging contrast
MIN_DOCS_PER_CELL  <- 10L   # country-year cells thinner than this are dropped

# Building the document table takes about seven minutes, so it is cached.
# Delete the cache file to rebuild it.
document_cache <- file.path(probe_dir, "document_measures.rds")
if (file.exists(document_cache)) {
  documents <- readRDS(document_cache)
  cat("Loaded cached document measures:", nrow(documents), "documents\n")
}

# ── Document-level measures ────────────────────────────────────────────────
if (!exists("documents")) {
con <- dbConnect(duckdb())
documents <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(year AS INTEGER) AS year,
         CAST(n_words_total AS INTEGER)      AS n_words_total,
         CAST(n_words_power_kw AS INTEGER)   AS n_words_power_kw,
         CAST(n_hedge_power_kw AS INTEGER)   AS n_hedge_power_kw,
         CAST(n_booster_power_kw AS INTEGER) AS n_booster_power_kw,
         CAST(n_words_other_kw AS INTEGER)   AS n_words_other_kw,
         CAST(n_hedge_other_kw AS INTEGER)   AS n_hedge_other_kw,
         CAST(n_booster_other_kw AS INTEGER) AS n_booster_other_kw,
         CAST(n_words_power_oa AS INTEGER)   AS n_words_power_oa,
         CAST(n_hedge_power_oa AS INTEGER)   AS n_hedge_power_oa,
         CAST(n_words_other_oa AS INTEGER)   AS n_words_other_oa,
         CAST(n_hedge_other_oa AS INTEGER)   AS n_hedge_other_oa,
         CAST(n_passive_power_kw AS INTEGER) AS n_passive_power_kw,
         CAST(n_agent_power_kw AS INTEGER)   AS n_agent_power_kw,
         CAST(n_passive_other_kw AS INTEGER) AS n_passive_other_kw,
         CAST(n_agent_other_kw AS INTEGER)   AS n_agent_other_kw,
         CAST(n_passive_power_oa AS INTEGER) AS n_passive_power_oa,
         CAST(n_agent_power_oa AS INTEGER)   AS n_agent_power_oa,
         CAST(n_passive_other_oa AS INTEGER) AS n_passive_other_oa,
         CAST(n_agent_other_oa AS INTEGER)   AS n_agent_other_oa,
         CAST(n_passive_power_pa AS INTEGER) AS n_passive_power_pa,
         CAST(n_agent_power_pa AS INTEGER)   AS n_agent_power_pa,
         CAST(n_passive_other_pa AS INTEGER) AS n_passive_other_pa,
         CAST(n_agent_other_pa AS INTEGER)   AS n_agent_other_pa
  FROM read_parquet('%s/document_contrast.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

rate <- function(numerator, denominator, floor_value = 1L) {
  ifelse(denominator >= floor_value, numerator / denominator, NA_real_)
}

# agent deletion, by power definition
for (suffix in c("kw", "oa", "pa")) {
  documents[[paste0("deletion_power_", suffix)]] <-
    1 - rate(documents[[paste0("n_agent_power_", suffix)]],
             documents[[paste0("n_passive_power_", suffix)]])
  documents[[paste0("deletion_other_", suffix)]] <-
    1 - rate(documents[[paste0("n_agent_other_", suffix)]],
             documents[[paste0("n_passive_other_", suffix)]])
  documents[[paste0("deletion_contrast_", suffix)]] <-
    documents[[paste0("deletion_power_", suffix)]] -
    documents[[paste0("deletion_other_", suffix)]]
}

# hedging and boosting per 1000 words, by power definition
for (suffix in c("kw", "oa")) {
  documents[[paste0("hedge_power_", suffix)]] <-
    1000 * rate(documents[[paste0("n_hedge_power_", suffix)]],
                documents[[paste0("n_words_power_", suffix)]], MIN_WORDS_PER_SIDE)
  documents[[paste0("hedge_other_", suffix)]] <-
    1000 * rate(documents[[paste0("n_hedge_other_", suffix)]],
                documents[[paste0("n_words_other_", suffix)]], MIN_WORDS_PER_SIDE)
  documents[[paste0("hedge_contrast_", suffix)]] <-
    documents[[paste0("hedge_power_", suffix)]] -
    documents[[paste0("hedge_other_", suffix)]]
}
documents[, booster_contrast_kw :=
    1000 * rate(n_booster_power_kw, n_words_power_kw, MIN_WORDS_PER_SIDE) -
    1000 * rate(n_booster_other_kw, n_words_other_kw, MIN_WORDS_PER_SIDE)]

# whole-document levels, for comparison with the contrasts
documents[, deletion_level := 1 - rate(n_agent_power_kw + n_agent_other_kw,
                                       n_passive_power_kw + n_passive_other_kw)]
documents[, hedge_level := 1000 * rate(n_hedge_power_kw + n_hedge_other_kw,
                                       n_words_total, MIN_WORDS_PER_SIDE)]

# ── Country, year, field ───────────────────────────────────────────────────
address <- fread(address_path, select = c("ut", "addr_no", "country"))
address <- address[addr_no == 1, .(ut, country_name = country)]
address[, country_name := tools::toTitleCase(tolower(country_name))]
address[, country_name := fifelse(country_name == "Usa", "USA",
                          fifelse(country_name == "Peoples r China", "China",
                          fifelse(country_name %in% c("England", "Scotland", "Wales",
                                                      "North Ireland", "United Kingdom"), "UK",
                                  country_name)))]
address[, iso3 := countrycode(country_name, "country.name", "iso3c", warn = FALSE)]

jsc <- fread(jsc_path)
setnames(jsc, c("ut", "field"))

documents <- merge(documents, address[!is.na(iso3), .(ut, iso3)], by = "ut")
documents <- merge(documents, jsc, by = "ut", all.x = TRUE)
documents <- documents[year >= 1991 & year <= 2019]
saveRDS(documents, document_cache)
}

# ── V-Dem ──────────────────────────────────────────────────────────────────
vdem <- as.data.table(readRDS(vdem_path))
vdem <- vdem[, .(iso3 = country_text_id, year, v2x_regime, v2xca_academ,
                 v2x_libdem, e_gdppc, e_pop)]
vdem[, autocracy := as.integer(v2x_regime < 2)]
vdem[, log_gdppc := log(e_gdppc)]
vdem[, log_pop := log(e_pop)]

# ── Country-year panel ─────────────────────────────────────────────────────
build_country_year <- function(dt, measure) {
  cell <- dt[!is.na(get(measure)), .(value = mean(get(measure)), n_docs = .N),
             by = .(iso3, year)]
  cell <- cell[n_docs >= MIN_DOCS_PER_CELL]
  merge(cell, vdem, by = c("iso3", "year"))
}

report <- function(label, dt, measure) {
  panel <- build_country_year(dt, measure)
  if (nrow(panel) < 50) { cat(sprintf("\n%s: too few cells (%d)\n", label, nrow(panel))); return(invisible(NULL)) }
  cross    <- feols(value ~ autocracy | year, panel, weights = ~n_docs, cluster = ~iso3)
  adjusted <- feols(value ~ autocracy + log_gdppc + log_pop | year, panel, weights = ~n_docs, cluster = ~iso3)
  within   <- feols(value ~ v2xca_academ | iso3 + year, panel, weights = ~n_docs, cluster = ~iso3)
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("country-years %d | countries %d | documents %d | mean %.4f (sd %.4f)\n",
              nrow(panel), uniqueN(panel$iso3), sum(panel$n_docs),
              weighted.mean(panel$value, panel$n_docs),
              sqrt(Hmisc_wtd_var(panel$value, panel$n_docs))))
  cat(sprintf("autocracy, year FE            %+.4f (se %.4f, p %.3f)\n",
              coef(cross)["autocracy"], se(cross)["autocracy"], pvalue(cross)["autocracy"]))
  cat(sprintf("autocracy, + log GDP and pop  %+.4f (se %.4f, p %.3f)\n",
              coef(adjusted)["autocracy"], se(adjusted)["autocracy"], pvalue(adjusted)["autocracy"]))
  cat(sprintf("academic freedom, country FE  %+.4f (se %.4f, p %.3f)\n",
              coef(within)["v2xca_academ"], se(within)["v2xca_academ"], pvalue(within)["v2xca_academ"]))
  invisible(panel)
}

Hmc <- function(x, w) sum(w * (x - weighted.mean(x, w))^2) / (sum(w) - 1)
Hmisc_wtd_var <- Hmc

cat("\n############ MAIN CONTRASTS ############\n")
report("Agent deletion contrast, sentence keyword",        documents, "deletion_contrast_kw")
report("Agent deletion contrast, outside agent phrase",    documents, "deletion_contrast_oa")
report("Agent deletion contrast, predicate argument",      documents, "deletion_contrast_pa")
report("Hedging contrast, sentence keyword",               documents, "hedge_contrast_kw")
report("Hedging contrast, outside agent phrase",           documents, "hedge_contrast_oa")
report("Boosting contrast, sentence keyword",              documents, "booster_contrast_kw")

cat("\n############ WHOLE-DOCUMENT LEVELS (what the contrast is supposed to improve on) ############\n")
report("Agent deletion level", documents, "deletion_level")
report("Hedging level",        documents, "hedge_level")

cat("\n############ FIELD PLACEBO ############\n")
political_fields <- c("Political Science", "International Relations", "Public Administration",
                      "Law", "Area Studies", "History", "Sociology")
report("Hedging contrast, political fields",     documents[field %in% political_fields], "hedge_contrast_kw")
report("Hedging contrast, all other fields",     documents[!(field %in% political_fields)], "hedge_contrast_kw")
report("Deletion contrast, political fields",    documents[field %in% political_fields], "deletion_contrast_kw")
report("Deletion contrast, all other fields",    documents[!(field %in% political_fields)], "deletion_contrast_kw")

