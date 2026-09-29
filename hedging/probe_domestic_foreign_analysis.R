# ---------------------------------------------------------------------------
# Domestic versus foreign referents.
#
# Two things at once:
#   1. spatial displacement — does a country's output study itself less under
#      autocracy? ("study Hungary rather than Turkey")
#   2. prediction 2 — is the caution asymmetry concentrated in papers that
#      talk about the author's own country?
#
# A document is domestic when it names the author's own country. The
# names-only variant drops demonym matches, because "Spanish" and "Russian"
# are language names as often as country names in this corpus.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)

hedging_dir <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging"
source(file.path(hedging_dir, "probe_panel_helpers.R"))

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
vdem_path <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                    "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")

con <- dbConnect(duckdb())
country_mentions <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, iso3 AS mentioned_iso3,
         CAST(n_mentions AS INTEGER) AS n_mentions,
         CAST(n_name_mentions AS INTEGER) AS n_name_mentions
  FROM read_parquet('%s/document_country.parquet')", probe_dir)))
nominalisation <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(in_power_clause AS INTEGER) AS in_power_clause,
         CAST(n_nominalisations AS INTEGER) AS n_nominalisations,
         CAST(n_actor_expressed AS INTEGER) AS n_actor_expressed
  FROM read_parquet('%s/document_nominalisation.parquet') WHERE is_gerund = 0",
  probe_dir)))
dbDisconnect(con, shutdown = TRUE)

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))
documents <- documents[, .(ut, year, iso3, field, hedge_contrast_oa, hedge_level)]

# Does the abstract name the author's own country?
own_country <- merge(country_mentions, documents[, .(ut, iso3)], by = "ut")
own_country <- own_country[mentioned_iso3 == iso3,
                           .(names_own_country = 1L,
                             names_own_country_by_name = as.integer(max(n_name_mentions) > 0)),
                           by = ut]
documents <- merge(documents, own_country, by = "ut", all.x = TRUE)
documents[is.na(names_own_country), `:=`(names_own_country = 0L,
                                         names_own_country_by_name = 0L)]
documents[, n_countries_named := 0L]
countries_per_document <- country_mentions[, .(n = uniqueN(mentioned_iso3)), by = ut]
documents[countries_per_document, n_countries_named := i.n, on = "ut"]

vdem <- load_vdem(vdem_path)

cat("\n############ 1. SPATIAL DISPLACEMENT ############\n")
report("Share of papers naming own country",            documents, "names_own_country", vdem)
report("Same, name matches only (no demonyms)",         documents, "names_own_country_by_name", vdem)
report("Number of countries named per paper",           documents, "n_countries_named", vdem)

cat("\n############ 2. CAUTION, SPLIT BY DOMESTIC FOCUS ############\n")
# Sum over the methods-predicate flag first: without this, dcast has more
# than one row per ut and side and silently aggregates by counting rows.
nominalisation <- nominalisation[, .(n_nominalisations = sum(n_nominalisations),
                                     n_actor_expressed = sum(n_actor_expressed)),
                                 by = .(ut, in_power_clause)]
nominalisation_side <- dcast(nominalisation, ut ~ in_power_clause,
                             value.var = c("n_nominalisations", "n_actor_expressed"), fill = 0L)
setnames(nominalisation_side,
         c("n_nominalisations_0", "n_nominalisations_1",
           "n_actor_expressed_0", "n_actor_expressed_1"),
         c("n_nom_other", "n_nom_power", "n_actor_other", "n_actor_power"))
nominalisation_side[, actor_deletion_contrast := fifelse(
  n_nom_power >= 1 & n_nom_other >= 1,
  (1 - n_actor_power / n_nom_power) - (1 - n_actor_other / n_nom_other), NA_real_)]
combined <- merge(documents, nominalisation_side[, .(ut, actor_deletion_contrast)],
                  by = "ut", all.x = TRUE)

for (measure in c("actor_deletion_contrast", "hedge_contrast_oa")) {
  report(sprintf("%s | names own country", measure),
         combined[names_own_country == 1], measure, vdem)
  report(sprintf("%s | does not name own country", measure),
         combined[names_own_country == 0], measure, vdem)
}

saveRDS(combined, file.path(probe_dir, "document_domestic_measures.rds"))
