# ---------------------------------------------------------------------------
# Do authors in autocracies refer to power-holders less specifically?
#
# "the Ministry of the Interior" identifies a party; "the authorities" does
# not. Measured as the share of power mentions that are named, contrasted
# within document against the share of ordinary organisation mentions that are
# named, which holds constant how specifically this author names things.
#
# Input: ~/wos_parsed/hedging_probe/document_specificity.parquet (from
#        probe_reference_specificity.sql) and the cached document table from
#        probe_regime_analysis.R, which carries country, year and field.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)

hedging_dir <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging"
source(file.path(hedging_dir, "probe_panel_helpers.R"))

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
vdem_path <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                    "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")

MIN_POWER_MENTIONS    <- 2L
MIN_BASELINE_MENTIONS <- 1L

con <- dbConnect(duckdb())
specificity <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut,
         CAST(n_power_mentions AS INTEGER)       AS n_power_mentions,
         CAST(n_power_named AS INTEGER)          AS n_power_named,
         CAST(n_power_strict AS INTEGER)         AS n_power_strict,
         CAST(n_power_strict_named AS INTEGER)   AS n_power_strict_named,
         CAST(n_baseline_mentions AS INTEGER)    AS n_baseline_mentions,
         CAST(n_baseline_named AS INTEGER)       AS n_baseline_named
  FROM read_parquet('%s/document_specificity.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))
specificity <- merge(specificity, documents[, .(ut, year, iso3, field)], by = "ut")
rm(documents); invisible(gc())

specificity[, power_named_share := fifelse(n_power_mentions >= MIN_POWER_MENTIONS,
                                           n_power_named / n_power_mentions, NA_real_)]
specificity[, power_strict_named_share := fifelse(n_power_strict >= MIN_POWER_MENTIONS,
                                                  n_power_strict_named / n_power_strict, NA_real_)]
specificity[, baseline_named_share := fifelse(n_baseline_mentions >= MIN_BASELINE_MENTIONS,
                                              n_baseline_named / n_baseline_mentions, NA_real_)]
specificity[, specificity_contrast := power_named_share - baseline_named_share]

vdem <- load_vdem(vdem_path)

cat("\n############ REFERENCE SPECIFICITY ############\n")
report("Power named share, no baseline",        specificity, "power_named_share", vdem)
report("Power named share, strict lemmas only", specificity, "power_strict_named_share", vdem)
report("Baseline organisation named share",     specificity, "baseline_named_share", vdem)
report("Specificity contrast, within document", specificity, "specificity_contrast", vdem)

political_fields <- c("Political Science", "International Relations", "Public Administration",
                      "Law", "Area Studies", "History", "Sociology")
cat("\n############ FIELD SPLIT ############\n")
report("Power named share, political fields",  specificity[field %in% political_fields], "power_named_share", vdem)
report("Power named share, other fields",      specificity[!(field %in% political_fields)], "power_named_share", vdem)

saveRDS(specificity, file.path(probe_dir, "document_specificity_measures.rds"))
