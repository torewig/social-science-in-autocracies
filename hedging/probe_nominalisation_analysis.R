# ---------------------------------------------------------------------------
# Nominalisation channel on the widened clause base.
#
# "The ministry's closure of the newspapers" names an actor; "the closures"
# does not. Same protective requirement as agent deletion, no passive needed.
# Contrast is actor deletion on nominalisations in power clauses minus the
# same in the author's other clauses.
#
# Input: ~/wos_parsed/hedging_probe/document_nominalisation.parquet
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)

hedging_dir <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging"
source(file.path(hedging_dir, "probe_panel_helpers.R"))

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
vdem_path <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                    "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")

con <- dbConnect(duckdb())
nominalisation <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut,
         CAST(in_power_clause AS INTEGER)     AS in_power_clause,
         CAST(is_gerund AS INTEGER)           AS is_gerund,
         CAST(is_methods_predicate AS INTEGER) AS is_methods_predicate,
         CAST(n_nominalisations AS INTEGER)   AS n_nominalisations,
         CAST(n_actor_expressed AS INTEGER)   AS n_actor_expressed
  FROM read_parquet('%s/document_nominalisation.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))
country_year <- documents[, .(ut, year, iso3, field)]
rm(documents); invisible(gc())

vdem <- load_vdem(vdem_path)

# Each variant keeps or drops gerunds and methods predicates, so that the
# effect of those choices is visible rather than assumed.
build_contrast <- function(keep_gerunds, keep_methods) {
  subset <- nominalisation[(keep_gerunds | is_gerund == 0) &
                           (keep_methods | is_methods_predicate == 0)]
  side <- subset[, .(n_nominalisations = sum(n_nominalisations),
                     n_actor_expressed = sum(n_actor_expressed)),
                 by = .(ut, in_power_clause)]
  wide <- dcast(side, ut ~ in_power_clause,
                value.var = c("n_nominalisations", "n_actor_expressed"), fill = 0L)
  setnames(wide,
           c("n_nominalisations_0", "n_nominalisations_1",
             "n_actor_expressed_0", "n_actor_expressed_1"),
           c("n_nom_other", "n_nom_power", "n_actor_other", "n_actor_power"))
  wide[, actor_deletion_contrast := fifelse(
    n_nom_power >= 1 & n_nom_other >= 1,
    (1 - n_actor_power / n_nom_power) - (1 - n_actor_other / n_nom_other),
    NA_real_)]
  merge(wide, country_year, by = "ut")
}

cat("\n############ NOMINALISATION: ACTOR DELETION CONTRAST ############\n")
for (keep_gerunds in c(FALSE, TRUE)) {
  for (keep_methods in c(TRUE, FALSE)) {
    label <- sprintf("gerunds %s | methods predicates %s",
                     if (keep_gerunds) "kept" else "excluded",
                     if (keep_methods) "kept" else "excluded")
    report(label, build_contrast(keep_gerunds, keep_methods),
           "actor_deletion_contrast", vdem)
  }
}
