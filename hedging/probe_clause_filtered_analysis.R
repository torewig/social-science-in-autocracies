# ---------------------------------------------------------------------------
# Step 1 of the revised order: does the agent-deletion contrast change once
# (a) the power entity must be an argument of the predicate and (b) passives
# whose implicit agent is the researcher are removed?
#
# Every combination of power definition and methods filter is reported. None
# is treated as the headline; the point is to see which choices move the
# result and by how much.
#
# Input: ~/wos_parsed/hedging_probe/document_clause_counts.parquet
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)

hedging_dir <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging"
source(file.path(hedging_dir, "probe_panel_helpers.R"))

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
vdem_path <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                    "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")

con <- dbConnect(duckdb())
counts <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, definition, CAST(is_power_side AS INTEGER) AS is_power_side,
         CAST(exclude_methods_predicates AS INTEGER) AS exclude_methods_predicates,
         CAST(n_passive AS INTEGER) AS n_passive,
         CAST(n_agent_expressed AS INTEGER) AS n_agent_expressed
  FROM read_parquet('%s/document_clause_counts.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

wide <- dcast(counts, ut + definition + exclude_methods_predicates ~ is_power_side,
              value.var = c("n_passive", "n_agent_expressed"), fill = 0L)
setnames(wide,
         c("n_passive_0", "n_passive_1", "n_agent_expressed_0", "n_agent_expressed_1"),
         c("n_passive_other", "n_passive_power", "n_agent_other", "n_agent_power"))

wide[, deletion_contrast := fifelse(
  n_passive_power >= 1 & n_passive_other >= 1,
  (1 - n_agent_power / n_passive_power) - (1 - n_agent_other / n_passive_other),
  NA_real_)]

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))
wide <- merge(wide, documents[, .(ut, year, iso3, field)], by = "ut")
rm(documents); invisible(gc())

vdem <- load_vdem(vdem_path)

cat("\n############ AGENT DELETION CONTRAST, ALL VARIANTS ############\n")
for (definition_name in c("sentence_keyword", "sentence_outside_agent_phrase", "predicate_argument")) {
  for (filter_methods in c(0L, 1L)) {
    label <- sprintf("%s | methods predicates %s", definition_name,
                     if (filter_methods == 1L) "excluded" else "kept")
    report(label, wide[definition == definition_name &
                       exclude_methods_predicates == filter_methods],
           "deletion_contrast", vdem)
  }
}

saveRDS(wide, file.path(probe_dir, "document_clause_contrasts.rds"))
