# ---------------------------------------------------------------------------
# EXPLORATORY. Verbs: action vocabulary, and which grammatical slot
# power-holders are put in.
#
# Two outcomes the noun work cannot reach:
#   action_proximity  count-weighted verb score, regime-blind
#   agency_share      share of power mentions in the active subject slot,
#                     the slot that attributes an action to the power-holder
#
# Same three designs as before: author fixed effects, stayers versus movers,
# and movers before versus after the move.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
papers <- readRDS(file.path(probe_dir, "mover_stayer_papers.rds"))

con <- dbConnect(duckdb())
verb_score <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, action_proximity_score, CAST(n_verb_tokens AS INTEGER) AS n_verb_tokens
  FROM read_parquet('%s/document_verb_score.parquet')", probe_dir)))
power_slot <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(n_power_arguments AS INTEGER) AS n_power_arguments,
         CAST(n_active_subject AS INTEGER) AS n_active_subject,
         CAST(n_passive_subject AS INTEGER) AS n_passive_subject,
         CAST(n_object AS INTEGER) AS n_object,
         CAST(n_oblique AS INTEGER) AS n_oblique
  FROM read_parquet('%s/document_power_slot.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

papers <- merge(papers, verb_score, by = "ut", all.x = TRUE)
papers <- merge(papers, power_slot, by = "ut", all.x = TRUE)
papers[, agency_share := fifelse(n_power_arguments >= 1,
                                 n_active_subject / n_power_arguments, NA_real_)]
papers[, object_share := fifelse(n_power_arguments >= 1,
                                 n_object / n_power_arguments, NA_real_)]

cat(sprintf("papers %d | with a power argument %d | SD action proximity %.4f | mean agency share %.3f\n",
            nrow(papers), sum(!is.na(papers$agency_share)),
            sd(papers$action_proximity_score, na.rm = TRUE),
            mean(papers$agency_share, na.rm = TRUE)))

outcomes <- c(action_proximity = "action_proximity_score",
              agency_share     = "agency_share",
              object_share     = "object_share")
treatments <- c(academic_freedom = "v2xca_academ",
                academic_exchange = "v2cafexch",
                academic_research = "v2cafres")

cat("\n############ AUTHOR + YEAR FE ############\n")
for (outcome_label in names(outcomes)) {
  for (treatment_label in names(treatments)) {
    y <- outcomes[[outcome_label]]; x <- treatments[[treatment_label]]
    fit <- feols(as.formula(sprintf("%s ~ %s | daisng_id + year", y, x)),
                 papers[!is.na(get(y)) & !is.na(get(x))], cluster = ~iso3)
    b <- coef(fit)[x]; s <- se(fit)[x]
    cat(sprintf("%-18s %-18s %+8.5f (se %.5f, p %.3f) n=%d\n",
                outcome_label, treatment_label, b, s, pvalue(fit)[x], nobs(fit)))
  }
}

cat("\n############ STAYERS VS MOVERS ############\n")
for (outcome_label in names(outcomes)) {
  y <- outcomes[[outcome_label]]
  fit <- feols(as.formula(sprintf("%s ~ origin_freedom * away | daisng_id + year", y)),
               papers[!is.na(get(y)) & !is.na(origin_freedom)], cluster = ~origin_iso3)
  terms <- c("origin_freedom", "origin_freedom:away")
  if (!all(terms %in% names(coef(fit)))) { cat(sprintf("%-18s not estimable\n", outcome_label)); next }
  cat(sprintf("%-18s stayers %+8.5f (p %.3f) | x away %+8.5f (p %.3f) n=%d\n", outcome_label,
              coef(fit)[terms[1]], pvalue(fit)[terms[1]],
              coef(fit)[terms[2]], pvalue(fit)[terms[2]], nobs(fit)))
}

cat("\n############ MOVERS, BEFORE VS AFTER THE MOVE ############\n")
movers <- papers[daisng_id %in% papers[, .(both = uniqueN(away)), by = daisng_id][both == 2, daisng_id]]
for (outcome_label in names(outcomes)) {
  y <- outcomes[[outcome_label]]
  fit <- feols(as.formula(sprintf("%s ~ away | daisng_id + year", y)),
               movers[!is.na(get(y))], cluster = ~origin_iso3)
  cat(sprintf("%-18s away %+8.5f (se %.5f, p %.3f) n=%d\n", outcome_label,
              coef(fit)["away"], se(fit)["away"], pvalue(fit)["away"], nobs(fit)))
}
