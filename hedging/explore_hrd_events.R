# ---------------------------------------------------------------------------
# EXPLORATORY, not frozen. Regime-spell events from the Historical Regimes
# Dataset, which catches tightenings inside autocracies that the binary
# regime dummy misses (China 2018, Turkey 2017, Thailand's coups).
#
# Pre window is the three years before the spell starts, post is the three
# years after. The start year itself is dropped, since publication lag makes
# it ambiguous.
#
# Direction comes from the change in V-Dem academic freedom across the event,
# so the test is whether hedging moves against academic freedom event by
# event, rather than whether autocracies differ from democracies.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(countrycode)
library(fixest)

hedging_dir <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging"
source(file.path(hedging_dir, "probe_panel_helpers.R"))
probe_dir <- "/home/martigso/wos_parsed/hedging_probe"

WINDOW <- 3L
MIN_DOCS_PER_SIDE <- 10L

con <- dbConnect(duckdb())
hedging <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(in_power_clause AS INTEGER) AS in_power_clause,
         CAST(n_words AS INTEGER) AS n_words,
         CAST(n_hedge_core AS INTEGER) AS n_hedge_core,
         CAST(n_booster_core AS INTEGER) AS n_booster_core
  FROM read_parquet('%s/document_clause_hedging.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

wide <- dcast(hedging, ut ~ in_power_clause,
              value.var = c("n_words", "n_hedge_core", "n_booster_core"), fill = 0L)
setnames(wide, c("n_words_0","n_words_1","n_hedge_core_0","n_hedge_core_1",
                 "n_booster_core_0","n_booster_core_1"),
         c("w_other","w_power","h_other","h_power","b_other","b_power"))
wide <- wide[w_power >= 1 & w_other >= 1]
wide[, hedge_contrast := 1000 * h_power / w_power - 1000 * h_other / w_other]
wide[, booster_contrast := 1000 * b_power / w_power - 1000 * b_other / w_other]
wide[, wide_sides := w_power >= 20 & w_other >= 20]

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))
wide <- merge(wide, documents[, .(ut, year, iso3)], by = "ut")
rm(documents); invisible(gc())
authors <- fread("/home/martigso/Dropbox/postdoc/liberal_socialscience/data/author.csv",
                 select = c("ut", "daisng_id"))
wide <- merge(wide, authors, by = "ut", all.x = TRUE)

hrd <- fread("/home/martigso/Dropbox/postdoc/un/ga_minutes/patterns/data/HRD2023.csv")
events <- unique(hrd[startyear >= 1991 & startyear <= 2019,
                     .(country_name, v_dem_code, regime_name_, startyear)])
events[, iso3 := countrycode(v_dem_code, "vdem", "iso3c", warn = FALSE)]
events <- events[!is.na(iso3)]

vdem <- load_vdem(paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                         "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds"))

summarise_event <- function(event_row, use_wide_sides) {
  window_docs <- wide[iso3 == event_row$iso3 &
                      year >= event_row$startyear - WINDOW &
                      year <= event_row$startyear + WINDOW &
                      year != event_row$startyear]
  if (use_wide_sides) window_docs <- window_docs[wide_sides == TRUE]
  window_docs[, post := as.integer(year > event_row$startyear)]
  if (window_docs[post == 0, .N] < MIN_DOCS_PER_SIDE ||
      window_docs[post == 1, .N] < MIN_DOCS_PER_SIDE) return(NULL)
  freedom <- vdem[iso3 == event_row$iso3 &
                  year >= event_row$startyear - WINDOW &
                  year <= event_row$startyear + WINDOW]
  data.table(
    iso3 = event_row$iso3, regime = event_row$regime_name_, startyear = event_row$startyear,
    n_pre = window_docs[post == 0, .N], n_post = window_docs[post == 1, .N],
    hedge_pre  = window_docs[post == 0, mean(hedge_contrast)],
    hedge_post = window_docs[post == 1, mean(hedge_contrast)],
    booster_change = window_docs[post == 1, mean(booster_contrast)] -
                     window_docs[post == 0, mean(booster_contrast)],
    freedom_change = freedom[year > event_row$startyear, mean(v2xca_academ, na.rm = TRUE)] -
                     freedom[year < event_row$startyear, mean(v2xca_academ, na.rm = TRUE)])
}

for (use_wide_sides in c(FALSE, TRUE)) {
  cat(sprintf("\n############ INCLUSION: %s ############\n",
              if (use_wide_sides) "20 words per side" else "1 word per side"))
  results <- rbindlist(lapply(seq_len(nrow(events)),
                              function(i) summarise_event(events[i], use_wide_sides)))
  if (nrow(results) == 0) { cat("no events meet the document minimum\n"); next }
  results[, hedge_change := hedge_post - hedge_pre]
  results <- results[order(freedom_change)]
  print(results[, .(iso3, startyear, n_pre, n_post,
                    freedom_change = round(freedom_change, 3),
                    hedge_change = round(hedge_change, 2),
                    booster_change = round(booster_change, 2))])
  cat(sprintf("\nevents: %d | correlation(freedom change, hedging change): %.3f\n",
              nrow(results), cor(results$freedom_change, results$hedge_change)))
  if (nrow(results) >= 5) {
    fit <- feols(hedge_change ~ freedom_change, results,
                 weights = pmin(results$n_pre, results$n_post))
    cat(sprintf("weighted slope %+.3f (se %.3f, p %.3f)\n",
                coef(fit)[2], se(fit)[2], pvalue(fit)[2]))
    placebo <- feols(booster_change ~ freedom_change, results,
                     weights = pmin(results$n_pre, results$n_post))
    cat(sprintf("booster placebo slope %+.3f (se %.3f, p %.3f)\n",
                coef(placebo)[2], se(placebo)[2], pvalue(placebo)[2]))
  }
}

cat("\n############ AUTHOR FIXED EFFECTS INSIDE THE BIG CASES ############\n")
for (case in list(list("CHN", 2018), list("TUR", 2017), list("MYS", 2018))) {
  case_docs <- wide[iso3 == case[[1]] & !is.na(daisng_id) &
                    year >= case[[2]] - WINDOW & year <= case[[2]] + WINDOW &
                    year != case[[2]]]
  case_docs[, post := as.integer(year > case[[2]])]
  repeat_authors <- case_docs[, .(n = .N, sides = uniqueN(post)), by = daisng_id][n >= 2 & sides == 2]
  cat(sprintf("\n%s %d: %d documents, %d authors spanning the event\n",
              case[[1]], case[[2]], nrow(case_docs), nrow(repeat_authors)))
  if (nrow(repeat_authors) >= 20) {
    fit <- feols(hedge_contrast ~ post | daisng_id,
                 case_docs[daisng_id %in% repeat_authors$daisng_id], cluster = ~daisng_id)
    cat(sprintf("  same-author post effect %+.3f (se %.3f, p %.3f, n %d)\n",
                coef(fit)["post"], se(fit)["post"], pvalue(fit)["post"], nobs(fit)))
  } else cat("  too few authors span the event\n")
}
