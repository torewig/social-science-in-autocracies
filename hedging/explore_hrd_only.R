# ---------------------------------------------------------------------------
# EXPLORATORY. HRD on its own, with no V-Dem anywhere.
#
# Three tests that need only the Historical Regimes Dataset:
#   A. regime age. The memo claims habituation rather than deliberate choice,
#      which predicts more caution under long-established regimes, not a spike
#      at the moment of change.
#   B. spell boundaries against placebo boundaries. Does hedging move at
#      regime changes more than at ordinary year boundaries in the same
#      countries?
#   C. how the previous regime ended, classified from HRD's own notes field
#      by whether it mentions a coup.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(countrycode)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
set.seed(2026)
WINDOW <- 3L
MIN_DOCS_PER_SIDE <- 10L

con <- dbConnect(duckdb())
hedging <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(in_power_clause AS INTEGER) AS in_power_clause,
         CAST(n_words AS INTEGER) AS n_words, CAST(n_hedge_core AS INTEGER) AS n_hedge_core,
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

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))
wide <- merge(wide, documents[, .(ut, year, iso3)], by = "ut")
rm(documents); invisible(gc())
authors <- fread("/home/martigso/Dropbox/postdoc/liberal_socialscience/data/author.csv",
                 select = c("ut", "daisng_id"))
wide <- merge(wide, authors, by = "ut", all.x = TRUE)

hrd <- fread("/home/martigso/Dropbox/postdoc/un/ga_minutes/patterns/data/HRD2023.csv")
hrd[, iso3 := countrycode(v_dem_code, "vdem", "iso3c", warn = FALSE)]
hrd <- hrd[!is.na(iso3)]

# ── A. Regime age ──────────────────────────────────────────────────────────
age <- unique(hrd[, .(iso3, year, cumulative_duration)])[, .(regime_age = max(cumulative_duration)),
                                                         by = .(iso3, year)]
panel <- merge(wide, age, by = c("iso3", "year"))
panel[, log_regime_age := log1p(regime_age)]
cat("############ A. REGIME AGE ############\n")
cat(sprintf("documents %d | countries %d | median regime age %.0f years\n",
            nrow(panel), uniqueN(panel$iso3), median(panel$regime_age)))
for (spec in list(c("country FE only", "hedge_contrast ~ log_regime_age | iso3"),
                  c("country + year FE", "hedge_contrast ~ log_regime_age | iso3 + year"),
                  c("author + year FE", "hedge_contrast ~ log_regime_age | daisng_id + year"),
                  c("PLACEBO booster, author + year FE",
                    "booster_contrast ~ log_regime_age | daisng_id + year"))) {
  fit <- feols(as.formula(spec[2]), panel[!is.na(daisng_id)], cluster = ~iso3)
  cat(sprintf("  %-34s %+7.4f (se %.4f, p %.3f)\n", spec[1],
              coef(fit)["log_regime_age"], se(fit)["log_regime_age"],
              pvalue(fit)["log_regime_age"]))
}

# ── B. Spell boundaries versus placebo boundaries ──────────────────────────
spells <- unique(hrd[startyear >= 1991 & startyear <= 2019, .(iso3, startyear)])
country_year <- wide[, .(hedge = mean(hedge_contrast), n = .N), by = .(iso3, year)]

boundary_change <- function(country, boundary_year) {
  pre  <- country_year[iso3 == country & year %between% c(boundary_year - WINDOW, boundary_year - 1)]
  post <- country_year[iso3 == country & year %between% c(boundary_year + 1, boundary_year + WINDOW)]
  if (sum(pre$n) < MIN_DOCS_PER_SIDE || sum(post$n) < MIN_DOCS_PER_SIDE) return(NULL)
  data.table(iso3 = country, boundary_year,
             change = weighted.mean(post$hedge, post$n) - weighted.mean(pre$hedge, pre$n),
             n_min = min(sum(pre$n), sum(post$n)))
}

real <- rbindlist(lapply(seq_len(nrow(spells)),
                         function(i) boundary_change(spells$iso3[i], spells$startyear[i])))
spell_years <- spells[, paste(iso3, startyear)]
placebo_grid <- CJ(iso3 = unique(real$iso3), year = 1994:2016)
placebo_grid <- placebo_grid[!paste(iso3, year) %in% spell_years]
placebo <- rbindlist(lapply(seq_len(nrow(placebo_grid)),
                            function(i) boundary_change(placebo_grid$iso3[i], placebo_grid$year[i])))
cat("\n############ B. SPELL BOUNDARIES VS PLACEBO BOUNDARIES ############\n")
cat(sprintf("real regime changes: %d | placebo year boundaries: %d\n", nrow(real), nrow(placebo)))
cat(sprintf("mean absolute change at regime changes:  %.3f\n", mean(abs(real$change))))
cat(sprintf("mean absolute change at placebo years:   %.3f\n", mean(abs(placebo$change))))
cat(sprintf("signed mean at regime changes: %+.3f | at placebo years: %+.3f\n",
            mean(real$change), mean(placebo$change)))
cat(sprintf("two-sample test of absolute change, p = %.3f\n",
            t.test(abs(real$change), abs(placebo$change))$p.value))

# ── C. How the previous regime ended ───────────────────────────────────────
ends <- unique(hrd[!is.na(endyear) & endyear >= 1988,
                   .(iso3, endyear, notes = tolower(paste(notes_to_v3regendtype,
                                                          notes_to_v3regendtypems)))])
ends[, by_coup := as.integer(grepl("coup|military takeover|seizure of power", notes))]
starts <- merge(spells, ends[, .(iso3, startyear = endyear + 1L, by_coup)],
                by = c("iso3", "startyear"))
starts <- starts[, .(by_coup = max(by_coup)), by = .(iso3, startyear)]
real_typed <- merge(real, starts, by.x = c("iso3", "boundary_year"),
                    by.y = c("iso3", "startyear"))
cat("\n############ C. REGIME ARRIVED BY COUP ############\n")
if (nrow(real_typed) >= 6) {
  cat(sprintf("events matched to an end type: %d (%d by coup)\n",
              nrow(real_typed), sum(real_typed$by_coup)))
  print(real_typed[order(-by_coup), .(iso3, boundary_year, by_coup, change = round(change, 2), n_min)])
  cat(sprintf("mean change after a coup %+.3f | otherwise %+.3f\n",
              real_typed[by_coup == 1, mean(change)], real_typed[by_coup == 0, mean(change)]))
} else cat("too few typed events\n")
