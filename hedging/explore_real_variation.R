# ---------------------------------------------------------------------------
# EXPLORATORY. Does the null survive where academic freedom actually moves?
#
# The median author sees a within-author change of 0.023 SD in the academic
# freedom index, and 85% of papers come from countries whose whole-period
# range is under 0.5 SD. Every "well-powered null" so far was identified off
# that. Three corrections:
#
#   A. restrict to authors who actually experience change
#   B. restrict to countries where the index actually moves
#   C. test lags, since the paper's own CSS/LSS result is null
#      contemporaneously and significant at 2-4 year lags
# ---------------------------------------------------------------------------

library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
papers <- readRDS(file.path(probe_dir, "mover_stayer_papers.rds"))
papers <- papers[!is.na(v2xca_academ)]

outcomes <- c(any_power_clause = "has_power_clause",
              power_proximity  = "power_proximity_score",
              hedging_contrast = "hedge_contrast")

report <- function(label, data, y, x = "v2xca_academ", fe = "daisng_id + year") {
  d <- data[!is.na(get(y)) & !is.na(get(x))]
  if (nrow(d) < 500) { cat(sprintf("%-44s too few (%d)\n", label, nrow(d))); return(invisible(NULL)) }
  fit <- feols(as.formula(sprintf("%s ~ %s | %s", y, x, fe)), d, cluster = ~iso3)
  if (!(x %in% names(coef(fit)))) { cat(sprintf("%-44s not estimable\n", label)); return(invisible(NULL)) }
  star <- if (pvalue(fit)[x] < .01) "**" else if (pvalue(fit)[x] < .05) "*" else
          if (pvalue(fit)[x] < .1) "." else " "
  cat(sprintf("%-44s %+9.5f (se %.5f, p %.3f)%s n=%d\n", label,
              coef(fit)[x], se(fit)[x], pvalue(fit)[x], star, nobs(fit)))
}

author_range <- papers[, .(n = .N, rng = max(v2xca_academ) - min(v2xca_academ)), by = daisng_id]
movers_in_freedom <- author_range[n >= 2 & rng >= 0.5, daisng_id]
country_range <- papers[, .(rng = max(v2xca_academ) - min(v2xca_academ), n = .N), by = iso3]
changing_countries <- country_range[rng >= 0.5, iso3]

cat(sprintf("authors experiencing >= 0.5 SD change: %d | countries with >= 0.5 SD range: %d\n\n",
            length(movers_in_freedom), length(changing_countries)))

cat("############ A. AUTHORS WHO ACTUALLY EXPERIENCE CHANGE ############\n")
for (label in names(outcomes)) {
  report(sprintf("%s | all authors", label), papers, outcomes[[label]])
  report(sprintf("%s | change >= 0.5 SD", label),
         papers[daisng_id %in% movers_in_freedom], outcomes[[label]])
}

cat("\n############ B. COUNTRIES WHERE THE INDEX MOVES ############\n")
for (label in names(outcomes)) {
  report(sprintf("%s | country+year FE, changing only", label),
         papers[iso3 %in% changing_countries], outcomes[[label]], fe = "iso3 + year")
}

cat("\n############ C. LAGS ############\n")
freedom <- unique(papers[, .(iso3, year, v2xca_academ)])
freedom <- freedom[, .(v2xca_academ = mean(v2xca_academ)), by = .(iso3, year)]
for (k in 0:5) {
  lagged <- copy(freedom)[, `:=`(year = year + k)]
  setnames(lagged, "v2xca_academ", "freedom_lag")
  d <- merge(papers, lagged, by = c("iso3", "year"))
  for (label in names(outcomes)) {
    report(sprintf("%s | lag %d", label, k), d, outcomes[[label]], x = "freedom_lag")
  }
}
