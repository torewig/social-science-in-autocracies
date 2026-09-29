# Shared country-year panel helpers for the probe scripts.
library(data.table)
library(fixest)

MIN_DOCS_PER_CELL <- 10L

load_vdem <- function(vdem_path) {
  vdem <- as.data.table(readRDS(vdem_path))
  vdem <- vdem[, .(iso3 = country_text_id, year, v2x_regime, v2xca_academ,
                   v2x_libdem, e_gdppc, e_pop)]
  vdem[, autocracy := as.integer(v2x_regime < 2)]
  vdem[, log_gdppc := log(e_gdppc)]
  vdem[, log_pop := log(e_pop)]
  vdem[]
}

build_country_year <- function(dt, measure, vdem) {
  cell <- dt[!is.na(get(measure)), .(value = mean(get(measure)), n_docs = .N),
             by = .(iso3, year)]
  merge(cell[n_docs >= MIN_DOCS_PER_CELL], vdem, by = c("iso3", "year"))
}

weighted_sd <- function(x, w) {
  sqrt(sum(w * (x - weighted.mean(x, w))^2) / (sum(w) - 1))
}

report <- function(label, dt, measure, vdem) {
  panel <- build_country_year(dt, measure, vdem)
  if (nrow(panel) < 50) {
    cat(sprintf("\n%s: too few cells (%d)\n", label, nrow(panel)))
    return(invisible(NULL))
  }
  cross    <- feols(value ~ autocracy | year, panel, weights = ~n_docs, cluster = ~iso3)
  adjusted <- feols(value ~ autocracy + log_gdppc + log_pop | year, panel,
                    weights = ~n_docs, cluster = ~iso3)
  within   <- feols(value ~ v2xca_academ | iso3 + year, panel,
                    weights = ~n_docs, cluster = ~iso3)
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("country-years %d | countries %d | documents %d | mean %.4f (sd %.4f)\n",
              nrow(panel), uniqueN(panel$iso3), sum(panel$n_docs),
              weighted.mean(panel$value, panel$n_docs),
              weighted_sd(panel$value, panel$n_docs)))
  for (model in list(list("autocracy, year FE", cross, "autocracy"),
                     list("autocracy, + log GDP and pop", adjusted, "autocracy"),
                     list("academic freedom, country FE", within, "v2xca_academ"))) {
    fit <- model[[2]]; term <- model[[3]]
    cat(sprintf("%-30s %+.4f (se %.4f, p %.3f)\n", model[[1]],
                coef(fit)[term], se(fit)[term], pvalue(fit)[term]))
  }
  invisible(panel)
}
