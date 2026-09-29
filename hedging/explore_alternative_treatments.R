# ---------------------------------------------------------------------------
# EXPLORATORY. Is the pattern specific to the academic freedom index, or does
# it hold across V-Dem measures closer to the mechanism?
#
# The memo's mechanism is legal exposure for asserting facts about
# identifiable power-holders. Academic freedom is an expert index over five
# components. Media self-censorship, freedom of discussion and censorship
# effort sit closer to the act being theorised.
#
# Each measure is standardised so coefficients are comparable: a one standard
# deviation fall in the measure, in hedging-contrast units.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
vdem_path <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                    "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")

treatments <- c(
  academic_freedom_index   = "v2xca_academ",
  academic_cultural_expr   = "v2clacfree",
  media_self_censorship    = "v2meslfcen",
  freedom_of_discussion_m  = "v2cldiscm",
  freedom_of_expression    = "v2x_freexp_altinf",
  cso_repression           = "v2csreprss",
  media_censorship_effort  = "v2mecenefm",
  harassment_of_journalists = "v2meharjrn",
  academic_exchange        = "v2cafexch",
  academic_research        = "v2cafres",
  campus_integrity         = "v2casurv",
  liberal_democracy        = "v2x_libdem",
  electoral_democracy      = "v2x_polyarchy",
  physical_integrity       = "v2clrspct")

vdem <- as.data.table(readRDS(vdem_path))
vdem <- vdem[, c(.(iso3 = country_text_id, year = year), .SD),
             .SDcols = c(unname(treatments), "e_gdppc", "e_pop")]
vdem[, log_gdppc := log(e_gdppc)][, log_pop := log(e_pop)]
for (v in unname(treatments)) set(vdem, j = v, value = scale(vdem[[v]])[, 1])

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

documents <- readRDS(file.path(probe_dir, "document_measures.rds"))
wide <- merge(wide, documents[, .(ut, year, iso3)], by = "ut")
rm(documents); invisible(gc())
authors <- fread("/home/martigso/Dropbox/postdoc/liberal_socialscience/data/author.csv",
                 select = c("ut", "daisng_id"))
wide <- merge(wide, authors, by = "ut", all.x = TRUE)
panel <- merge(wide, vdem, by = c("iso3", "year"))

country_year_cell <- panel[, .(hedge = mean(hedge_contrast), n_docs = .N),
                           by = .(iso3, year)][n_docs >= 10]
country_year_cell <- merge(country_year_cell, vdem, by = c("iso3", "year"))

cat(sprintf("%-26s %18s %18s %16s\n", "treatment", "author FE", "country+year FE", "placebo"))
cat(strrep("-", 82), "\n")
for (label in names(treatments)) {
  v <- treatments[[label]]
  author_fit <- feols(as.formula(sprintf("hedge_contrast ~ %s | daisng_id + year", v)),
                      panel[!is.na(get(v)) & !is.na(daisng_id)], cluster = ~iso3)
  cell_fit <- feols(as.formula(sprintf("hedge ~ %s | iso3 + year", v)),
                    country_year_cell[!is.na(get(v))], weights = ~n_docs, cluster = ~iso3)
  placebo_fit <- feols(as.formula(sprintf("booster_contrast ~ %s | daisng_id + year", v)),
                       panel[!is.na(get(v)) & !is.na(daisng_id)], cluster = ~iso3)
  star <- function(p) if (p < .01) "**" else if (p < .05) "*" else if (p < .1) "." else " "
  cat(sprintf("%-26s %+7.3f (p %.3f)%s %+7.3f (p %.3f)%s %+6.3f (p %.3f)%s\n",
              label,
              coef(author_fit)[v], pvalue(author_fit)[v], star(pvalue(author_fit)[v]),
              coef(cell_fit)[v], pvalue(cell_fit)[v], star(pvalue(cell_fit)[v]),
              coef(placebo_fit)[v], pvalue(placebo_fit)[v], star(pvalue(placebo_fit)[v])))
}
cat("\nCoefficients are per one standard deviation of the treatment.\n")
cat("Negative means: less freedom / more repression goes with MORE hedging of power clauses.\n")
