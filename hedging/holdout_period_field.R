# ---------------------------------------------------------------------------
# Held-out replication by period and by field.
#
# Nothing in this corpus is untouched, so the honest substitute is splitting:
# if the effect is real it should appear in both halves of a split, at
# similar size. If it lives in one period or one field it is not a general
# phenomenon.
# ---------------------------------------------------------------------------

library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
set.seed(2026)

panel <- readRDS(file.path(probe_dir, "certainty_composite_panel.rds"))
topics <- readRDS(file.path(probe_dir, "topic_avoidance_papers.rds"))
panel <- merge(panel, topics[, .(ut, field)], by = "ut", all.x = TRUE)

run <- function(label, data) {
  data <- data[!is.na(certainty_contrast)]
  if (nrow(data) < 300 || uniqueN(data$iso3) < 10) {
    cat(sprintf("%-40s too small (n=%d)\n", label, nrow(data))); return(invisible(NULL)) }
  fit <- try(feols(certainty_contrast ~ v2xca_academ | daisng_id + year, data, cluster = ~iso3),
             silent = TRUE)
  if (inherits(fit, "try-error") || !("v2xca_academ" %in% names(coef(fit)))) {
    cat(sprintf("%-40s not estimable\n", label)); return(invisible(NULL)) }
  b <- coef(fit)["v2xca_academ"]
  star <- if (pvalue(fit)["v2xca_academ"] < .01) "**" else
          if (pvalue(fit)["v2xca_academ"] < .05) "*" else
          if (pvalue(fit)["v2xca_academ"] < .1) "." else " "
  cat(sprintf("%-40s %+8.5f (se %.5f, p %.3f)%s  %+6.3f SD  n=%d\n", label, b,
              se(fit)["v2xca_academ"], pvalue(fit)["v2xca_academ"], star,
              b / sd(data$certainty_contrast), nobs(fit)))
  invisible(b)
}

cat("############ FULL SAMPLE (reference) ############\n")
run("all", panel)

cat("\n############ PERIOD SPLIT ############\n")
cat(sprintf("documents before 2010: %d | 2010 and later: %d\n",
            panel[year < 2010, .N], panel[year >= 2010, .N]))
run("1991-2009", panel[year < 2010])
run("2010-2019", panel[year >= 2010])
run("1991-2004", panel[year <= 2004])
run("2005-2012", panel[year >= 2005 & year <= 2012])
run("2013-2019", panel[year >= 2013])

cat("\n############ FIELD SPLIT ############\n")
fields <- panel[!is.na(field), unique(field)]
half_a <- sample(fields, length(fields) %/% 2)
cat(sprintf("fields: %d | split %d vs %d\n", length(fields), length(half_a), length(fields) - length(half_a)))
run("random field half A", panel[field %in% half_a])
run("random field half B", panel[!is.na(field) & !(field %in% half_a)])

cat("\nlargest fields, one at a time\n")
top_fields <- panel[!is.na(field), .N, by = field][order(-N)][1:8]
for (f in top_fields$field) run(sprintf("  %s", substr(f, 1, 34)), panel[field == f])

cat("\nleave-one-field-out, largest fields\n")
for (f in top_fields$field[1:5]) run(sprintf("  without %s", substr(f, 1, 26)), panel[field != f | is.na(field)])
