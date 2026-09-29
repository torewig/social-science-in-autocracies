# ---------------------------------------------------------------------------
# Method 2 analysis: UnScientify-style uncertainty patterns.
#
# Outcome is the within-document contrast in the share of sentences flagged as
# scientific-uncertainty expressions: power sentences minus the same author's
# other sentences. Length-adjusted version residualises the sentence flag on
# log token count first, since power sentences are longer and longer sentences
# match more patterns mechanically.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"

con <- dbConnect(duckdb())
sentences <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(is_power_sentence AS INTEGER) AS is_power_sentence,
         CAST(is_su_expression AS INTEGER) AS is_su_expression,
         CAST(n_groups_matched AS INTEGER) AS n_groups_matched,
         CAST(prior_work_reference AS INTEGER) AS prior_work_reference,
         CAST(n_tokens AS INTEGER) AS n_tokens
  FROM read_parquet('%s/unscientify_sentences.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)

sentences <- sentences[n_tokens >= 3]
sentences[, su_adjusted := residuals(lm(is_su_expression ~ log(n_tokens), sentences))]

cat("############ SENTENCE LEVEL ############\n")
cat(sprintf("flagged uncertain: power %.2f%% | other %.2f%%\n",
            100 * sentences[is_power_sentence == 1, mean(is_su_expression)],
            100 * sentences[is_power_sentence == 0, mean(is_su_expression)]))
cat(sprintf("mean tokens:       power %.1f | other %.1f\n",
            sentences[is_power_sentence == 1, mean(n_tokens)],
            sentences[is_power_sentence == 0, mean(n_tokens)]))
cat(sprintf("length-adjusted:   power %+.4f | other %+.4f | gap %+.4f\n",
            sentences[is_power_sentence == 1, mean(su_adjusted)],
            sentences[is_power_sentence == 0, mean(su_adjusted)],
            sentences[is_power_sentence == 1, mean(su_adjusted)] -
            sentences[is_power_sentence == 0, mean(su_adjusted)]))
cat(sprintf("attributed to prior work: power %.2f%% | other %.2f%%\n",
            100 * sentences[is_power_sentence == 1, mean(prior_work_reference)],
            100 * sentences[is_power_sentence == 0, mean(prior_work_reference)]))

by_side <- sentences[, .(su = mean(is_su_expression), su_adj = mean(su_adjusted)),
                     by = .(ut, is_power_sentence)]
wide <- dcast(by_side, ut ~ is_power_sentence, value.var = c("su", "su_adj"))
setnames(wide, c("su_0", "su_1", "su_adj_0", "su_adj_1"),
         c("su_other", "su_power", "adj_other", "adj_power"))
wide <- wide[!is.na(su_power) & !is.na(su_other)]
wide[, su_contrast := su_power - su_other]
wide[, su_contrast_adjusted := adj_power - adj_other]

papers <- readRDS(file.path(probe_dir, "mover_stayer_papers.rds"))
panel <- merge(wide, papers[, .(ut, daisng_id, iso3, year, v2xca_academ, v2cafexch,
                                v2cafres, hedge_contrast)], by = "ut")
cat(sprintf("\nestimation sample: %d documents | %d authors | %d countries\n",
            nrow(panel), uniqueN(panel$daisng_id), uniqueN(panel$iso3)))
cat(sprintf("mean contrast: raw %+.4f (sd %.4f) | length-adjusted %+.4f (sd %.4f)\n",
            mean(panel$su_contrast), sd(panel$su_contrast),
            mean(panel$su_contrast_adjusted), sd(panel$su_contrast_adjusted)))

cat("\n############ AUTHOR + YEAR FE ############\n")
cat("mechanism predicts NEGATIVE: less freedom, more uncertainty on power sentences\n\n")
for (y in c("su_contrast", "su_contrast_adjusted")) {
  for (treatment in c("v2xca_academ", "v2cafexch", "v2cafres")) {
    fit <- feols(as.formula(sprintf("%s ~ %s | daisng_id + year", y, treatment)),
                 panel, cluster = ~iso3)
    star <- if (pvalue(fit)[treatment] < .01) "**" else
            if (pvalue(fit)[treatment] < .05) "*" else
            if (pvalue(fit)[treatment] < .1) "." else " "
    cat(sprintf("%-22s %-14s %+8.5f (se %.5f, p %.3f)%s n=%d\n", y, treatment,
                coef(fit)[treatment], se(fit)[treatment], pvalue(fit)[treatment], star, nobs(fit)))
  }
}

cat("\n############ AGREEMENT WITH THE HYLAND LEXICON ############\n")
both <- panel[!is.na(hedge_contrast)]
cat(sprintf("documents with both: %d | correlation %+.3f\n",
            nrow(both), cor(both$su_contrast, both$hedge_contrast)))
saveRDS(panel, file.path(probe_dir, "unscientify_panel.rds"))
