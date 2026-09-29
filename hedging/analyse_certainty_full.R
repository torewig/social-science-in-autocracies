# ---------------------------------------------------------------------------
# Learned certainty model against the Hyland lexicon, full eligible corpus.
#
# pedropei/sentence-level-certainty (Pei & Jurgens, EMNLP 2021): SciBERT
# regression head on 2,167 annotated scientific findings, 1-6 scale.
#
# Sentence length is controlled explicitly. Power sentences run about 40
# characters longer than other sentences, and the model scores longer
# sentences as less certain, which accounts for most of the raw gap. The
# length-adjusted score residualises certainty on log sentence length before
# the within-document contrast is taken.
# ---------------------------------------------------------------------------

library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
hedging_dir <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging"

scores <- fread(file.path(probe_dir, "certainty_scores_full.csv"))
lengths <- fread(file.path(probe_dir, "certainty_sentences_full.csv"),
                 select = c("ut", "sent_id", "n_tokens"))
scores <- merge(scores, lengths, by = c("ut", "sent_id"))
scores <- scores[n_tokens >= 3]

cat(sprintf("sentences %d | documents %d | power sentences %d\n",
            nrow(scores), uniqueN(scores$ut), sum(scores$is_power_sentence)))

cat("\n############ 1. SENTENCE LEVEL ############\n")
cat(sprintf("raw certainty      power %.3f | other %.3f | gap %+.4f\n",
            scores[is_power_sentence == 1, mean(certainty)],
            scores[is_power_sentence == 0, mean(certainty)],
            scores[is_power_sentence == 1, mean(certainty)] -
            scores[is_power_sentence == 0, mean(certainty)]))
cat(sprintf("mean length (tokens) power %.1f | other %.1f\n",
            scores[is_power_sentence == 1, mean(n_tokens)],
            scores[is_power_sentence == 0, mean(n_tokens)]))

scores[, certainty_adjusted := residuals(lm(certainty ~ log(n_tokens), scores))]
cat(sprintf("length-adjusted    power %+.4f | other %+.4f | gap %+.4f\n",
            scores[is_power_sentence == 1, mean(certainty_adjusted)],
            scores[is_power_sentence == 0, mean(certainty_adjusted)],
            scores[is_power_sentence == 1, mean(certainty_adjusted)] -
            scores[is_power_sentence == 0, mean(certainty_adjusted)]))

# ── within-document contrasts ──────────────────────────────────────────────
by_side <- scores[, .(raw = mean(certainty), adjusted = mean(certainty_adjusted)),
                  by = .(ut, is_power_sentence)]
wide <- dcast(by_side, ut ~ is_power_sentence, value.var = c("raw", "adjusted"))
setnames(wide, c("raw_0", "raw_1", "adjusted_0", "adjusted_1"),
         c("raw_other", "raw_power", "adj_other", "adj_power"))
wide <- wide[!is.na(raw_power) & !is.na(raw_other)]
wide[, certainty_contrast := raw_power - raw_other]
wide[, certainty_contrast_adjusted := adj_power - adj_other]

papers <- readRDS(file.path(probe_dir, "mover_stayer_papers.rds"))
panel <- merge(wide, papers[, .(ut, daisng_id, iso3, year, v2xca_academ, v2cafexch,
                                v2cafres, hedge_contrast, away)], by = "ut")
cat(sprintf("\nestimation sample: %d documents | %d authors | %d countries\n",
            nrow(panel), uniqueN(panel$daisng_id), uniqueN(panel$iso3)))
cat(sprintf("mean contrast: raw %+.4f (sd %.4f) | length-adjusted %+.4f (sd %.4f)\n",
            mean(panel$certainty_contrast), sd(panel$certainty_contrast),
            mean(panel$certainty_contrast_adjusted), sd(panel$certainty_contrast_adjusted)))

cat("\n############ 2. AUTHOR + YEAR FE ############\n")
cat("positive = more academic freedom, power sentences relatively MORE certain\n")
cat("so the mechanism predicts a POSITIVE coefficient here\n\n")
outcomes <- c(certainty_raw = "certainty_contrast",
              certainty_length_adjusted = "certainty_contrast_adjusted",
              hyland_hedge_lexicon = "hedge_contrast")
for (outcome_label in names(outcomes)) {
  for (treatment in c("v2xca_academ", "v2cafexch", "v2cafres")) {
    y <- outcomes[[outcome_label]]
    fit <- feols(as.formula(sprintf("%s ~ %s | daisng_id + year", y, treatment)),
                 panel[!is.na(get(y))], cluster = ~iso3)
    b <- coef(fit)[treatment]; s <- se(fit)[treatment]
    star <- if (pvalue(fit)[treatment] < .01) "**" else
            if (pvalue(fit)[treatment] < .05) "*" else
            if (pvalue(fit)[treatment] < .1) "." else " "
    cat(sprintf("%-26s %-14s %+8.5f (se %.5f, p %.3f)%s n=%d\n",
                outcome_label, treatment, b, s, pvalue(fit)[treatment], star, nobs(fit)))
  }
}

cat("\n############ 3. AGREEMENT BETWEEN THE TWO MEASURES ############\n")
both <- panel[!is.na(hedge_contrast)]
cat(sprintf("documents with both: %d\n", nrow(both)))
cat(sprintf("correlation, certainty contrast vs hedge contrast: %+.3f\n",
            cor(both$certainty_contrast, both$hedge_contrast)))
cat(sprintf("correlation, length-adjusted version:              %+.3f\n",
            cor(both$certainty_contrast_adjusted, both$hedge_contrast)))
cat("(a negative correlation is agreement: more hedges means less certainty)\n")
saveRDS(panel, file.path(probe_dir, "certainty_panel_full.rds"))
