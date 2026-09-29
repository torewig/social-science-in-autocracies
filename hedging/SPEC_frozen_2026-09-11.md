# Frozen specification: clause-level hedging and academic freedom

Written 2026-09-11, **before** the author fixed-effects estimation was run.
Nothing below was chosen after seeing an author-level result. Everything
already run is exploratory and is recorded separately in
`~/wos_parsed/NOTE_clause_measure_probe_2026-09-11.md`.

## 1. Data

- Corpus: Web of Science SSH abstracts, English, stanza 1.13.0 dependency
  parse in `~/wos_parsed/tokens`.
- Years 1991–2019. Country is the first listed address, mapped to ISO3.
- Author identity is Clarivate `daisng_id` from `data/author.csv`, which holds
  first authors only.

## 2. Measure

**Clause.** Each token is walked up its head chain, at most three hops, to the
nearest clausal head. Clausal heads are `root`, and tokens with deprel
`advcl`, `ccomp`, `xcomp`, `acl`, `acl:relcl`, `parataxis`, or `conj` when the
token is a VERB or AUX. Tokens reaching no clausal head within three hops are
dropped.

**Power clause.** A clause is about power when a lemma from
`power_holder_lemmas.csv` (all 35 terms, NOUN or PROPN) is an argument of that
clause's own head, with deprel in `nsubj`, `nsubj:pass`, `obj`, `iobj`, `obl`.
`obl:agent` is excluded, so naming an agent cannot be what classifies the
clause.

**Hedges.** Hyland (2005) hedges from `hyland_hedges_boosters.csv`, subtypes
`modal`, `epistemic_verb` and `epistemic_adjadv`. Approximators are excluded.
Part of speech is constrained: modals as AUX or VERB, epistemic verbs as VERB,
adjectival and adverbial hedges as ADJ or ADV.

**Document contrast.**

    hedge_contrast = 1000 * hedge_power / words_power
                   - 1000 * hedge_other / words_other

**Inclusion.** A document enters when both sides have at least one word. The
20-words-per-side rule used in the exploratory country-year runs is a
robustness check here, not the primary rule: it was chosen for a country-year
design and it removes the repeat-author documents this design needs.

## 3. Primary hypothesis and estimand

The coefficient `β` on academic freedom in

    hedge_contrast_ijt = β · v2xca_academ_jt + author_i + year_t + ε_ijt

for document `i` by author `j` in country-year `t`, estimated by OLS, standard
errors clustered by country.

**Prediction: β < 0.** Less academic freedom, more hedging of power clauses
relative to the same author's other clauses.

This is the composition test. If the earlier country-year association was
authors changing their writing, β is negative. If it was a change in who
publishes internationally, β is zero while the country-year association
stands.

## 4. Pre-specified secondary tests

1. **Referent asymmetry.** Same model, run separately on documents naming the
   author's own country, documents naming only a foreign country, and
   documents naming no country. Prediction: β negative for own country,
   null for foreign only. No prediction for no country named.
2. **Booster placebo.** Same model with the boosting contrast as outcome.
   Prediction: null.
3. **Movers.** Restricted to authors publishing from two or more countries,
   with author fixed effects. No directional prediction; reported for size.

## 5. Robustness, reported whatever they show

- 20 words per side.
- Excluding methods predicates.
- Strict power lemma list (25 terms) only.
- Adding log GDP per capita and log population.
- Country and year fixed effects instead of author fixed effects, for
  comparison with the exploratory runs.

## 6. Reporting rules

- The effective sample is reported as the number of authors contributing two
  or more documents with within-author variation in academic freedom. If that
  is below 500, the design is reported as underpowered and `β` is not
  interpreted in either direction.
- `β` is one coefficient and is not corrected for multiplicity. The secondary
  tests are described as secondary.
- A null primary result is reported as a null, and the country-year
  association is then described as consistent with composition rather than
  behaviour.
- No variant not listed above is added after seeing these results. Anything
  further is a new exploratory round and is labelled as such.

## 7. What would change the interpretation

| outcome | reading |
|---|---|
| β < 0, booster placebo null, own-country asymmetry holds | behavioural; the strongest available evidence short of a transition design |
| β < 0 but boosters also move | a general style shift, not caution about power |
| β ≈ 0 with the country-year association intact | composition, not behaviour; the regime framing goes |
| effective sample under 500 authors | underpowered; no reading either way |
