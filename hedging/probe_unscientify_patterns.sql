-- ---------------------------------------------------------------------------
-- Method 2: UnScientify-style scientific uncertainty patterns.
--
-- Ningrum, Mayr & Atanassova (EEKE 2023; Journal of Informetrics 2025) reject
-- cue-word lists on the grounds that "relying solely on cues or markers such
-- as hedging words or modal verbs may not accurately identify scientific
-- uncertainty". Their system matches spans against 12 pattern groups using
-- POS, morphology and dependency, then cancels matches that a rebuttal or
-- confirmation in the same sentence overrides, then checks whether the
-- uncertainty belongs to the author or to cited work.
--
-- No lexicon or matcher is released, so the 12 groups are reimplemented here
-- against our own dependency parse, following the published definitions and
-- examples. This is a reimplementation, not their system.
--
-- Groups: explicit, modality, conditional, hypothesis, prediction,
-- interrogative, non-generalisable, adverbial, negation, subjectivity,
-- conjectural, disagreement.
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='10GB';
PRAGMA threads=8;
PRAGMA temp_directory='/home/martigso/wos_parsed/duckdb_temp';

CREATE OR REPLACE TABLE eligible_ut AS
SELECT ut FROM read_csv('/home/martigso/wos_parsed/hedging_probe/certainty_full_uts.csv', header = true);

CREATE OR REPLACE TABLE sentence_token AS
SELECT t.ut, t.sent_id, t.tok_id, t.text, lower(t.lemma) AS lemma, t.upos, t.deprel
FROM read_parquet('/home/martigso/wos_parsed/tokens/*.parquet') AS t
SEMI JOIN eligible_ut AS e ON t.ut = e.ut
WHERE t.field = 'abstract';

CREATE OR REPLACE TABLE sentence_pattern AS
SELECT
    ut, sent_id,
    -- 1. explicit uncertainty keywords
    max(CASE WHEN lemma IN ('unclear','unknown','uncertain','uncertainty','controversial',
         'debatable','ambiguous','inconclusive','contested','unresolved','unsettled',
         'undetermined','unexplained','puzzling','elusive') THEN 1 ELSE 0 END) AS explicit_su,
    -- 2. modality
    max(CASE WHEN upos IN ('AUX','VERB') AND lemma IN ('may','might','could','would','should','can')
         THEN 1 ELSE 0 END) AS modality,
    -- 3. conditional expression
    max(CASE WHEN deprel = 'mark' AND lemma IN ('if','unless','whether','provided','assuming')
         THEN 1 ELSE 0 END) AS conditional,
    -- 4. hypothesis
    max(CASE WHEN lemma IN ('hypothesise','hypothesize','hypothesis','hypotheses','assume',
         'assumption','presume','posit','postulate','conjecture') THEN 1 ELSE 0 END) AS hypothesis,
    -- 5. prediction
    max(CASE WHEN upos = 'VERB' AND lemma IN ('predict','forecast','project','anticipate','expect')
         THEN 1 ELSE 0 END) AS prediction,
    -- 6. interrogative
    max(CASE WHEN text = '?' THEN 1 ELSE 0 END) AS interrogative,
    -- 7. non-generalisable statement
    max(CASE WHEN lemma IN ('generalise','generalize','generalisability','generalizability',
         'preliminary','tentative','exploratory','indicative') THEN 1 ELSE 0 END) AS non_generalisable,
    -- 8. adverbial uncertainty
    max(CASE WHEN upos = 'ADV' AND lemma IN ('possibly','probably','perhaps','apparently',
         'presumably','arguably','seemingly','allegedly','potentially','conceivably','maybe')
         THEN 1 ELSE 0 END) AS adverbial_su,
    -- 9. negation
    max(CASE WHEN deprel = 'advmod' AND lemma IN ('not','never','n''t')
              OR (deprel = 'det' AND lemma = 'no') THEN 1 ELSE 0 END) AS negation,
    -- 10. subjectivity
    max(CASE WHEN upos = 'VERB' AND lemma IN ('believe','feel','think','argue','view','consider','regard')
         THEN 1 ELSE 0 END) AS subjectivity,
    -- 11. conjectural
    max(CASE WHEN lemma IN ('speculate','speculation','suspect','guess','suppose','surmise',
         'conceivable','plausible','likely','unlikely') THEN 1 ELSE 0 END) AS conjectural,
    -- 12. disagreement
    max(CASE WHEN lemma IN ('disagree','disagreement','dispute','contest','contradict',
         'contradictory','conflicting','inconsistent','debate','controversy') THEN 1 ELSE 0 END) AS disagreement,
    -- cancellation: a confirmation cue that overrides the uncertainty
    max(CASE WHEN upos IN ('ADV','ADJ') AND lemma IN ('clearly','definitely','certainly',
         'conclusively','undoubtedly','unambiguously','robustly') THEN 1 ELSE 0 END) AS confirmation_cue,
    -- authorial reference: does the sentence point at prior work rather than the author
    max(CASE WHEN lemma IN ('previous','prior','literature','scholar','researcher','study')
              AND deprel IN ('amod','nmod','nsubj','obl') THEN 1 ELSE 0 END) AS prior_work_reference,
    count(*) AS n_tokens
FROM sentence_token
GROUP BY ut, sent_id;

CREATE OR REPLACE TABLE sentence_uncertainty AS
SELECT s.*,
       (explicit_su + modality + conditional + hypothesis + prediction + interrogative
        + non_generalisable + adverbial_su + negation + subjectivity + conjectural
        + disagreement) AS n_groups_matched,
       CASE WHEN (explicit_su + modality + conditional + hypothesis + prediction + interrogative
        + non_generalisable + adverbial_su + negation + subjectivity + conjectural
        + disagreement) > 0 AND confirmation_cue = 0 THEN 1 ELSE 0 END AS is_su_expression,
       CASE WHEN p.ut IS NOT NULL THEN 1 ELSE 0 END AS is_power_sentence
FROM sentence_pattern AS s
LEFT JOIN (SELECT DISTINCT ut, sent_id FROM
           read_parquet('/home/martigso/wos_parsed/hedging_probe/power_sentence_flag.parquet')) AS p
       ON s.ut = p.ut AND s.sent_id = p.sent_id;

COPY sentence_uncertainty TO '/home/martigso/wos_parsed/hedging_probe/unscientify_sentences.parquet' (FORMAT PARQUET);

SELECT is_power_sentence,
       count(*) AS sentences,
       round(100.0 * avg(is_su_expression), 2) AS pct_uncertain,
       round(avg(n_groups_matched), 3) AS mean_groups,
       round(100.0 * avg(prior_work_reference), 2) AS pct_prior_work
FROM sentence_uncertainty GROUP BY 1 ORDER BY 1;
