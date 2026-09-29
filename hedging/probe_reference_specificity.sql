-- ---------------------------------------------------------------------------
-- How specifically are power-holders referred to?
--
-- "the Ministry of the Interior" identifies a party. "the authorities" does
-- not. That is the same protective requirement agent deletion defeats, by a
-- different grammatical route, and unlike agent deletion it does not need a
-- passive, so it applies to every mention.
--
-- Three levels of reference, from the parse alone:
--   named    the power noun is a proper noun, or carries a proper-noun
--            modifier ("Ministry of Health", "Health Ministry")
--   modified the power noun carries a capitalised adjective ("the Turkish
--            government")
--   bare     neither ("the government", "the authorities")
--
-- The within-document baseline is the same measure over ordinary organisation
-- nouns (university, company, hospital ...), which holds constant how
-- specifically this author names things in general.
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='14GB';
PRAGMA threads=12;

SET VARIABLE token_glob = '/home/martigso/wos_parsed/tokens/*.parquet';
SET VARIABLE hedging_dir = '/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging';

CREATE OR REPLACE TABLE power_lemma_list AS
SELECT lower(term) AS term, in_strict_list
FROM read_csv(getvariable('hedging_dir') || '/power_holder_lemmas.csv', header = true);

CREATE OR REPLACE TABLE baseline_lemma_list AS
SELECT lower(term) AS term
FROM read_csv(getvariable('hedging_dir') || '/baseline_organization_lemmas.csv', header = true)
WHERE lower(term) NOT IN (SELECT term FROM power_lemma_list);

-- One pass: the mention heads we care about, and every modifier that could
-- make a mention specific.
CREATE OR REPLACE TABLE specificity_input AS
SELECT t.ut, t.sent_id, t.tok_id, t.head, t.upos, t.deprel,
       lower(t.lemma) AS lemma,
       t.text,
       CASE WHEN regexp_matches(t.text, '^[A-Z]') THEN 1 ELSE 0 END AS is_capitalised
FROM read_parquet(getvariable('token_glob')) AS t
WHERE t.field = 'abstract'
  AND (   (t.upos IN ('NOUN', 'PROPN')
           AND (lower(t.lemma) IN (SELECT term FROM power_lemma_list)
             OR lower(t.lemma) IN (SELECT term FROM baseline_lemma_list)))
       OR (t.upos = 'PROPN' AND t.deprel IN ('compound', 'nmod', 'flat', 'appos'))
       OR (t.upos = 'ADJ'   AND t.deprel = 'amod'));

CREATE OR REPLACE TABLE proper_noun_modifier AS
SELECT DISTINCT ut, sent_id, head AS tok_id
FROM specificity_input
WHERE upos = 'PROPN' AND deprel IN ('compound', 'nmod', 'flat', 'appos');

CREATE OR REPLACE TABLE capitalised_adjective_modifier AS
SELECT DISTINCT ut, sent_id, head AS tok_id
FROM specificity_input
WHERE upos = 'ADJ' AND deprel = 'amod' AND is_capitalised = 1;

CREATE OR REPLACE TABLE mention AS
SELECT
    m.ut,
    m.sent_id,
    m.tok_id,
    CASE WHEN p.term IS NOT NULL THEN 'power' ELSE 'baseline' END AS mention_class,
    coalesce(p.in_strict_list, 0) AS in_strict_list,
    CASE
        WHEN m.upos = 'PROPN' OR pn.tok_id IS NOT NULL THEN 'named'
        WHEN ca.tok_id IS NOT NULL                     THEN 'modified'
        ELSE 'bare'
    END AS reference_level
FROM specificity_input AS m
LEFT JOIN power_lemma_list    AS p  ON m.lemma = p.term
LEFT JOIN baseline_lemma_list AS b  ON m.lemma = b.term
LEFT JOIN proper_noun_modifier AS pn
       ON m.ut = pn.ut AND m.sent_id = pn.sent_id AND m.tok_id = pn.tok_id
LEFT JOIN capitalised_adjective_modifier AS ca
       ON m.ut = ca.ut AND m.sent_id = ca.sent_id AND m.tok_id = ca.tok_id
WHERE m.upos IN ('NOUN', 'PROPN')
  AND (p.term IS NOT NULL OR b.term IS NOT NULL);

CREATE OR REPLACE TABLE pooled_specificity AS
SELECT mention_class, reference_level, count(*) AS n,
       100.0 * count(*) / sum(count(*)) OVER (PARTITION BY mention_class) AS pct
FROM mention GROUP BY 1, 2 ORDER BY 1, 2;

CREATE OR REPLACE TABLE document_specificity AS
SELECT
    ut,
    count(*) FILTER (mention_class = 'power')                                AS n_power_mentions,
    count(*) FILTER (mention_class = 'power'    AND reference_level = 'named')    AS n_power_named,
    count(*) FILTER (mention_class = 'power'    AND reference_level = 'bare')     AS n_power_bare,
    count(*) FILTER (mention_class = 'power'    AND in_strict_list = 1)           AS n_power_strict,
    count(*) FILTER (mention_class = 'power'    AND in_strict_list = 1
                     AND reference_level = 'named')                              AS n_power_strict_named,
    count(*) FILTER (mention_class = 'baseline')                             AS n_baseline_mentions,
    count(*) FILTER (mention_class = 'baseline' AND reference_level = 'named')    AS n_baseline_named,
    count(*) FILTER (mention_class = 'baseline' AND reference_level = 'bare')     AS n_baseline_bare
FROM mention GROUP BY ut;

COPY pooled_specificity   TO '/home/martigso/wos_parsed/hedging_probe/pooled_specificity.csv' (HEADER, DELIMITER ',');
COPY document_specificity TO '/home/martigso/wos_parsed/hedging_probe/document_specificity.parquet' (FORMAT PARQUET);

SELECT * FROM pooled_specificity;
