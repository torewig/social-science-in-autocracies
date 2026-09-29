-- ---------------------------------------------------------------------------
-- Widening the clause base.
--
-- The agent-deletion measure only sees passive clauses, which left 13,841
-- usable documents at the tight power definition. Two additions:
--
--   1. a clause counts as being about power when a power lemma is an argument
--      (nsubj, nsubj:pass, obj, iobj, obl) of ANY predicate, not only a
--      passive one. The agent phrase is excluded, as before, so that naming
--      the agent cannot be what classifies the clause.
--
--   2. nominalisation, the third protective move in the memo: "closures
--      increased" has no agent slot at all. A deverbal noun counts as
--      agentful when it carries an actor modifier — a possessive ("the
--      ministry's closure"), a nominal compound ("government closure") or a
--      by-phrase. Those modifiers attach to the noun, not to the predicate,
--      so they cannot double as the clause classifier.
--
-- Two passes: find the documents containing at least one power clause, then
-- extract nominalisations only for those documents, since the within-document
-- contrast never uses the others.
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='14GB';
PRAGMA threads=12;

SET VARIABLE token_glob = '/home/martigso/wos_parsed/tokens/*.parquet';
SET VARIABLE hedging_dir = '/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging';

CREATE OR REPLACE TABLE power_lemma_w AS
SELECT lower(term) AS term, in_strict_list
FROM read_csv(getvariable('hedging_dir') || '/power_holder_lemmas.csv', header = true);

CREATE OR REPLACE TABLE methods_predicate_w AS
SELECT DISTINCT lower(term) AS term
FROM read_csv(getvariable('hedging_dir') || '/methods_predicate_lemmas.csv', header = true);

-- Argument relations that make an entity a participant in the clause.
-- obl:agent is deliberately absent: it is the outcome, not the classifier.
CREATE OR REPLACE TABLE argument_relation AS
SELECT unnest(['nsubj', 'nsubj:pass', 'obj', 'iobj', 'obl']) AS deprel;

-- ── Pass 1: power arguments and the predicates they attach to ──────────────
CREATE OR REPLACE TABLE power_argument AS
SELECT t.ut, t.sent_id, t.head AS predicate_id,
       max(p.in_strict_list) AS in_strict_list
FROM read_parquet(getvariable('token_glob')) AS t
JOIN power_lemma_w AS p ON lower(t.lemma) = p.term
WHERE t.field = 'abstract'
  AND t.upos IN ('NOUN', 'PROPN')
  AND t.deprel IN (SELECT deprel FROM argument_relation)
GROUP BY t.ut, t.sent_id, t.head;

CREATE OR REPLACE TABLE power_clause_document AS
SELECT DISTINCT ut FROM power_argument;

-- ── Pass 2: predicates and nominalisations, in those documents only ────────
CREATE OR REPLACE TABLE clause_token AS
SELECT t.ut, t.sent_id, t.tok_id, t.head, t.deprel, t.upos,
       lower(t.lemma) AS lemma
FROM read_parquet(getvariable('token_glob')) AS t
SEMI JOIN power_clause_document AS d ON t.ut = d.ut
WHERE t.field = 'abstract'
  AND t.upos IN ('NOUN', 'PROPN', 'VERB', 'AUX');

CREATE OR REPLACE TABLE predicate AS
SELECT c.ut, c.sent_id, c.tok_id AS predicate_id, c.lemma AS predicate_lemma,
       CASE WHEN m.term IS NOT NULL THEN 1 ELSE 0 END AS is_methods_predicate
FROM clause_token AS c
LEFT JOIN methods_predicate_w AS m ON c.lemma = m.term
WHERE c.upos = 'VERB';

-- Deverbal nouns. Conservative suffix set: -tion, -sion, -ment, -ance,
-- -ence, -ure. Gerunds are flagged separately since -ing nouns are noisier.
CREATE OR REPLACE TABLE nominalisation AS
SELECT c.ut, c.sent_id, c.tok_id, c.head AS predicate_id,
       CASE WHEN c.lemma LIKE '%ing' THEN 1 ELSE 0 END AS is_gerund
FROM clause_token AS c
WHERE c.upos = 'NOUN'
  AND (c.lemma LIKE '%tion' OR c.lemma LIKE '%sion' OR c.lemma LIKE '%ment'
    OR c.lemma LIKE '%ance' OR c.lemma LIKE '%ence' OR c.lemma LIKE '%ure'
    OR c.lemma LIKE '%ing')
  AND length(c.lemma) > 5;

-- An actor modifier: possessive, nominal compound, or a by-phrase.
CREATE OR REPLACE TABLE actor_modifier AS
SELECT DISTINCT c.ut, c.sent_id, c.head AS tok_id
FROM clause_token AS c
WHERE c.upos IN ('NOUN', 'PROPN')
  AND c.deprel IN ('nmod:poss', 'compound');

CREATE OR REPLACE TABLE nominalisation_level AS
SELECT
    n.ut, n.sent_id, n.tok_id, n.predicate_id, n.is_gerund,
    CASE WHEN am.tok_id IS NOT NULL THEN 1 ELSE 0 END AS actor_expressed,
    CASE WHEN pa.predicate_id IS NOT NULL THEN 1 ELSE 0 END AS in_power_clause,
    coalesce(pr.is_methods_predicate, 0) AS is_methods_predicate
FROM nominalisation AS n
LEFT JOIN actor_modifier AS am
       ON n.ut = am.ut AND n.sent_id = am.sent_id AND n.tok_id = am.tok_id
LEFT JOIN power_argument AS pa
       ON n.ut = pa.ut AND n.sent_id = pa.sent_id AND n.predicate_id = pa.predicate_id
LEFT JOIN predicate AS pr
       ON n.ut = pr.ut AND n.sent_id = pr.sent_id AND n.predicate_id = pr.predicate_id;

CREATE OR REPLACE TABLE pooled_nominalisation AS
SELECT in_power_clause, is_gerund,
       count(*) AS n_nominalisations,
       sum(actor_expressed) AS n_actor_expressed,
       1 - sum(actor_expressed) / count(*)::DOUBLE AS actor_deletion_rate
FROM nominalisation_level GROUP BY 1, 2 ORDER BY 1, 2;

CREATE OR REPLACE TABLE document_nominalisation AS
SELECT ut, in_power_clause, is_gerund, is_methods_predicate,
       count(*) AS n_nominalisations,
       sum(actor_expressed) AS n_actor_expressed
FROM nominalisation_level
GROUP BY 1, 2, 3, 4;

COPY pooled_nominalisation   TO '/home/martigso/wos_parsed/hedging_probe/pooled_nominalisation.csv' (HEADER, DELIMITER ',');
COPY document_nominalisation TO '/home/martigso/wos_parsed/hedging_probe/document_nominalisation.parquet' (FORMAT PARQUET);

SELECT 'documents with a power clause' AS what, count(*) AS n FROM power_clause_document
UNION ALL SELECT 'power clauses (any predicate)', count(*) FROM power_argument
UNION ALL SELECT 'nominalisations in those documents', count(*) FROM nominalisation_level;
SELECT * FROM pooled_nominalisation;
