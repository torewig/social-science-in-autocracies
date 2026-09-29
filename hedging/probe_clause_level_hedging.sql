-- ---------------------------------------------------------------------------
-- Hedging attached to clauses rather than to whole sentences.
--
-- Until now a hedge counted as "about power" if a power lemma appeared
-- anywhere in the same sentence. A sentence often holds several clauses, so
-- "the ministry closed the papers, which may explain the decline" put the
-- hedge on the power side although it modifies the other clause.
--
-- Each token is walked up to its nearest clausal head, at most three hops,
-- which covers modal auxiliaries, epistemic adverbs and adjectival hedges.
-- A clause is about power when a power lemma is an argument of that clause's
-- own head, reusing power_argument from the widened base.
--
-- Restricted to the 246,607 documents that contain at least one power clause;
-- the within-document contrast is undefined for the rest.
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='14GB';
PRAGMA threads=12;

SET VARIABLE token_glob = '/home/martigso/wos_parsed/tokens/*.parquet';
SET VARIABLE hedging_dir = '/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging';
-- ATTACH does not accept a variable, so the path is literal here.
ATTACH IF NOT EXISTS '/home/martigso/wos_parsed/hedging_probe/widened.duckdb' AS widened (READ_ONLY);

CREATE OR REPLACE TABLE stance_lexicon AS
SELECT lower(term) AS term, type, subtype
FROM read_csv(getvariable('hedging_dir') || '/hyland_hedges_boosters.csv', header = true);

CREATE OR REPLACE TABLE power_argument AS SELECT * FROM widened.power_argument;
CREATE OR REPLACE TABLE power_clause_document AS SELECT * FROM widened.power_clause_document;

CREATE OR REPLACE TABLE token AS
SELECT t.ut, t.sent_id, t.tok_id, t.head, t.deprel, t.upos, lower(t.lemma) AS lemma
FROM read_parquet(getvariable('token_glob')) AS t
SEMI JOIN power_clause_document AS d ON t.ut = d.ut
WHERE t.field = 'abstract';

-- Heads of clauses: the sentence root, subordinate and coordinate predicates.
CREATE OR REPLACE TABLE clause_head AS
SELECT ut, sent_id, tok_id
FROM token
WHERE deprel = 'root'
   OR deprel IN ('advcl', 'ccomp', 'xcomp', 'acl', 'acl:relcl', 'parataxis')
   OR (deprel = 'conj' AND upos IN ('VERB', 'AUX'));

-- Walk up to three hops and take the first clausal head reached.
CREATE OR REPLACE TABLE token_clause AS
WITH hop AS (
    SELECT t.ut, t.sent_id, t.tok_id, t.upos, t.lemma,
           t.tok_id AS hop0, t.head AS hop1,
           h1.head  AS hop2, h2.head AS hop3
    FROM token AS t
    LEFT JOIN token AS h1 ON t.ut = h1.ut AND t.sent_id = h1.sent_id AND h1.tok_id = t.head
    LEFT JOIN token AS h2 ON t.ut = h2.ut AND t.sent_id = h2.sent_id AND h2.tok_id = h1.head
)
SELECT h.ut, h.sent_id, h.tok_id, h.upos, h.lemma,
       coalesce(c0.tok_id, c1.tok_id, c2.tok_id, c3.tok_id) AS clause_head_id
FROM hop AS h
LEFT JOIN clause_head AS c0 ON h.ut = c0.ut AND h.sent_id = c0.sent_id AND c0.tok_id = h.hop0
LEFT JOIN clause_head AS c1 ON h.ut = c1.ut AND h.sent_id = c1.sent_id AND c1.tok_id = h.hop1
LEFT JOIN clause_head AS c2 ON h.ut = c2.ut AND h.sent_id = c2.sent_id AND c2.tok_id = h.hop2
LEFT JOIN clause_head AS c3 ON h.ut = c3.ut AND h.sent_id = c3.sent_id AND c3.tok_id = h.hop3;

CREATE OR REPLACE TABLE clause_word AS
SELECT
    tc.ut, tc.sent_id, tc.clause_head_id,
    count(*) FILTER (tc.upos <> 'PUNCT')                                   AS n_words,
    count(*) FILTER (lex.type = 'hedge' AND lex.subtype <> 'approximator') AS n_hedge_core,
    count(*) FILTER (lex.type = 'booster' AND lex.subtype = 'emphatic')    AS n_booster_core
FROM token_clause AS tc
LEFT JOIN stance_lexicon AS lex
       ON tc.lemma = lex.term
      AND (   (lex.subtype = 'modal'          AND tc.upos IN ('AUX', 'VERB'))
           OR (lex.subtype = 'epistemic_verb' AND tc.upos = 'VERB')
           OR (lex.subtype IN ('epistemic_adjadv', 'emphatic') AND tc.upos IN ('ADJ', 'ADV')))
WHERE tc.clause_head_id IS NOT NULL
GROUP BY tc.ut, tc.sent_id, tc.clause_head_id;

CREATE OR REPLACE TABLE document_clause_hedging AS
SELECT
    cw.ut,
    CASE WHEN pa.predicate_id IS NOT NULL THEN 1 ELSE 0 END AS in_power_clause,
    count(*)              AS n_clauses,
    sum(cw.n_words)       AS n_words,
    sum(cw.n_hedge_core)  AS n_hedge_core,
    sum(cw.n_booster_core) AS n_booster_core
FROM clause_word AS cw
LEFT JOIN power_argument AS pa
       ON cw.ut = pa.ut AND cw.sent_id = pa.sent_id AND cw.clause_head_id = pa.predicate_id
GROUP BY cw.ut, 2;

COPY document_clause_hedging TO '/home/martigso/wos_parsed/hedging_probe/document_clause_hedging.parquet' (FORMAT PARQUET);

SELECT in_power_clause, sum(n_clauses) AS clauses, sum(n_words) AS words,
       1000.0 * sum(n_hedge_core) / sum(n_words)   AS hedges_per_1k,
       1000.0 * sum(n_booster_core) / sum(n_words) AS boosters_per_1k
FROM document_clause_hedging GROUP BY 1 ORDER BY 1;
