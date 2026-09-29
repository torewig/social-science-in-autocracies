-- ---------------------------------------------------------------------------
-- Prototype of the clause-level agency/hedging measure (theory_memo.md)
--
-- Reads the stanza dependency parse in ~/wos_parsed/tokens and builds, per
-- abstract, a within-document contrast between clauses that are about
-- power-holding entities and all other clauses:
--
--   agent deletion  = passive clauses with no expressed agent (no obl:agent)
--   hedging         = Hyland (2005) hedges per 1000 words
--   boosting        = Hyland (2005) boosters per 1000 words
--
-- Three ways of deciding that a clause is "about power", from loosest to
-- tightest, so that the sentence-keyword proxy used for the preliminary
-- numbers can be checked against tighter definitions:
--
--   sentence_keyword   a power lemma anywhere in the sentence (the proxy)
--   sentence_outside_agent_phrase
--                      same, but power lemmas that sit inside the "by ..."
--                      agent phrase do not count. Guards against the
--                      circularity that naming the agent is both the
--                      classifier and the outcome.
--   predicate_argument a power lemma is a direct dependent of the passive
--                      predicate itself, and is not the agent phrase
--
-- Writes to ~/wos_parsed/hedging_probe/ (outside Dropbox, like the parse).
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='14GB';
PRAGMA threads=12;

SET VARIABLE token_glob = '/home/martigso/wos_parsed/tokens/*.parquet';
SET VARIABLE lexicon_path = '/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging/hyland_hedges_boosters.csv';
SET VARIABLE power_path = '/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging/power_holder_lemmas.csv';
SET VARIABLE methods_path = '/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging/methods_predicate_lemmas.csv';
SET VARIABLE out_dir = '/home/martigso/wos_parsed/hedging_probe';

CREATE OR REPLACE TABLE stance_lexicon AS
SELECT lower(term) AS term, type, subtype
FROM read_csv(getvariable('lexicon_path'), header = true);

CREATE OR REPLACE TABLE methods_predicate AS
SELECT DISTINCT lower(term) AS term
FROM read_csv(getvariable('methods_path'), header = true);

CREATE OR REPLACE TABLE power_lemma AS
SELECT lower(term) AS term, in_strict_list
FROM read_csv(getvariable('power_path'), header = true);

-- ---------------------------------------------------------------------------
-- Pass 1 over the parse: sentence-level word, hedge and booster counts.
-- The POS conditions keep "may" the month out of the modals and the
-- adjective/adverb hedges out of their noun and verb homographs.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE TABLE sentence_counts AS
SELECT
    t.ut,
    t.sent_id,
    max(t.year) AS year,
    count(*) FILTER (t.upos <> 'PUNCT')                                   AS n_words,
    count(*) FILTER (lex.type = 'hedge'   AND lex.subtype <> 'approximator') AS n_hedge_core,
    count(*) FILTER (lex.type = 'hedge')                                  AS n_hedge_all,
    count(*) FILTER (lex.type = 'booster' AND lex.subtype = 'emphatic')   AS n_booster_core,
    count(*) FILTER (lex.type = 'booster')                                AS n_booster_all
FROM read_parquet(getvariable('token_glob')) AS t
LEFT JOIN stance_lexicon AS lex
       ON lower(t.lemma) = lex.term
      AND (   (lex.subtype = 'modal'             AND t.upos IN ('AUX', 'VERB'))
           OR (lex.subtype = 'epistemic_verb'    AND t.upos = 'VERB')
           OR (lex.subtype = 'reporting'         AND t.upos = 'VERB')
           OR (lex.subtype IN ('epistemic_adjadv', 'approximator', 'emphatic')
               AND t.upos IN ('ADJ', 'ADV')))
WHERE t.field = 'abstract'
GROUP BY t.ut, t.sent_id;

-- ---------------------------------------------------------------------------
-- Pass 2 over the parse: only the tokens the clause logic needs.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE TABLE relevant_token AS
SELECT
    t.ut,
    t.sent_id,
    t.tok_id,
    t.head,
    t.deprel,
    lower(t.lemma) AS lemma,
    t.upos
FROM read_parquet(getvariable('token_glob')) AS t
WHERE t.field = 'abstract'
  AND (   t.deprel IN ('nsubj:pass', 'obl:agent')
       OR t.upos IN ('VERB', 'AUX')
       OR (t.upos IN ('NOUN', 'PROPN')
           AND lower(t.lemma) IN (SELECT term FROM power_lemma)));

-- Passive clauses, keyed by their predicate, and the agents that are expressed
CREATE OR REPLACE TABLE passive_clause AS
SELECT DISTINCT ut, sent_id, head AS predicate_id
FROM relevant_token WHERE deprel = 'nsubj:pass';

CREATE OR REPLACE TABLE expressed_agent AS
SELECT ut, sent_id, tok_id, head AS predicate_id
FROM relevant_token WHERE deprel = 'obl:agent';

-- Power tokens, flagged for whether they sit inside a "by ..." agent phrase
CREATE OR REPLACE TABLE power_token AS
SELECT
    r.ut,
    r.sent_id,
    r.tok_id,
    r.head,
    r.deprel,
    p.in_strict_list,
    CASE WHEN r.deprel = 'obl:agent' OR a.tok_id IS NOT NULL THEN 1 ELSE 0 END
        AS inside_agent_phrase
FROM relevant_token AS r
JOIN power_lemma AS p ON r.lemma = p.term
LEFT JOIN expressed_agent AS a
       ON r.ut = a.ut AND r.sent_id = a.sent_id AND r.head = a.tok_id
WHERE r.upos IN ('NOUN', 'PROPN');

CREATE OR REPLACE TABLE power_sentence AS
SELECT
    ut,
    sent_id,
    1                                                        AS has_power_lemma,
    max(in_strict_list)                                      AS has_power_lemma_strict,
    max(CASE WHEN inside_agent_phrase = 0 THEN 1 ELSE 0 END) AS has_power_lemma_outside_agent,
    max(CASE WHEN inside_agent_phrase = 0 THEN in_strict_list ELSE 0 END)
                                                             AS has_power_lemma_outside_agent_strict
FROM power_token
GROUP BY ut, sent_id;

-- Power lemma attached directly to the passive predicate
CREATE OR REPLACE TABLE power_predicate AS
SELECT
    pt.ut,
    pt.sent_id,
    pt.head AS predicate_id,
    1                       AS power_is_predicate_argument,
    max(pt.in_strict_list)  AS power_is_predicate_argument_strict
FROM power_token AS pt
JOIN passive_clause AS pc
      ON pt.ut = pc.ut AND pt.sent_id = pc.sent_id AND pt.head = pc.predicate_id
WHERE pt.inside_agent_phrase = 0
GROUP BY pt.ut, pt.sent_id, pt.head;

-- ---------------------------------------------------------------------------
-- One row per passive clause, with the three power definitions
-- ---------------------------------------------------------------------------
CREATE OR REPLACE TABLE predicate_lemma AS
SELECT ut, sent_id, tok_id AS predicate_id, lemma AS predicate_lemma,
       CASE WHEN m.term IS NOT NULL THEN 1 ELSE 0 END AS predicate_is_methods_verb
FROM relevant_token AS r
LEFT JOIN methods_predicate AS m ON r.lemma = m.term
WHERE r.upos IN ('VERB', 'AUX');

CREATE OR REPLACE TABLE clause_level AS
SELECT
    pc.ut,
    pc.sent_id,
    pc.predicate_id,
    CASE WHEN ea.predicate_id IS NOT NULL THEN 1 ELSE 0 END AS agent_expressed,
    coalesce(ps.has_power_lemma, 0)                      AS power_sentence_keyword,
    coalesce(ps.has_power_lemma_strict, 0)               AS power_sentence_keyword_strict,
    coalesce(ps.has_power_lemma_outside_agent, 0)        AS power_sentence_outside_agent,
    coalesce(ps.has_power_lemma_outside_agent_strict, 0) AS power_sentence_outside_agent_strict,
    coalesce(pp.power_is_predicate_argument, 0)          AS power_predicate_argument,
    coalesce(pp.power_is_predicate_argument_strict, 0)   AS power_predicate_argument_strict,
    coalesce(pl.predicate_is_methods_verb, 0)            AS predicate_is_methods_verb,
    pl.predicate_lemma
FROM passive_clause AS pc
LEFT JOIN (SELECT DISTINCT ut, sent_id, predicate_id FROM expressed_agent) AS ea
       ON pc.ut = ea.ut AND pc.sent_id = ea.sent_id AND pc.predicate_id = ea.predicate_id
LEFT JOIN power_sentence AS ps
       ON pc.ut = ps.ut AND pc.sent_id = ps.sent_id
LEFT JOIN power_predicate AS pp
       ON pc.ut = pp.ut AND pc.sent_id = pp.sent_id AND pc.predicate_id = pp.predicate_id
LEFT JOIN predicate_lemma AS pl
       ON pc.ut = pl.ut AND pc.sent_id = pl.sent_id AND pc.predicate_id = pl.predicate_id;

-- ---------------------------------------------------------------------------
-- Pooled baselines: the preliminary-numbers table, plus the same thing under
-- the two tighter power definitions
-- ---------------------------------------------------------------------------
CREATE OR REPLACE TABLE pooled_baseline AS
WITH sentence_side AS (
    SELECT
        sc.ut, sc.sent_id, sc.n_words, sc.n_hedge_core, sc.n_booster_core,
        coalesce(ps.has_power_lemma, 0)               AS sentence_keyword,
        coalesce(ps.has_power_lemma_outside_agent, 0) AS sentence_outside_agent,
        coalesce(ps.has_power_lemma_strict, 0)        AS sentence_keyword_strict
    FROM sentence_counts AS sc
    LEFT JOIN power_sentence AS ps ON sc.ut = ps.ut AND sc.sent_id = ps.sent_id
),
clause_side AS (
    SELECT ut, sent_id,
           count(*)              AS n_passive,
           sum(agent_expressed)  AS n_agent_expressed
    FROM clause_level GROUP BY ut, sent_id
)
SELECT
    definition,
    is_power_side,
    count(*)                                              AS n_sentences,
    sum(coalesce(cs.n_passive, 0))                        AS n_passive_clauses,
    1 - sum(coalesce(cs.n_agent_expressed, 0))
        / nullif(sum(coalesce(cs.n_passive, 0)), 0)       AS agent_deletion_rate,
    1000.0 * sum(ss.n_hedge_core)   / sum(ss.n_words)     AS hedges_per_1k_words,
    1000.0 * sum(ss.n_booster_core) / sum(ss.n_words)     AS boosters_per_1k_words
FROM sentence_side AS ss
LEFT JOIN clause_side AS cs ON ss.ut = cs.ut AND ss.sent_id = cs.sent_id,
LATERAL (VALUES
    ('sentence_keyword',             ss.sentence_keyword),
    ('sentence_outside_agent_phrase', ss.sentence_outside_agent),
    ('sentence_keyword_strict',      ss.sentence_keyword_strict)
) AS v(definition, is_power_side)
GROUP BY definition, is_power_side
ORDER BY definition, is_power_side;

-- ---------------------------------------------------------------------------
-- Document-level within-document contrast, one row per abstract
-- ---------------------------------------------------------------------------
CREATE OR REPLACE TABLE document_contrast AS
WITH clause_by_side AS (
    SELECT
        ut,
        sum(CASE WHEN power_sentence_keyword = 1 THEN 1 ELSE 0 END)                       AS n_passive_power_kw,
        sum(CASE WHEN power_sentence_keyword = 1 THEN agent_expressed ELSE 0 END)         AS n_agent_power_kw,
        sum(CASE WHEN power_sentence_keyword = 0 THEN 1 ELSE 0 END)                       AS n_passive_other_kw,
        sum(CASE WHEN power_sentence_keyword = 0 THEN agent_expressed ELSE 0 END)         AS n_agent_other_kw,
        sum(CASE WHEN power_sentence_outside_agent = 1 THEN 1 ELSE 0 END)                 AS n_passive_power_oa,
        sum(CASE WHEN power_sentence_outside_agent = 1 THEN agent_expressed ELSE 0 END)   AS n_agent_power_oa,
        sum(CASE WHEN power_sentence_outside_agent = 0 THEN 1 ELSE 0 END)                 AS n_passive_other_oa,
        sum(CASE WHEN power_sentence_outside_agent = 0 THEN agent_expressed ELSE 0 END)   AS n_agent_other_oa,
        sum(CASE WHEN power_predicate_argument = 1 THEN 1 ELSE 0 END)                     AS n_passive_power_pa,
        sum(CASE WHEN power_predicate_argument = 1 THEN agent_expressed ELSE 0 END)       AS n_agent_power_pa,
        sum(CASE WHEN power_predicate_argument = 0 THEN 1 ELSE 0 END)                     AS n_passive_other_pa,
        sum(CASE WHEN power_predicate_argument = 0 THEN agent_expressed ELSE 0 END)       AS n_agent_other_pa
    FROM clause_level
    GROUP BY ut
),
words_by_side AS (
    SELECT
        sc.ut,
        max(sc.year) AS year,
        sum(sc.n_words)                                                                    AS n_words_total,
        sum(CASE WHEN coalesce(ps.has_power_lemma, 0) = 1 THEN sc.n_words ELSE 0 END)      AS n_words_power_kw,
        sum(CASE WHEN coalesce(ps.has_power_lemma, 0) = 1 THEN sc.n_hedge_core ELSE 0 END) AS n_hedge_power_kw,
        sum(CASE WHEN coalesce(ps.has_power_lemma, 0) = 1 THEN sc.n_booster_core ELSE 0 END) AS n_booster_power_kw,
        sum(CASE WHEN coalesce(ps.has_power_lemma, 0) = 0 THEN sc.n_words ELSE 0 END)      AS n_words_other_kw,
        sum(CASE WHEN coalesce(ps.has_power_lemma, 0) = 0 THEN sc.n_hedge_core ELSE 0 END) AS n_hedge_other_kw,
        sum(CASE WHEN coalesce(ps.has_power_lemma, 0) = 0 THEN sc.n_booster_core ELSE 0 END) AS n_booster_other_kw,
        sum(CASE WHEN coalesce(ps.has_power_lemma_outside_agent, 0) = 1 THEN sc.n_words ELSE 0 END)      AS n_words_power_oa,
        sum(CASE WHEN coalesce(ps.has_power_lemma_outside_agent, 0) = 1 THEN sc.n_hedge_core ELSE 0 END) AS n_hedge_power_oa,
        sum(CASE WHEN coalesce(ps.has_power_lemma_outside_agent, 0) = 1 THEN sc.n_booster_core ELSE 0 END) AS n_booster_power_oa,
        sum(CASE WHEN coalesce(ps.has_power_lemma_outside_agent, 0) = 0 THEN sc.n_words ELSE 0 END)      AS n_words_other_oa,
        sum(CASE WHEN coalesce(ps.has_power_lemma_outside_agent, 0) = 0 THEN sc.n_hedge_core ELSE 0 END) AS n_hedge_other_oa,
        sum(CASE WHEN coalesce(ps.has_power_lemma_outside_agent, 0) = 0 THEN sc.n_booster_core ELSE 0 END) AS n_booster_other_oa
    FROM sentence_counts AS sc
    LEFT JOIN power_sentence AS ps ON sc.ut = ps.ut AND sc.sent_id = ps.sent_id
    GROUP BY sc.ut
)
SELECT w.*, c.* EXCLUDE (ut)
FROM words_by_side AS w
LEFT JOIN clause_by_side AS c ON w.ut = c.ut;

-- One row per document x power definition x side x methods filter, so that
-- every variant stays reportable instead of one being hard-coded as the
-- headline.
CREATE OR REPLACE TABLE document_clause_counts AS
SELECT
    cl.ut,
    v.definition,
    v.is_power_side,
    f.exclude_methods_predicates,
    count(*)                 AS n_passive,
    sum(cl.agent_expressed)  AS n_agent_expressed
FROM clause_level AS cl,
LATERAL (VALUES
    ('sentence_keyword',              cl.power_sentence_keyword),
    ('sentence_outside_agent_phrase', cl.power_sentence_outside_agent),
    ('predicate_argument',            cl.power_predicate_argument)
) AS v(definition, is_power_side),
LATERAL (VALUES (0), (1)) AS f(exclude_methods_predicates)
WHERE f.exclude_methods_predicates = 0 OR cl.predicate_is_methods_verb = 0
GROUP BY 1, 2, 3, 4;

CREATE OR REPLACE TABLE methods_predicate_share AS
SELECT definition, is_power_side,
       sum(predicate_is_methods_verb) AS n_methods_predicate,
       count(*)                       AS n_passive,
       100.0 * sum(predicate_is_methods_verb) / count(*) AS pct_methods
FROM clause_level AS cl,
LATERAL (VALUES
    ('sentence_keyword',              cl.power_sentence_keyword),
    ('sentence_outside_agent_phrase', cl.power_sentence_outside_agent),
    ('predicate_argument',            cl.power_predicate_argument)
) AS v(definition, is_power_side)
GROUP BY 1, 2 ORDER BY 1, 2;

COPY document_clause_counts TO '/home/martigso/wos_parsed/hedging_probe/document_clause_counts.parquet' (FORMAT PARQUET);
COPY methods_predicate_share TO '/home/martigso/wos_parsed/hedging_probe/methods_predicate_share.csv' (HEADER, DELIMITER ',');

COPY pooled_baseline   TO '/home/martigso/wos_parsed/hedging_probe/pooled_baseline.csv' (HEADER, DELIMITER ',');
COPY document_contrast TO '/home/martigso/wos_parsed/hedging_probe/document_contrast.parquet' (FORMAT PARQUET);

SELECT 'sentences' AS table_name, count(*) AS n FROM sentence_counts
UNION ALL SELECT 'passive_clauses', count(*) FROM clause_level
UNION ALL SELECT 'documents', count(*) FROM document_contrast;
