-- Face validity check: print sentences the instrument classifies as
-- power-clauses with the agent deleted, and with the agent expressed.
-- Reads the probe database built by probe_clause_measure.sql.
PRAGMA memory_limit='6GB';

SET VARIABLE token_glob = '/home/martigso/wos_parsed/tokens/part-0000[0-4]*.parquet';

WITH flagged AS (
    SELECT c.ut, c.sent_id, c.agent_expressed
    FROM clause_level AS c
    WHERE c.power_sentence_outside_agent_strict = 1
),
sentence_text AS (
    SELECT t.ut, t.sent_id,
           string_agg(t.text, ' ' ORDER BY t.tok_id) AS sentence
    FROM read_parquet(getvariable('token_glob')) AS t
    JOIN (SELECT DISTINCT ut, sent_id FROM flagged) AS f
      ON t.ut = f.ut AND t.sent_id = f.sent_id
    WHERE t.field = 'abstract'
    GROUP BY t.ut, t.sent_id
)
SELECT f.agent_expressed, substr(s.sentence, 1, 240) AS sentence
FROM flagged AS f JOIN sentence_text AS s ON f.ut = s.ut AND f.sent_id = s.sent_id
USING SAMPLE 24 ROWS
ORDER BY f.agent_expressed;
