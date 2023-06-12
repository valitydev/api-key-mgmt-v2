-- migrations/1686524106-create_api_keys.sql
-- :up
-- Up migration
CREATE TABLE apikeys (
      id            TEXT,
      name          TEXT,
      party_id      TEXT,
      status        TEXT,
      revoke_token  TEXT,
      metadata      TEXT,
      create_at     TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
-- :down
-- Down migration
DROP TABLE apikeys;
