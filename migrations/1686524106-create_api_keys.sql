-- migrations/1686524106-create_api_keys.sql
-- :up
-- Up migration
CREATE TYPE apikeys_status AS ENUM ('active', 'revoked');

CREATE TABLE apikeys (
      id            TEXT,
      name          TEXT,
      party_id      TEXT,
      status        apikeys_status,
      revoke_token  TEXT,
      metadata      TEXT,
      created_at    TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
-- :down
-- Down migration
DROP TABLE apikeys;

DROP TYPE apikeys_status;
