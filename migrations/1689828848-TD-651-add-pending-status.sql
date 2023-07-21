-- migrations/1689828848-TD-651-add-pending-status.sql
-- :up
-- Up migration
ALTER TABLE apikeys ADD COLUMN pending_status apikeys_status;

-- :down
-- Down migration
ALTER TABLE apikeys DROP COLUMN pending_status;