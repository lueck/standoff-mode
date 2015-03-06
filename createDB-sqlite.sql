-- Creator:       MySQL Workbench 5.2.40/ExportSQLite plugin 2013.08.05
-- Author:        Christian Lück
-- Caption:       New Model
-- Project:       Name of the project
-- Changed:       2015-03-04 00:55
-- Created:       2015-03-03 21:29
-- Revised:	  Needed some revision after export.

-- foreign key support requires sqlite >= 3.6.19, Debian wheezy
-- comes with 3.7.13, so we turn it on
PRAGMA foreign_keys = ON;

-- Schema: StandoffMarkup

-- - identifying 1:n relationship between markupInstance and
--   stringrange

-- - comment: YES! But since compound integer primary keys are not
--   support but only text PKs (since 0.0 equals 0, see faq) we only
--   use non-identifying relationships

-- - uuid: It should be a 128-bit integer, but sqlite3 offers only 8
--         byte integers. So we use TEXT.
--	   Use lower(hex(randomblob(16)) to create a uuid value.

-- BEGIN;

CREATE TABLE If NOT EXISTS "document"(
  "documentID" INTEGER PRIMARY KEY NOT NULL,
  "uuid" TEXT NOT NULL,
  "text" LONGTEXT,
  "md5checksum" BIGINT,
  "mimeType" VARCHAR(255),
  "encoding" VARCHAR(45),
  "sourceUri" VARCHAR(300),
  "localUri" VARCHAR(300),
  CONSTRAINT "uuid_UNIQUE"
    UNIQUE("uuid")
);

CREATE TABLE If NOT EXISTS "markupDefinition"(
  "markupDefinitionID" INTEGER PRIMARY KEY NOT NULL,
  "uuid" TEXT NOT NULL,
  "name" VARCHAR(45) NOT NULL,
  "reading" VARCHAR(500),
  "version" DATETIME,
  CONSTRAINT "uuid_UNIQUE"
    UNIQUE("uuid")
);

CREATE TABLE If NOT EXISTS "markupInstance"(
  "markupInstanceID" INTEGER PRIMARY KEY NOT NULL,
  "documentID" INTEGER NOT NULL,
  "uuid" TEXT NOT NULL,
  "markupDefinitionID" INTEGER NOT NULL,
  CONSTRAINT "uuid_UNIQUE"
    UNIQUE("uuid"),
  CONSTRAINT "fk_markupInstance_document1"
    FOREIGN KEY("documentID")
    REFERENCES "document"("documentID"),
  CONSTRAINT "fk_markupInstance_markupDefinition1"
    FOREIGN KEY("markupDefinitionID")
    REFERENCES "markupDefinition"("markupDefinitionID")
);
CREATE INDEX If NOT EXISTS "markupInstance.fk_markupInstance_document1" ON "markupInstance"("documentID");
CREATE INDEX If NOT EXISTS "markupInstance.fk_markupInstance_markupDefinition1" ON "markupInstance"("markupDefinitionID");

CREATE TABLE If NOT EXISTS "stringrange"(
  "stringrangeID" INTEGER PRIMARY KEY NOT NULL,
  "documentID" INTEGER NOT NULL,
  "markupInstanceID" INTEGER NOT NULL,
  "uuid" TEXT NOT NULL,
  "startchar" INTEGER NOT NULL,
  "endchar" INTEGER NOT NULL,
  CONSTRAINT "uuid_UNIQUE"
    UNIQUE("uuid"),
  CONSTRAINT "fk_stringrange_markupInstance"
    FOREIGN KEY("markupInstanceID")
    REFERENCES "markupInstance"("markupInstanceID"),
  CONSTRAINT "fk_stringrange_document"
    FOREIGN KEY("documentID")
    REFERENCES "document"("documentID")
);
CREATE INDEX If NOT EXISTS "stringrange.fk_stringrange_markupInstance" ON "stringrange"("markupInstanceID");
CREATE INDEX If NOT EXISTS "stringrange.fk_stringrange_document" ON "stringrange"("documentID");

-- COMMIT;
