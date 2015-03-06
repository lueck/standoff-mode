BEGIN;

INSERT INTO 
       markupDefinition 
       (name, reading, uuid, version) 
VALUES 
       ('beispiel', 'Beispiel', lower(hex(randomblob(16))), datetime('now'));

INSERT INTO 
       markupDefinition 
       (name, reading, uuid, version) 
VALUES 
       ('marker', 'Marker', lower(hex(randomblob(16))), datetime('now'));

INSERT INTO 
       markupDefinition 
       (name, reading, uuid, version) 
VALUES 
       ('konzept', 'Konzept', lower(hex(randomblob(16))), datetime('now'));

COMMIT;
