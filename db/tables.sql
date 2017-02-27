CREATE TABLE cookie
(
    chat_id BIGINT PRIMARY KEY NOT NULL,
    name TEXT,
    value TEXT
);
CREATE TABLE polisinfo
(
    id INTEGER DEFAULT nextval('polisinfo_id_seq'::regclass) PRIMARY KEY NOT NULL,
    alias TEXT NOT NULL,
    polis TEXT NOT NULL,
    date_of_birth CHAR(10),
    chat_id BIGINT NOT NULL,
    CONSTRAINT polisinfo_usercontext_chat_id_fk FOREIGN KEY (chat_id) REFERENCES usercontext (chat_id)
);
CREATE INDEX polisinfo_chat_id_index ON polisinfo (chat_id);
CREATE TABLE usercontext
(
    chat_id BIGINT PRIMARY KEY NOT NULL,
    context JSON
);