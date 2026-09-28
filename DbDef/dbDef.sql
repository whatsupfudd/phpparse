CREATE EXTENSION IF NOT EXISTS pgcrypto;


DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'project_kind') THEN
    CREATE TYPE project_kind AS ENUM ('php', 'html', 'ruby', 'python', 'cpp', 'js', 'react');
  END IF;
END$$;


create table IF NOT EXISTS project (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , label varchar(512)
  , kind project_kind
  , eid UUID NOT NULL DEFAULT gen_random_uuid()
  , createdOn TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);


-- Folder: a folder in the WordPress code base.
create table IF NOT EXISTS folder (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , project_fk bigint not null references project(uid)
  , path varchar(1024)
  , parent_fk bigint references folder(uid)
);

-- File: a PHP file of WordPress.
create table IF NOT EXISTS File (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , folder_fk bigint references Folder(uid)  -- null if the file is in the root folder.
  , path varchar(1024)
);

create table IF NOT EXISTS Constant (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , file_fk bigint not null references File(uid)
  , value bytea
);

create table IF NOT EXISTS AST (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , file_fk bigint not null references File(uid)
  , value bytea
);

create table IF NOT EXISTS Tag (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , name varchar(64)
);

create table IF NOT EXISTS TagAstRef (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , tag_fk bigint not null references Tag(uid)
  , ast_fk bigint not null references AST(uid)
  , node bigint not null
);

create table IF NOT EXISTS TagConstantRef (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , tag_fk bigint not null references Tag(uid)
  , constant_fk bigint not null references Constant(uid)
  , node bigint not null
);

create table IF NOT EXISTS Error (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , file_fk bigint not null references File(uid)
  , createdOn TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
  , procTime real
  , message text
);
