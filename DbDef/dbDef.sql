-- WPVersion: Keep track of the versions of WordPress being stored in the DB.
create table WPVersion (
  uid serial primary key
  , label varchar(64)
);

-- Folder: a folder in the WordPress code base.
create table Folder (
  uid serial primary key
  , versionRef int not null references WPVersion(uid)
  , path varchar(1024)
  , parentRef int references Folder(uid)
);

-- File: a PHP file of WordPress.
create table File (
  uid serial primary key
  , folderRef int references Folder(uid)  -- null if the file is in the root folder.
  , path varchar(1024)
);

create table Constant (
  uid serial primary key
  , fileRef int not null references File(uid)
  , value bytea
);

create table AST (
  uid serial primary key
  , fileRef int not null references File(uid)
  , value bytea
);

create table Tag (
  uid serial primary key
  , name varchar(64)
);

create table TagAstRef (
  uid serial primary key
  , tagRef int not null references Tag(uid)
  , astRef int not null references AST(uid)
  , node int not null
);

create table TagConstantRef (
  uid serial primary key
  , tagRef int not null references Tag(uid)
  , constantRef int not null references Constant(uid)
  , node int not null
);

create table Error (
  uid serial primary key
  , fileRef int not null references File(uid)
  , procTime real
  , message text
);
