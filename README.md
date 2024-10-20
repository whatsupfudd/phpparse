# phpparse
Using the Cannelle package, the `phpparse` tool parses PHP source code into an abstract syntax tree (AST), and then stores that into a database schema.

## Usage
```
phpparse wpload [--version/-v <version>] <path>
```
If the `path` is a directory, all `.php` files in the directory will be parsed and the resulting dir-tree will be mapped into
the database, and then every file will be parsed and the resulting AST and string constants will also be stored in the database.

If the `path` is a single file, it will be parsed and the result will be presented on the standard output.

The `--version/-v` option specifies which version of WordPress the code relates to. By default it uses 6.2.0; the default can be modified using the `version` entry in the configuration file (~/.fudd/phpparse/config.yaml).

