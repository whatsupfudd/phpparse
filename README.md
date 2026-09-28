# phpparse

**PHP source ingestion, compact structural serialization, and source-code
intelligence for the FUDD ecosystem.**

`phpparse` is a Haskell application and library that parses PHP source code
through **Cannelle**, converts the resulting typed PHP representation into a
compact binary form, and optionally persists the analysed project into
PostgreSQL for later inspection, analysis, and transformation.

The project began with WordPress source-code ingestion and its current
database model still reflects that origin.

Its wider architectural role is more general:

> turn PHP source trees into durable machine-readable program structure that
> FUDD tools can analyse without repeatedly reparsing the original source.

At a high level:

```text
PHP project
    |
    v
filesystem discovery
    |
    v
PHP files
    |
    v
Tree-sitter
    |
    v
Cannelle PHP parser
    |
    v
typed PhpContext
    |
    +--------------------+
    |                    |
    v                    v
semantic action      demanded source
structure             fragments
    |                    |
    v                    v
compact opcode      deduplicated
stream              constant pool
    |                    |
    +----------+---------+
               |
               v
           PostgreSQL
               |
               v
      downstream analysis
               |
      +--------+---------+
      |                  |
      v                  v
   Recycler           Daniell /
modernisation       FUDD tooling
```

The current application supports two particularly useful modes:

- parse one PHP file and print Cannelle's parsed representation; or
- import all PHP files under a directory into the source-analysis database.

The current package version is:

```text
0.1.0.0
```

`phpparse` is an early-stage development tool. The existing parser/ingestion
pipeline is useful, but the binary serialization format, database lifecycle,
tests, and downstream analysis APIs should not yet be considered stable.

---

## Contents

- [Role in the FUDD ecosystem](#role-in-the-fudd-ecosystem)
- [Relationship with Cannelle](#relationship-with-cannelle)
- [Relationship with Recycler](#relationship-with-recycler)
- [Relationship with Daniell](#relationship-with-daniell)
- [What phpparse does](#what-phpparse-does)
- [Current implementation status](#current-implementation-status)
- [Architecture](#architecture)
- [Getting started](#getting-started)
- [Configuration](#configuration)
- [Command-line interface](#command-line-interface)
- [Single-file parsing](#single-file-parsing)
- [Directory ingestion](#directory-ingestion)
- [Filesystem discovery](#filesystem-discovery)
- [Cannelle PHP representation](#cannelle-php-representation)
- [Source-fragment extraction](#source-fragment-extraction)
- [Constant-pool compaction](#constant-pool-compaction)
- [AST binary representation](#ast-binary-representation)
- [Constant-pool binary representation](#constant-pool-binary-representation)
- [PostgreSQL persistence](#postgresql-persistence)
- [Parse-error handling](#parse-error-handling)
- [Current data lifecycle](#current-data-lifecycle)
- [Source provenance](#source-provenance)
- [Target architecture](#target-architecture)
- [Analysis overlays](#analysis-overlays)
- [PHP modernisation workflow](#php-modernisation-workflow)
- [Module map](#module-map)
- [Testing strategy](#testing-strategy)
- [Performance considerations](#performance-considerations)
- [Security and untrusted source](#security-and-untrusted-source)
- [Development roadmap](#development-roadmap)
- [Design principles](#design-principles)
- [Current limitations](#current-limitations)
- [Repository housekeeping](#repository-housekeeping)
- [License](#license)

---

# Role in the FUDD ecosystem

FUDD's software-modernisation tools need to treat existing applications as
structured data.

A PHP application is more than a directory containing text files.

Its source expresses:

```text
functions
classes
methods
properties
variables
calls
assignments
control flow
includes
namespaces
constants
expressions
dependencies
templates
framework conventions
```

Working only with raw source text forces every analysis step to rediscover
that structure.

`phpparse` provides an ingestion boundary:

```text
legacy PHP source
       |
       v
    phpparse
       |
       v
machine-readable
program structure
       |
       +------> search
       +------> inspection
       +------> dependency analysis
       +------> transformation
       +------> architecture recovery
       +------> code generation
       `------> modernisation
```

The project is particularly relevant to PHP/WordPress modernisation, but the
core PHP representation is not inherently limited to WordPress.

---

# Relationship with Cannelle

The actual PHP grammar and typed PHP parser belong to **Cannelle**.

`phpparse` imports:

```haskell
Cannelle.PHP.Parse
Cannelle.PHP.AST
Cannelle.PHP.Print
```

and uses:

```haskell
tsParsePhp
```

to parse source.

A useful architectural distinction is:

```text
Cannelle
--------

Tree-sitter integration
PHP CST interpretation
typed PHP structures
compiler/parser infrastructure


phpparse
--------

source-tree discovery
project ingestion
compact serialization
PostgreSQL persistence
parse statistics/errors
source snapshot organization
```

Conceptually:

```text
PHP bytes
   |
   v
Tree-sitter CST
   |
   v
Cannelle combinatorial processing
   |
   v
PhpContext
   |
   v
phpparse serialization / persistence
```

This is important because `phpparse` should not gradually duplicate
Cannelle's PHP parser.

Improvements to PHP syntax understanding belong primarily in Cannelle.

Improvements to durable project ingestion, storage, provenance, and
source-intelligence workflows belong primarily in `phpparse` or downstream
consumers.

---

# Relationship with Recycler

One of the major long-term consumers of PHP source intelligence is
**Recycler**.

Recycler is intended to understand and progressively modernise existing
applications into FUDD/EasyWordy/Wapp architectures.

For PHP applications, the broad pipeline is:

```text
existing PHP application
          |
          v
       phpparse
          |
          v
Cannelle PHP structure
          |
          v
durable source representation
          |
          v
       Recycler
          |
    +-----+-------+
    |             |
    v             v
analysis       annotation
    |             |
    +------+------+
           |
           v
architecture recovery
           |
           v
Fuddle / EasyWordy /
Haskell generation
           |
           v
equivalence testing
           |
           v
progressive replacement
```

`phpparse` therefore provides **source evidence** for Recycler.

It should not itself become the entire modernisation engine.

---

# Relationship with Daniell

Daniell also performs project discovery and source-oriented processing.

The current division is roughly:

```text
phpparse
    specialized PHP/WordPress ingestion
    compact PHP serialization
    source database population

Daniell
    multi-technology project discovery
    project planning
    transformation orchestration
    materialisation / scaffolding

Cannelle
    language and template parsing
    typed representations
    compiler-oriented processing
```

Some functionality currently overlaps, particularly filesystem discovery and
PHP ingestion.

As the FUDD architecture matures, shared mechanisms should move toward
common libraries rather than being independently maintained in several
applications.

---

# What phpparse does

The current `wpload` command behaves differently depending on whether its
argument is a file or directory.

```text
PATH
 |
 +-- file --------------------------+
 |                                  |
 |                                  v
 |                            parse with Cannelle
 |                                  |
 |                                  v
 |                         print PhpContext
 |
 `-- directory ---------------------+
                                    |
                                    v
                              scan PHP files
                                    |
                                    v
                          register source tree
                                    |
                                    v
                         parse each PHP file
                                    |
                  +-----------------+----------------+
                  |                                  |
               success                            failure
                  |                                  |
                  v                                  v
         compact constants                     store error
                  |
                  v
          serialize PHP logic
                  |
                  v
        store AST + constants
```

The directory mode requires PostgreSQL.

The single-file mode does not persist the parse result.

---

# Current implementation status

| Capability | Status |
| --- | --- |
| Parse one PHP file | Implemented |
| Print parsed `PhpContext` | Implemented |
| Recursively scan directory | Implemented |
| Ignore `.git` trees | Implemented |
| Filter directory import to `.php` | Implemented |
| WordPress-version namespace | Implemented |
| Folder registration | Implemented |
| File registration | Implemented |
| Cannelle PHP parsing | Implemented |
| Parse-error persistence | Implemented |
| Parse duration measurement | Partial |
| Source-fragment extraction | Implemented |
| Constant deduplication | Implemented |
| Compact AST opcode stream | Implemented, incomplete format |
| Compact constant pool | Implemented |
| AST persistence as `bytea` | Implemented |
| Constant-pool persistence as `bytea` | Implemented |
| Incremental/replacement imports | Not implemented |
| AST binary version header | Not implemented |
| Stable public binary specification | Not implemented |
| Binary deserializer | Not present here |
| Source-content hashing | Not implemented |
| File-change detection | Not implemented |
| Dependency graph | Not implemented |
| Call graph | Not implemented |
| Symbol index | Not implemented |
| Data/control-flow analysis | Not implemented |
| Project comparison | Not implemented |
| Recycler analysis overlays | Roadmap |
| Automated tests | Not implemented |

---

# Architecture

The current directory-ingestion architecture is:

```text
                     PHP source tree
                           |
                           v
                  FileSystem.Explore
                           |
                           v
                PHP-containing folders
                           |
                           v
                     PostgreSQL
              version / folder / file
                           |
                           v
                    tsParsePhp
                           |
                           v
                     PhpContext
                    /          \
                   /            \
                  v              v
               logic      contentDemands
                  |              |
                  |              v
                  |         source slices
                  |              |
                  |              v
                  |        MD5 compaction
                  |              |
                  +-------+------+
                          |
                          v
                    ID rewriting
                          |
              +-----------+-----------+
              |                       |
              v                       v
      packed AST bytea        constants bytea
              |                       |
              +-----------+-----------+
                          |
                          v
                      PostgreSQL
```

The use of a separate constant pool is central to the current design.

Instead of copying every identifier, string, literal fragment, comment, or
verbatim region repeatedly into the structural stream, the AST can refer to
compact integer IDs.

---

# Getting started

## Requirements

The current project uses:

```text
Package:        phpparse-0.1.0.0
Stack snapshot: LTS 21.25
GHC:            9.4.8
system-ghc:     true
```

Important dependencies include:

```text
Cannelle
tree-sitter
tree-sitter-php
Hasql
Hasql.TH
Hasql.Pool
postgresql-binary
cryptohash-md5
binary
pathwalk
```

---

## Local FUDD dependencies

The current `stack.yaml` expects local development checkouts:

```yaml
extra-deps:
- ../../LocalPkgs/cannelle
- ../../LocalPkgs/haskell-tree-sitter/tree-sitter
- ../../LocalPkgs/haskell-tree-sitter/tree-sitter-haskell
- ../../LocalPkgs/haskell-tree-sitter/tree-sitter-php
```

A standalone clone of `phpparse` is therefore **not currently
self-contained**.

Either reproduce the FUDD development workspace layout or update the package
locations to appropriate Git/published dependencies.

---

## Clone

```bash
git clone git@github.com:whatsupfudd/phpparse.git
cd phpparse
```

---

## Build

```bash
stack build
```

---

## CLI help

```bash
stack exec -- phpparse --help
```

The current commands are:

```text
help
version
wpload
```

---

# Configuration

The current default configuration file is:

```text
~/.fudd/phpparse/config.yaml
```

It can be replaced using:

```text
--config
-c
```

or the environment variable:

```text
phpparseCONF
```

The application also reads:

```text
phpparseHOME
```

although the resulting application-home value is not currently used by the
PHP processing pipeline.

---

## Example configuration

```yaml
debug: 0

version: "6.2.1"

db:
  host: "127.0.0.1"
  port: 5432
  user: "phpparse"
  passwd: "development-password"
  dbase: "phpparse"
  poolSize: 5
  poolTimeOut: 60
```

The `version` value currently represents the **WordPress source version
label** used to group imported source trees.

Despite one CLI help string currently describing `--version` as a "Version
of PHP to parse", the implementation stores the value in the `WPVersion`
table and defaults to a WordPress-style version.

The current compiled default is:

```text
6.2.1
```

---

## Configuration fields currently used

The effective runtime currently consumes:

```text
debug
version

db.host
db.port
db.user
db.passwd
db.dbase
```

The configuration structures also contain:

```text
primaryLocale
db.poolSize
db.poolTimeOut
```

but those values are not currently propagated by `mergeOptions`.

The PostgreSQL pool therefore continues to use its compiled defaults for
those settings.

---

## Development database defaults

If no database configuration overrides them, the current compiled defaults
are:

```text
host      = test
port      = 5432
user      = test
password  = test
database  = test
pool size = 5
```

These values are development placeholders.

Real deployments should always provide explicit credentials.

---

# Command-line interface

## Version

```bash
stack exec -- phpparse version
```

This prints:

```text
package version
Git commit hash
Git commit date
```

---

## Help

```bash
stack exec -- phpparse help
```

The command itself is currently a placeholder.

For useful parser-generated help use:

```bash
stack exec -- phpparse --help
```

---

## PHP/WordPress ingestion

```bash
stack exec -- phpparse wpload PATH
```

An explicit source-version label can be supplied with:

```bash
stack exec -- phpparse wpload \
  --version 6.2.1 \
  /path/to/wordpress
```

Global options precede the command when required:

```bash
stack exec -- phpparse \
  --config ~/.fudd/phpparse/config.yaml \
  wpload \
  --version 6.2.1 \
  /path/to/wordpress
```

The current directory-mode implementation requires the version label to
contain exactly three dot-separated components.

---

# Single-file parsing

Given:

```bash
stack exec -- phpparse wpload ./example.php
```

`phpparse`:

```text
reads/parses the PHP file
        |
        v
Cannelle tsParsePhp
        |
        +---- failure -> console error
        |
        `---- success -> printPhpContext
```

No PostgreSQL persistence occurs for the file in this mode.

This makes single-file mode useful for:

```text
parser development
grammar debugging
examining Cannelle output
testing unfamiliar PHP constructs
```

The source representation printed here is Cannelle's developer-facing
representation rather than the compact database format.

---

# Directory ingestion

For a directory:

```bash
stack exec -- phpparse wpload \
  --version 6.2.1 \
  ./wordpress
```

the current process is substantially different.

```text
root directory
     |
     v
discover .php files
     |
     v
locate/create source version
     |
     v
for every directory containing PHP:
     |
     +--> locate/create Folder
     |
     `--> for every PHP file:
             |
             +--> locate/create File
             |
             +--> parse PHP
             |
             +--> extract demanded source
             |
             +--> compact constant pool
             |
             +--> encode AST
             |
             +--> insert AST
             |
             `--> insert Constant pool
```

Parse failures are inserted into the `Error` table instead.

---

# Filesystem discovery

The project contains a generic recursive filesystem scanner under:

```text
FileSystem.Explore
```

using `pathwalk`.

The scanner:

```text
walks recursively
skips directories beneath .git
classifies known file suffixes
returns paths relative to the project root
```

The generic classifier recognizes such types as:

```text
PHP
HTML
JavaScript
TypeScript
TSX / JSX
Haskell
Elm
CSS
Markdown
YAML
TOML
JSON
XML
Org
AsciiDoc
Pandoc
RSS
text
```

For `phpparse` directory processing, however, the caller supplies a filter
that accepts only:

```text
.php
```

files.

The wider classification system is therefore mostly shared/generic
infrastructure in the current application.

---

## Folder representation

The scanner returns:

```haskell
type PathNode =
  (FilePath, [FileItem])

type PathFiles =
  Seq PathNode
```

Only directories containing matching files need to contribute a `PathNode`.

This representation is then mapped into the database's folder/file model.

---

# Cannelle PHP representation

The parser result used by `phpparse` is:

```haskell
PhpContext
```

The two members currently most important to persistence are:

```text
logic
contentDemands
```

Conceptually:

```text
PhpContext
 |
 +-- logic
 |     structured PhpAction stream
 |
 `-- contentDemands
       source spans whose original text
       is required by the structural model
```

This is an important optimization.

The typed AST need not carry a complete duplicate of the source text inside
every structural node.

Instead, structure can refer back to requested source fragments.

---

# Source-fragment extraction

Cannelle identifies source regions it needs through:

```haskell
SegmentPos
```

values.

Each segment contains Tree-sitter source coordinates corresponding to:

```text
start row
start column
end row
end column
```

`phpparse` loads the source file, splits it into lines, and extracts the
requested byte ranges.

The extraction logic supports:

```text
single-line ranges
two-line ranges
multi-line ranges
ranges ending at column zero
```

The result is logically:

```text
demand ID -> original source bytes
```

Examples of demanded material may include:

```text
identifiers
literal source
verbatim PHP regions
comments
names
other text retained by the Cannelle representation
```

---

# Constant-pool compaction

Repeated source fragments do not need to be stored repeatedly.

The current implementation groups demanded source fragments by:

```text
MD5(fragment bytes)
```

and constructs a compact constant pool.

Conceptually:

```text
content demands
      |
      v
+------------------+
| "$user"          |
| "example"        |
| "$user"          |
| "example"        |
| "other"          |
+------------------+
      |
      v
MD5 grouping
      |
      v
+------------------+
| constant 0       | -> "$user"
| constant 1       | -> "example"
| constant 2       | -> "other"
+------------------+
      |
      v
old demand IDs -> compact IDs
```

The structural serializer then uses those compact integer IDs.

This reduces repeated text inside the AST representation.

---

## Important MD5 caveat

MD5 is currently used as the **map key** for deduplication.

The implementation does not subsequently compare the original bytes to
confirm that equal MD5 values really represent equal content.

Consequently, a deliberately constructed MD5 collision could cause two
different source fragments to be merged.

For trusted development source this may have been an acceptable prototype
shortcut.

For a durable source-intelligence system that may ingest untrusted PHP,
deduplication should instead use:

```text
hash + byte equality check
```

or an appropriate collision-safe content-addressing mechanism.

MD5 should not be treated as a unique identity.

---

# AST binary representation

`phpparse` currently converts:

```haskell
Vector PhpAction
```

into a compact:

```haskell
ByteString
```

using recursively encoded `Int32` values.

All integers are emitted in:

```text
big-endian 32-bit representation
```

using:

```haskell
putInt32be
```

---

## General structure

The format is essentially a prefix opcode stream.

Conceptually:

```text
[opcode]
[fixed metadata]
[child count where necessary]
[child representation]
[child representation]
...
```

For example, a binary expression is structurally encoded as:

```text
BinaryOp
    |
    +-- operator
    +-- left expression
    `-- right expression
```

and becomes conceptually:

```text
[BinaryOp opcode]
[operator opcode]
[left expression ...]
[right expression ...]
```

A variable-length structure normally includes a count before its children.

---

## Source references

Instead of storing source strings inline, many opcodes contain:

```text
constant-pool ID
```

values.

For example, symbolic names and simple variables can refer to compact
constant IDs produced during source compaction.

---

## Source positions

Some fallback/miscellaneous nodes preserve source locations instead.

These may encode:

```text
start row
start column
end row
end column
```

as integer operands.

This allows unsupported or not-yet-specialized pieces of PHP syntax to
retain a route back to their source region.

---

## Represented PHP structures

The current serializer has explicit handling for a substantial set of PHP
constructs, including forms corresponding to:

```text
blocks
expressions
if statements
foreach
return
echo
function definitions
class definitions
dangling alternative-syntax closures

variables
symbols
binary expressions
unary expressions
ternaries
function calls
arrays
parenthesized expressions
assignments
subscripts
member access
member calls
casts
object creation
include/require
augmented assignment
scope calls
scoped property access
error suppression
lists
heredocs
class constants
shell commands
throw
increment/decrement
cloning
literals

class constants
properties
methods
trait use

qualified names
scope modes
member-access modes
string interpolation
```

This makes the stored representation significantly richer than a flat token
stream.

---

## Incomplete AST encoding

The binary serializer is still under development.

Several PHP constructs currently receive only partial representation or
placeholder data.

Examples visible in the current code include incomplete treatment of:

```text
attributes
member modifiers
extends information
implements information
use-list details
some class-member details
several statement forms
```

Some AST constructors currently emit an opcode without enough associated
data to reconstruct the complete original semantic node.

The present binary format must therefore be treated as an **analysis
prototype**, not a lossless or stable public serialization format.

---

# Constant-pool binary representation

The compact constant pool is stored separately from the AST.

The current binary layout is:

```text
+----------------------------+
| number of constants        | Int32 BE
+----------------------------+
| total payload length       | Int32 BE
+----------------------------+
| length of constant 0       | Int32 BE
+----------------------------+
| length of constant 1       | Int32 BE
+----------------------------+
| ...                        |
+----------------------------+
| concatenated source bytes  |
+----------------------------+
```

If there are `N` constants, the header therefore consists of:

```text
2 + N
```

32-bit integers.

Offsets are not stored explicitly.

They can be reconstructed by summing the preceding constant lengths.

---

## Why separate constants from structure?

For source analysis, structural information often contains highly repeated
text.

Examples include:

```text
variable names
common method names
class names
literal strings
comments
operators or fragments
```

Separating the structural stream from source bytes provides several useful
properties:

```text
smaller structural representation

integer comparisons for repeated symbols

potential constant-level indexing

reuse by later analysis

simpler structural traversal
```

It also creates a natural path toward shared string pools or project-level
interning later.

---

# PostgreSQL persistence

The current database access layer uses:

```text
Hasql
Hasql.TH
Hasql.Pool
postgresql-binary
```

The SQL statements indicate the following logical database model.

---

## `WPVersion`

Stores the imported WordPress/source version:

```text
uid
label
```

Directory ingestion first performs a get-or-create operation on the version
label.

---

## `Folder`

Associates a relative folder with a source version:

```text
uid
versionRef
path
```

---

## `File`

Associates a filename with a folder:

```text
uid
folderRef
path
```

---

## `AST`

Stores the compact PHP structural representation:

```text
fileRef
value bytea
```

---

## `Constant`

Stores the corresponding compact source-fragment pool:

```text
fileRef
value bytea
```

---

## `Error`

Stores parser failures:

```text
fileRef
message
procTime
```

where:

```text
procTime
```

is the measured parser duration when available.

---

## Current hierarchy

The effective source hierarchy is therefore:

```text
WPVersion
    |
    +-- Folder
           |
           +-- File
                  |
                  +-- AST
                  +-- Constant
                  `-- Error
```

This is sufficient for the first source-ingestion use case.

A more general Recycler source store will likely need additional identity,
snapshot, hashing, parse-version, and analysis information.

---

# Parse-error handling

Every file parse is timed using:

```haskell
getCurrentTime
```

before and after:

```haskell
tsParsePhp
```

If parsing fails, `phpparse` stores:

```text
file identity
error text
parse duration
```

in the database.

This is useful for later measuring parser coverage across large legacy
systems.

---

## Successful parse timing

The current implementation computes parse duration for successful parses as
well, but does not persist it.

A future parse-run model should probably retain processing statistics for
both successful and unsuccessful files.

That would permit queries such as:

```text
parser throughput

slowest source files

coverage by PHP/WordPress version

failure rate

representation size per source byte
```

---

# Current data lifecycle

Version, folder, and file records use get-or-create semantics.

For example:

```text
locate version
    |
    +-- found -> reuse
    |
    `-- absent -> insert
```

The same pattern is used for folders and files.

AST and Constant rows are different.

The current implementation performs unconditional:

```text
INSERT INTO AST ...
INSERT INTO Constant ...
```

for every successful import.

As a result, repeated ingestion of the same source tree may produce multiple
AST and Constant rows for one `File` unless constraints outside the code
prevent that.

---

## Partial persistence

AST and constant-pool insertion are also performed as independent database
operations.

The current workflow is therefore capable of reaching states such as:

```text
AST stored
Constant insertion failed
```

or:

```text
object identities created
parse/storage operation failed later
```

A mature importer should define explicit transactional or versioned
semantics for each file parse result.

---

# Source provenance

The current database retains:

```text
source version
folder
filename
```

but does not store a content hash for each imported file.

For durable source intelligence, useful provenance should eventually include:

```text
source-tree identity
Git revision where available
file path
source-content hash
parser version
Cannelle format version
parse timestamp
parse status
AST format version
constant-pool format version
```

This permits a downstream tool to establish exactly which source bytes
produced a particular structural record.

---

# Target architecture

The current architecture can evolve into a more reusable source-intelligence
pipeline.

```text
                      source repository
                             |
                             v
                      snapshot identity
                             |
                             v
                    filesystem inventory
                             |
                             v
                        PHP files
                             |
                             v
                    Cannelle parsing
                             |
                             v
                 semantic PHP structure
                             |
                    canonical packing
                             |
             +---------------+---------------+
             |                               |
             v                               v
       structural data                  text pool
             |                               |
             +---------------+---------------+
                             |
                             v
                      immutable parse
                          artifact
                             |
             +---------------+---------------+
             |               |               |
             v               v               v
          indexes        overlays         viewers
             |               |               |
             +---------------+---------------+
                             |
                             v
                         Recycler
                             |
                             v
                  modernization workflow
```

Several architectural improvements follow from this model.

---

# Stable packed representation

The current opcode format should evolve into a **versioned packed semantic
representation**.

The durable format should eventually provide at least:

```text
magic identifier
format version
language/version metadata
section directory
fixed endianness
structural payload
text/constants section
source-span information
integrity information
```

This prevents a future serializer update from silently changing the meaning
of already stored AST blobs.

---

## Stable opcode specification

Current opcode numbers are implementation details embedded directly in
`Commands.Process`.

For long-lived stored data they should become a documented specification.

For example:

```text
opcode
node type
operand count or framing rule
operand semantics
child semantics
format-version introduction
```

should be explicit.

Changing a Haskell constructor order or refactoring a serializer must not
silently invalidate historical data.

---

# Serialization should become reusable

The current:

```text
convertAST
convertConstants
```

functions live directly inside:

```text
Commands.Process
```

That was reasonable for the initial prototype.

As the representation becomes shared by:

```text
phpparse
Daniell
Recycler
analysis tools
viewers
```

the serialization implementation should move into a reusable Cannelle or
dedicated packed-format module.

A useful conceptual boundary is:

```text
Cannelle.PHP.Serialize
Cannelle.PHP.Deserialize
```

or an equivalent shared package.

Then:

```text
phpparse
```

can remain responsible for project ingestion rather than defining the
canonical binary format itself.

---

# Analysis overlays

A durable AST should remain as close as practical to parsed source facts.

Higher-order analysis should be stored separately.

For example:

```text
base PHP structure
        |
        +------> symbol-resolution overlay
        |
        +------> call-graph overlay
        |
        +------> variable/data-flow overlay
        |
        +------> framework overlay
        |
        +------> WordPress hook overlay
        |
        +------> security-analysis overlay
        |
        `------> Recycler migration overlay
```

This avoids repeatedly rewriting the base AST as analysis becomes richer.

It also enables several analyses to coexist over the same parsed source
snapshot.

---

# Suggested downstream indexes

Once the basic PHP representation is durable, useful derived indexes
include:

```text
functions by qualified name

classes / interfaces / traits / enums

methods and properties

call sites

include / require relationships

constant definitions

global variables

namespace imports

class inheritance

trait use

WordPress hooks

WordPress filters

database accesses

HTTP/request inputs

template-output sites
```

These indexes should be derived artifacts.

They do not need to complicate the packed syntax representation itself.

---

# PHP modernisation workflow

The broader FUDD modernization model is:

```text
1. INVENTORY
   existing PHP project
        |
        v
   source tree + parser coverage


2. STRUCTURE
   PHP source
        |
        v
   Cannelle / phpparse
        |
        v
   durable semantic representation


3. ANALYSIS
   symbols
   calls
   dependencies
   framework conventions
   routes
   templates
   mutable state


4. ARCHITECTURE RECOVERY
   identify application concepts
   and execution boundaries


5. TRANSFORMATION
   PHP structures
        |
        v
   FUDD intermediate representations


6. MATERIALISATION
   Fuddle
   EasyWordy
   Wapp
   Haskell
   compatibility/native boundaries


7. VERIFICATION
   legacy behavior
        vs
   generated behavior
```

`phpparse` primarily occupies stages 1 and 2.

Recycler owns the broader modernization lifecycle.

---

# WordPress specialization

The current application's WordPress heritage is visible in:

```text
wpload command name
WPVersion database table
default source version
version validation
```

WordPress-specific source intelligence is valuable.

WordPress projects contain architectural conventions such as:

```text
actions
filters
shortcodes
template hierarchy
plugin entry points
theme functions
wpdb usage
global state
include conventions
```

However, those concepts should normally be derived **above** the general PHP
syntax layer.

For example:

```text
PHP FunctionCall
       |
       v
callee = add_action
       |
       v
WordPress analysis overlay
       |
       v
hook registration
```

This preserves a reusable PHP parser while still allowing strong
WordPress-specific analysis.

---

# Module map

## Processing

| Module | Responsibility |
| --- | --- |
| `Commands.Process` | Single-file parsing and directory ingestion |
| `Commands.Version` | Package/Git version reporting |
| `Commands.Help` | Current placeholder help implementation |
| `Commands` | Command aggregation |

---

## Filesystem

| Module | Responsibility |
| --- | --- |
| `FileSystem.Explore` | Recursive project discovery |
| `FileSystem.Types` | Path and file-kind representation |

---

## PostgreSQL

| Module | Responsibility |
| --- | --- |
| `DB.Connect` | Hasql pool configuration/lifecycle |
| `DB.Opers` | Source/version/file/AST/constant/error operations |

---

## Configuration

| Module | Responsibility |
| --- | --- |
| `Options.Cli` | Command-line parser |
| `Options.ConfFile` | YAML configuration |
| `Options.Runtime` | Effective runtime options |
| `Options` | Configuration merging |

---

## Application

| Module | Responsibility |
| --- | --- |
| `MainLogic` | Command dispatch |
| `app/Main.hs` | Startup and configuration loading |

---

## Generic/inherited infrastructure

| Module | Current status |
| --- | --- |
| `HttpSup.CorsPolicy` | Not relevant to the current command-line workflow |

Unused generic application-template infrastructure should be removed when it
no longer serves a planned role.

---

# Testing strategy

The current test executable is only:

```haskell
main =
  putStrLn "Test suite not yet implemented"
```

This is the largest immediate engineering weakness in the repository.

Parser tests may already exist inside Cannelle, but `phpparse` itself needs
tests for **ingestion and serialization**.

---

## Single-file integration tests

Given known PHP fixtures:

```text
PHP source
    |
    v
Cannelle
    |
    v
expected PhpContext properties
```

Test representative constructs such as:

```text
functions
classes
interfaces
traits
namespaces
foreach
conditionals
calls
member access
arrays
strings
includes
WordPress idioms
```

---

## Constant-compaction tests

Verify:

```text
duplicate fragments -> one constant

different fragments -> different constants

all original demand IDs obtain a mapping

multi-line fragments retain exact bytes

empty fragments are handled deliberately
```

Add explicit hash-collision handling tests once deduplication is hardened.

---

## Binary AST golden tests

For stable fixture PHP files:

```text
PHP
 |
 v
PhpContext
 |
 v
packed AST
```

compare the byte representation against versioned golden files.

This makes accidental opcode-format changes visible during review.

---

## Binary decoder tests

Once a decoder exists:

```text
PhpContext subset
      |
      v
serialize
      |
      v
deserialize
      |
      v
equivalent structure
```

should become a central invariant.

---

## Constant-format tests

Verify:

```text
constant count
total byte length
individual lengths
concatenated payload
```

including:

```text
zero constants
one constant
large constant
UTF-8 source
arbitrary PHP source bytes
```

---

## Database integration tests

Using a disposable PostgreSQL database, verify:

```text
first version import
repeated version import
folder creation
file creation
successful parse storage
parse-error storage
re-import behavior
transaction failure behavior
```

---

## Project-fixture tests

Maintain small complete PHP projects representing:

```text
plain PHP
WordPress plugin
WordPress theme
small WordPress core subset
namespaced modern PHP
legacy procedural PHP
```

and assert inventory/coverage results.

---

## Scale tests

`phpparse` is intended to process whole source trees.

Benchmark:

```text
files/second
source MiB/second
parse time
serialization time
database time
AST bytes/source byte
constant bytes/source byte
peak memory
```

against realistic PHP projects.

---

# Performance considerations

The current representation contains several sensible performance choices.

---

## Compact integer representation

AST structure is stored as:

```text
Int32 opcodes + operands
```

rather than verbose textual JSON.

This reduces:

```text
storage
deserialization overhead
comparison cost
I/O
```

for large codebases.

---

## Constant interning

Repeated source fragments are stored once per file.

This is particularly useful in source code where names recur frequently.

---

## Binary PostgreSQL storage

Both structural and constant data are stored as:

```text
bytea
```

without expanding every syntax node into relational rows.

That avoids very high row counts during initial source ingestion.

---

## Per-file isolation

Each file is parsed independently.

That creates natural units for:

```text
parallel parsing
incremental re-analysis
cache invalidation
failure isolation
```

even though the current implementation processes files sequentially.

---

# Future parallelism

Directory processing currently uses:

```haskell
mapM_
```

and therefore processes files sequentially.

Once database semantics and parser thread safety are well defined, file
parsing is a natural candidate for bounded parallel execution:

```text
source tree
    |
    +--> parser worker
    +--> parser worker
    +--> parser worker
    `--> parser worker
             |
             v
       persistence queue
```

Concurrency should be bounded by:

```text
CPU
memory
Tree-sitter/Cannelle behavior
PostgreSQL pool size
```

rather than creating one unrestricted thread per file.

---

# Incremental ingestion

A mature implementation should not reparse unchanged source.

A useful file identity can include:

```text
snapshot ID
relative path
content hash
parser format version
```

Then:

```text
same content hash
+
same parser version
        |
        v
reuse existing parse artifact
```

This becomes especially valuable when Recycler repeatedly analyses large
legacy projects after small source modifications.

---

# Security and untrusted source

PHP source being analysed must be treated as untrusted data.

`phpparse` parses source.

It should **never execute the PHP application** merely to understand it.

This is one of the benefits of a Tree-sitter/Cannelle static parsing
pipeline.

---

## Path handling

Directory scanners should continue to avoid source-controlled metadata such
as:

```text
.git
```

and should ultimately define deliberate behavior for:

```text
symbolic links
very deep trees
special files
very large source files
permission failures
```

---

## Parser resource limits

Hostile or pathological source may cause excessive:

```text
CPU
memory
parse-tree size
recursion depth
```

A production source-ingestion service should have explicit resource limits
and file-level failure isolation.

---

## Constant hash collisions

As noted above, the current MD5-only deduplication key is unsafe as a unique
identity for adversarial input.

Do not use the current MD5 mapping as a security boundary.

---

## Database credentials

Do not commit production database passwords into:

```text
~/.fudd/phpparse/config.yaml
```

or project repositories.

Use the FUDD deployment secret-management mechanism appropriate to the
environment.

---

# Development roadmap

## Phase 0 — Stabilize the current importer

Before expanding analysis:

1. implement the `phpparse-test` suite;
2. add fixtures covering current PHP AST serialization;
3. make configuration optional for single-file/version commands where
   possible;
4. correct the CLI's version terminology;
5. remove outdated WordPress-version documentation;
6. make file import transactional;
7. define re-import semantics;
8. persist successful parse metrics; and
9. remove unused generic template modules.

---

## Phase 1 — Freeze packed PHP format v1

Move from implementation-defined serialization to a documented format.

Add:

```text
magic
format version
flags
section offsets
structural section
constant/text section
optional source-span section
integrity/checksum information
```

Publish the opcode table.

---

## Phase 2 — Implement deserialization

A stored AST is only a durable program representation if it can be decoded
reliably.

Provide a shared API conceptually similar to:

```haskell
serializePhp
  :: PhpContext
  -> PackedPhp

deserializePhp
  :: PackedPhp
  -> Either DecodeError PhpContext
```

or an intentionally normalized equivalent.

---

## Phase 3 — Move serialization into shared infrastructure

Extract PHP packing out of:

```text
Commands.Process
```

into reusable Cannelle/source-format modules.

This permits:

```text
phpparse
Daniell
Recycler
test tools
analysis services
```

to use the exact same format implementation.

---

## Phase 4 — Immutable source snapshots

Introduce explicit project/source snapshot identity.

Persist:

```text
project
revision/snapshot
folder
file
content hash
parse artifact
parser version
```

rather than attaching all imports directly to only a WordPress version.

Retain WordPress version as domain metadata where useful.

---

## Phase 5 — Incremental parsing

Compare source-content hashes.

Only parse:

```text
new files
changed files
files requiring parser-format upgrade
```

Reuse immutable parse artifacts for unchanged content.

---

## Phase 6 — Symbol index

Derive project-wide symbols:

```text
functions
classes
interfaces
traits
enums
methods
properties
constants
namespaces
```

with source-span references.

---

## Phase 7 — Relationship graphs

Build derived graphs for:

```text
calls
includes
inheritance
trait use
namespaces/imports
constant references
member access
```

Keep these separate from the base syntax artifact.

---

## Phase 8 — WordPress analysis

Add WordPress-specific overlays:

```text
actions
filters
shortcodes
template relationships
plugin registration
wpdb access
global variables
option access
HTTP/request boundaries
```

This moves `wpload` from a version-labelled PHP importer toward genuine
WordPress architecture recovery.

---

## Phase 9 — Recycler integration

Expose stable project-analysis APIs to Recycler.

Recycler should be able to query:

```text
source node
symbol
call graph
source span
PHP structure
WordPress semantics
analysis status
```

without understanding the packed binary encoding.

---

## Phase 10 — Transformation provenance

For automated modernization, retain mappings such as:

```text
legacy PHP source node
        |
        v
FUDD transformation
        |
        v
generated Fuddle/Haskell/Wapp node
```

This enables:

```text
traceability
side-by-side comparison
human review
behavioral validation
incremental replacement
```

---

# Design principles

## Keep parsing in Cannelle

`phpparse` should consume Cannelle's PHP representation, not establish a
second independent PHP grammar.

---

## Keep source evidence

Optimization must not make it impossible to explain where a conclusion came
from.

Every important semantic object should retain a route to:

```text
file
source span
source snapshot
```

---

## Separate base syntax from analysis

A call graph is not syntax.

A WordPress hook interpretation is not syntax.

A Recycler conversion decision is not syntax.

Store those as overlays or derived artifacts.

---

## Version persistent binary formats

A binary format without a version is not a long-lived persistence contract.

---

## Prefer immutable parse artifacts

A parsed source snapshot should be reproducible evidence.

New analysis should normally create new derived state rather than silently
rewriting the historical source interpretation.

---

## Avoid stringly typed analysis

Once higher-level concepts become stable, represent them with typed
structures rather than collections of ad-hoc text labels.

---

## Treat unknown syntax explicitly

Legacy PHP will contain unusual and old constructions.

Where Cannelle cannot yet give a specialized node, preserve:

```text
source range
raw/fallback representation
diagnostic
```

rather than discarding the construct.

---

## Make automation explainable

Recycler should eventually be able to answer:

```text
Why was this PHP construct converted this way?

Which source node caused this generated component?

Which unresolved feature requires human intervention?
```

`phpparse`'s source-backed representation is part of the evidence chain
required to answer those questions.

---

# Current limitations

## The serialization format is not versioned

There is no magic value or format-version header in the stored AST blob.

---

## The serializer is incomplete

Several PHP AST cases retain placeholder or incomplete operand data.

It is therefore not currently a lossless `PhpContext` representation.

---

## There is no decoder in this repository

The binary format can currently be written but is not exposed here as a
round-trip serialization API.

---

## Deduplication relies solely on MD5 identity

No source-byte equality check follows an MD5 collision.

---

## Re-imports can create repeated AST/Constant rows

Version/folder/file records are reused, while AST and constant records are
inserted anew.

---

## File persistence is not atomic

AST and constant data are inserted through separate database operations.

---

## Successful parse duration is discarded

Duration is calculated but only persisted on error.

---

## There is no content hash

The database cannot currently tell whether an existing `File` record refers
to identical source bytes on a later import.

---

## `--version` terminology is inconsistent

The CLI says:

```text
Version of PHP to parse
```

while the implementation uses a WordPress-version record and defaults to:

```text
6.2.1
```

This should be renamed or clarified.

---

## README version information is stale

The previous README stated:

```text
6.2.0
```

as the default version.

The current code uses:

```text
6.2.1
```

---

## Configuration is required at application startup

`app/Main.hs` attempts to load a YAML configuration before dispatching any
command.

Consequently, even operations that logically need no database configuration
may fail when the default configuration file is absent.

---

## Some configuration fields are ignored

Current merge logic does not apply:

```text
primaryLocale
db.poolSize
db.poolTimeOut
```

from the YAML file.

---

## Files are processed sequentially

There is not yet bounded parallel parsing.

---

## The database schema is WordPress-oriented

`WPVersion` makes sense for the original application but is too narrow as
the sole snapshot identity for generic PHP/Recycler analysis.

---

## No project-level semantic analysis exists

The importer stores per-file structure but does not yet derive:

```text
symbols
calls
dependencies
inheritance
hooks
routes
data flow
```

across the whole application.

---

## Test suite is a placeholder

`stack test` does not currently exercise the importer or serializer.

---

## Repository depends on local packages

Cannelle and Tree-sitter packages are referenced using relative development
paths.

---

# Repository housekeeping

The current package metadata still points to:

```text
hugdro/phpparse
```

rather than:

```text
whatsupfudd/phpparse
```

Update:

```text
github
homepage
bug-reports
source-repository
README URL
```

accordingly.

---

## Package description

The current package description is still the generic:

```text
Please see the README...
```

A more useful synopsis would be:

```text
PHP source ingestion and compact Cannelle AST persistence for FUDD
source-code analysis and application modernization.
```

---

## Warning policy

The project already enables several useful GHC warnings, including:

```text
-Wcompat
-Widentities
-Wincomplete-record-updates
-Wincomplete-uni-patterns
-Wpartial-fields
-Wredundant-constraints
```

Once the current code is stabilized, enabling:

```text
-Wall
```

and incrementally eliminating warnings would be worthwhile.

---

## Changelog

The changelog should begin tracking representation-level changes.

In particular, record milestones such as:

```text
packed PHP format v1
decoder v1
snapshot model
incremental parsing
symbol index
WordPress analysis
Recycler integration
```

Changes to persistent binary representation are especially important and
should never be hidden inside an ordinary implementation release.

---

# Long-term position

`phpparse` should become the PHP-specific **source ingestion front end** for
FUDD's source-intelligence and modernization stack.

The durable division is:

```text
                       PHP application
                             |
                             v
                         phpparse
                             |
                  filesystem + snapshots
                             |
                             v
                         Cannelle
                             |
                    PHP semantic CST/AST
                             |
                             v
                    packed source artifact
                             |
          +------------------+------------------+
          |                  |                  |
          v                  v                  v
       indexes            overlays          viewers
          |                  |                  |
          +------------------+------------------+
                             |
                             v
                         Recycler
                             |
                  architecture recovery
                             |
                             v
                 transformation planning
                             |
                             v
             Fuddle / EasyWordy / Haskell
                             |
                             v
                     behavioral validation
```

The immediate engineering objective should not be to add large amounts of
framework-specific logic directly to `Commands.Process`.

It should be to make the existing PHP source representation **durable and
reusable**:

```text
Cannelle PhpContext
        |
        v
versioned packed format
        |
        v
round-trip decoder
        |
        v
immutable source snapshot
        |
        v
stable analysis API
```

Once that foundation exists, WordPress understanding, project-wide graphs,
security analysis, automatic modernization, and AI-assisted source reasoning
can be built as derived layers without compromising the source evidence on
which they depend.

---

# License

The package declares the **BSD-3-Clause** license.

See the repository's `LICENSE` file for the authoritative license terms.