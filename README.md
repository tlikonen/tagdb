Tagdb
=====

_Tagdb_ is a simple tag-based command-line database tool for storing
text records. Every record is associated with one or more tags which can
be used to find the records. _Tagdb_ is useful for saving and finding
personal notes quickly.


## Examples

A new record is created with `-c` option and with a list of tags that
will be associated with the record. Normally the default text editor is
launched for editing the record contents but the standard input stream
can be used as well.

    tagdb -c address email John_Smith

_Tagdb's_ default operation prints database records that match the given
tags. For example, the following command prints all records which have
tags containing string `John` and `email`.

    tagdb John email

Records and their tags can be edited with `-e` option. The default text
editor is launched and all matching records can be edited at once.

    tagdb -e John_Smith email


## Usage

    Usage: tagdb [options] [--] TAG ...

      The default operation prints all database records that match the given
      TAG(s).

    General options

      -q    Quiet output.
      -v    Verbose output.

      --db=FILE
            Use FILE as the database file instead of the default
            ~/.config/tagdb.db. The program will try to create all the
            necessary directories for FILE.

      --format=MODE
            Set output format to MODE which can be "text", "text-color"
            or "org-mode" (Emacs). You can add suffix "/default" to MODE
            in which case the specified mode will be saved as the default
            output mode.

    Command options

      -s TAG ...
            Short output. This is like the default operation but only prints
            the first line of records' content. The first line could be used
            as record's title.

      -n TAG ...
            Count and print how many records match the given TAG(s).

      -c TAG ...
            Create a new database record associated with the given tags. If
            there seems to be data coming from the standard input it will be
            saved as the record's content. Otherwise the default text editor
            is launched for editing the record. Empty lines at the beginning
            and end are ignored.

      -e TAG ...
            Find all records that match the given tags and launch the
            default text editor for editing the records' contents and tags.
            Empty lines at the beginning and end of the record content are
            ignored. If a record has empty content the record will be
            deleted from the database.

      -l [STRING]
            List tags that match the given string. If no string is given
            list all tags.

      -r OLD NEW
            Reassociate records. All database records associated with the
            old tag will be associated with the new tag. The old tag is then
            removed.

      -h    Print this help text.


## Compile and Install

The build process is quite straightforward in GNU/Linux distributions.
_Tagdb_ depends on [SBCL][] (a Common Lisp implementation) and
[SQLite][] database back-end. For example, in [Debian GNU/Linux][Debian]
or similar distributions they are in packages `sbcl` and
`libsqlite3-dev`. _Tagdb_ also needs a couple of Common Lisp libraries
but the build process downloads and installs them automatically (with
[Quicklisp][QL]).

Here's the build process:

    $ sudo apt install build-essential git sbcl libsqlite3-dev
    $ git clone https://github.com/tlikonen/tagdb
    $ cd tagdb
    $ make

Command `make install` will install `tagdb` files under `/usr/local`
directories. You can configure move the location with makefile variable
`prefix` or separately executable file directory with `bindir` or
library directory with `libdir`. Variable `sbcl` defines the path for
SBCL. For example: `make sbcl=... prefix=...`. Use the same variables
with build and install.

When the program is installed start with `tagdb -h` command.


[SBCL]:    http://www.sbcl.org/
[SQLite]:  http://www.sqlite.org/
[Debian]:  http://www.debian.org/
[QL]:      http://www.quicklisp.org/


## Copyright and License

Copyright (C) 2014-2019 Teemu Likonen <<tlikonen@iki.fi>>

OpenPGP key: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

The license text: <http://www.gnu.org/licenses/gpl-3.0.html>

The source code repository: <https://github.com/tlikonen/tagdb>

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
