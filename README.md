Tagdb
=====

**Simple tag-based command-line database tool**


Introduction
------------

_Tagdb_ is a simple tag-based command-line database tool which can store
any kind of text records. Every record is associated with one or more
tags which can be used to find the records. _Tagdb_ is useful for saving
and finding personal information.


Examples
--------

A new record is created with `-c` option and with a list of tags that
will be associated with the record. Normally the default text editor is
launched for editing the record contents but the standard input stream
can be used as well.

    tagdb -c address email John_Smith

_Tagdb's_ default operation prints database records that match the given
tags. For example, the following command prints all records with tags
`John_Smith` and `email`.

    tagdb John_Smith email

Records and their tags can be edited with `-e` option. The default text
editor is launched and all matching records can be edited at once.

    tagdb -e John_Smith email

Currently used tags can be printed with `-l` option and more help on
command-line usage is printed with `-h` option.


Build and install
-----------------

The build process is fairly straightforward in GNU/Linux distributions.
_Tagdb_ depends on [SBCL][] (a Common Lisp implementation) and
[SQLite][] database back-end. For example, in [Debian GNU/Linux][Debian]
or similar distributions they are in packages `sbcl` and
`libsqlite3-dev`. _Tagdb_ also needs a couple of Common Lisp libraries
but the build process downloads and installs them automatically (with
[Quicklisp][QL]).

Here's the build and install process in short:

    $ sudo aptitude install sbcl libsqlite3-dev git
    $ git clone https://github.com/tlikonen/tagdb
    $ cd tagdb
    $ make
    $ make install          # Default target: ~/bin/tagdb


[SBCL]:    http://www.sbcl.org/
[SQLite]:  http://www.sqlite.org/
[Debian]:  http://www.debian.org/
[QL]:      http://www.quicklisp.org/


The source code repository
--------------------------

Github repository URL: <https://github.com/tlikonen/tagdb>

The branch named _master_ is the release branch and it should always be
safe to use. New features and experimental code are developed in other
branches and possibly merged to _master_ when they are ready.


Copyright and license
---------------------

Copyright (C) 2014 Teemu Likonen <<tlikonen@iki.fi>>

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

The license text: <http://www.gnu.org/licenses/gpl-3.0.html>
