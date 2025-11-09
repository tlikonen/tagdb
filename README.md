Tagdb
=====

_Tagdb_ is a simple tag-based command-line database tool for storing
text records. Every record is associated with one or more tags which can
be used to find the records. _Tagdb_ is useful for saving and finding
personal notes quickly.


## Examples

A new record is created with `-c` option and with a list of tags that
will be associated with the record. The default text editor is launched
for editing the record contents but the standard input stream can be
used as well with option `--cs`.

    tagdb -c address email John_Smith

_Tagdb’s_ default operation prints database records that match the given
tags. For example, the following command prints all records which have
tags containing string `John` and `email`.

    tagdb John email

Records and their tags can be edited with `-e` option. The default text
editor is launched and all matching records can be edited in the same
text editor session.

    tagdb -e John_Smith email

More information about command line arguments are in
[usage.txt](src/usage.txt).


## Compile and Install

_Tagdb_ is written in the [Rust](https://www.rust-lang.org/) programming
language. It works at least in Linux operating systems. For compiling
the source code the usual Rust development environment and tools are
needed. See the Rust web package for more information or install Rust
tools from your operating system’s package repository. File
[Cargo.toml](Cargo.toml) has technical information about the program and
its external requirements.

Usual commands:

  * `cargo build`: compile the source code
  * `cargo run`: (compile and) run the program in the source code
    directory
  * `cargo install --path .`: install to the default location:
    `~/.cargo/bin/tagdb`
  * `cargo clean`: clean all compiled temporary files from the source
    code directory

Simply `cargo install --path .` will download all the needed dependency
packages (crates), compile and install the program. Command `cargo
uninstall` removes the installed binary file.


## Copyright and License

Copyright (C) 2014–2025 Teemu Likonen <<tlikonen@iki.fi>> ([web][],
[PGP][])

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

[web]: http://www.iki.fi/tlikonen/
[PGP]: http://www.iki.fi/tlikonen/teemu.pgp
