version = 2021
prefix = /usr/local
bindir = $(prefix)/bin
libdir = $(prefix)/lib
sbcl = $(shell which sbcl)
src = src/*.asd src/*.lisp

-include config.mk

all: build/tagdb

build/tagdb: quicklisp/setup.lisp $(src) version.txt
	$(sbcl) --script make.lisp "$(libdir)/tagdb/"

quicklisp/install.lisp:
	mkdir -p quicklisp
	wget -O $@ "http://beta.quicklisp.org/quicklisp.lisp"

quicklisp/setup.lisp: quicklisp/install.lisp
	$(sbcl) --noinform --no-sysinit --no-userinit --non-interactive \
		--load asdf.conf \
		--load quicklisp/install.lisp \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'

config.mk:
	@echo "bindir = $(bindir)" > $@
	@echo "libdir = $(libdir)" >> $@
	@echo "sbcl = $(sbcl)" >> $@
	@cat $@

version.txt:
	if v=$$(git describe --always --dirty); \
		then echo "$$v" > $@; \
		else echo "$(version)" > $@; \
		fi

README.md: build/tagdb build/help.txt
	@echo "Updating $@"
	@mv -f $@ build/readme.tmp
	@sed -n -e '0,/^## Usage$$/p' build/readme.tmp > $@
	@echo >> $@
	@sed -r '/.+/s/^/    /' build/help.txt >> $@
	@echo >> $@
	@sed -n -e '/^## Compile and Install$$/,$$p' build/readme.tmp >> $@

install:
	install -d -m 755 "$(bindir)" "$(libdir)/tagdb"
	install -m 755 build/tagdb "$(bindir)"
	install -m 644 build/src/tagdb.asd "$(libdir)/tagdb"
	install -m 644 build/src/tagdb--all-systems.fasl "$(libdir)/tagdb"

uninstall:
	rm -f -- "$(bindir)/tagdb"
	rm -fr -- "$(libdir)/tagdb"

clean:
	rm -f version.txt
	rm -fr build

distclean: clean
	rm -fr quicklisp
	rm -f config.mk

.PHONY: all clean distclean install uninstall
