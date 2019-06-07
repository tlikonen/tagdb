prefix = /usr/local
bindir = $(prefix)/bin
libdir = $(prefix)/lib
sbcl = /usr/bin/sbcl
src = tagdb.asd tagdb.lisp pathconv.lisp

-include config.mk

all: build/tagdb

build/tagdb: quicklisp/setup.lisp $(src)
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
	@echo "Write $@"

README.md: build/tagdb
	@echo "Updating $@"
	@mv -f $@ $@.tmp
	@sed -n -e '0,/^## Usage$$/p' $@.tmp > $@
	@echo >> $@
	@sed -r '/.+/s/^/    /' build/help.txt >> $@
	@echo >> $@
	@sed -n -e '/^## Compile and Install$$/,$$p' $@.tmp >> $@

install:
	install -d -m 755 "$(bindir)" "$(libdir)/tagdb"
	install -m 755 build/tagdb "$(bindir)"
	install -m 644 build/tagdb.asd "$(libdir)/tagdb"
	install -m 644 build/tagdb--all-systems.fasl "$(libdir)/tagdb"

uninstall:
	rm -f -- "$(bindir)/tagdb"
	rm -fr -- "$(libdir)/tagdb"

clean:
	rm -fr build
	rm -f README.md.tmp

distclean: clean
	rm -fr quicklisp
	rm -f config.mk

.PHONY: all clean distclean install uninstall
