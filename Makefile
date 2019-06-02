sbcl = sbcl
bindir = $(HOME)/bin
src = tagdb.asd tagdb.lisp pathconv.lisp

tagdb: quicklisp/setup.lisp $(src)
	$(sbcl) --script make-quickload.lisp
	$(sbcl) --script make-image.lisp

quicklisp/install.lisp:
	mkdir -p quicklisp
	wget -O $@ "http://beta.quicklisp.org/quicklisp.lisp"

quicklisp/setup.lisp: quicklisp/install.lisp
	$(sbcl) --noinform --no-sysinit --no-userinit --non-interactive \
		--load quicklisp/install.lisp \
		--eval '(require "asdf")' \
		--eval '(asdf:disable-output-translations)' \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'

README.md: tagdb
	@echo "Updating $@"
	@mv -f $@ $@.tmp
	@sed -n -e '0,/^## Usage$$/p' $@.tmp > $@
	@echo >> $@
	@./tagdb -h | sed -r '/.+/s/^/    /' >> $@
	@echo >> $@
	@sed -n -e '/^## Compile and Install$$/,$$p' $@.tmp >> $@

install:
	install -d -m 755 $(bindir)
	install -m 755 tagdb $(bindir)

clean:
	rm -f tagdb README.md.tmp *.fasl

clean-all: clean
	rm -fr quicklisp

.PHONY: all clean clean-all install
