sbcl = sbcl
bindir = $(HOME)/bin

src = tagdb.asd tagdb.lisp

tagdb: quicklisp/setup.lisp $(src)
	$(sbcl) --script make-image.lisp

quicklisp.lisp:
	wget -O $@ "http://beta.quicklisp.org/quicklisp.lisp"

quicklisp/setup.lisp: quicklisp.lisp
	$(sbcl) --noinform --no-sysinit --no-userinit --non-interactive \
		--load quicklisp.lisp \
		--eval '(require "asdf")' \
		--eval '(asdf:disable-output-translations)' \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'

README.md: tagdb
	@echo "Updating $@"
	@mv -f $@ $@.tmp
	@sed -n -e '0,/^Usage$$/p' $@.tmp > $@
	@echo ----- >> $@
	@echo >> $@
	@./tagdb -h | sed -r '/.+/s/^/    /' >> $@
	@echo >> $@
	@sed -n -e '/^Compile and Install$$/,$$p' $@.tmp >> $@

install:
	install -d -m 755 $(bindir)
	install -m 755 tagdb $(bindir)

clean:
	rm -f tagdb README.md.tmp *.fasl

clean-all: clean
	rm -fr quicklisp
	rm -f quicklisp.lisp

.PHONY: all clean clean-all install
