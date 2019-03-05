sbcl = sbcl
bindir = $(HOME)/bin

src = tagdb.asd tagdb.lisp

all: tagdb README.md

tagdb: quicklisp/setup.lisp $(src)
	$(sbcl) --script make-image.lisp

quicklisp.lisp:
	wget -O $@ "http://beta.quicklisp.org/quicklisp.lisp"

quicklisp/setup.lisp: quicklisp.lisp
	$(sbcl) --noinform --no-sysinit --no-userinit --non-interactive \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'

README.md: README.template tagdb
	sed '/^###USAGE###/,$$d' $< >$@
	./tagdb -h | sed -r '/.+/s/^/    /' >>$@
	sed '1,/^###USAGE###/d' $< >>$@

install:
	install -d -m 755 $(bindir)
	install -m 755 tagdb $(bindir)

clean:
	rm -f tagdb

clean-all: clean
	rm -fr quicklisp
	rm -f quicklisp.lisp

.PHONY: all clean clean-all install
