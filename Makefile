sbcl = sbcl
bindir = $(HOME)/bin

src = tagdb.asd tagdb.lisp
src-ql = $(patsubst %,quicklisp/local-projects/%,$(src))

all: tagdb README.md

tagdb: quicklisp/setup.lisp $(src-ql)
	$(sbcl) --script make-image.lisp

$(src-ql): quicklisp/local-projects/%: %
	cp $< $@

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
