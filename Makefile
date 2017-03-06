bindir = $(HOME)/bin
IMAGE = tagdb
SYSTEM = tagdb
MAKEIMG = make-image.lisp
SBCL = sbcl
LISPFILES = tagdb.asd tagdb.lisp

$(IMAGE): $(MAKEIMG) $(LISPFILES)
	@$(SBCL) --script $(MAKEIMG) $(SYSTEM) $(IMAGE)

install: $(IMAGE)
	install -d -- $(bindir)
	install -m 755 -- $(IMAGE) $(bindir)

uninstall:
	rm -f -- $(bindir)/$(IMAGE)

clean:
	rm -f -- $(IMAGE) system-index.txt

clean-quicklisp:
	rm -fr -- quicklisp

clean-all: clean clean-quicklisp

.PHONY: clean clean-quicklisp clean-all install uninstall
