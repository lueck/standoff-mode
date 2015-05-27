.PHONY: test compile clean standoff-mode-pkg.el

#VERSION := $(shell git describe --tags)
VERSION := "0.2.2"

ELPKG := standoff-mode.el \
	standoff-api.el \
	standoff-dummy.el \
	standoff-xml.el \
	standoff-relations.el \
	standoff-mode-pkg.el

ELC := $(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	emacs -Q -batch -L `pwd` -f batch-byte-compile $<

package : standoff-mode-$(VERSION).tar

standoff-mode-$(VERSION).tar: ${ELPKG} README.md LICENSE
	tar -cf $@ --transform "s,^,standoff-mode-$(VERSION)/," $^

test:
	@for idx in test/*-test.el; do \
		printf '* %s\n' $$idx ; \
		./$$idx ; \
		[ $$? -ne 0 ] && exit 1 ; \
	done; :

compile: $(ELC)

clean:
	rm *.tar *.elc
