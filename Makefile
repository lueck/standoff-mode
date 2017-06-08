.PHONY: test compile clean

#VERSION := $(shell git describe --tags)
VERSION := "0.4.2"

ELPKG := standoff-api.el \
	standoff-dummy.el \
	standoff-log.el \
	standoff-util.el \
	standoff-mark.el \
	standoff-json.el \
	standoff-json-file.el \
	standoff-xml.el \
	standoff-relations.el \
	standoff-mode.el \
	standoff-mode-pkg.el

DOC_HTML := docs/en/index.html docs/arb/index.html docs/de/index.html

DOC_INFO := standoff-en.info standoff-de.info

DOC_PDF := standoff-en.pdf standoff-arb.pdf standoff-de.pdf

# Note: The manual for the ``Praktikum'' must be generated before the
# regular manual, because the praktikum manual overwrites the files
# for regular manual as temporary files

RM_DIRS := 

DOC_TMP := standoff-{en,de}\.{aux,cp,dvi,fn,ky,log,pg,tmp,toc,tp,vr,fns,vrs,kys}

ELC := $(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	emacs -Q -batch -L `pwd` -f batch-byte-compile $<

html : ${DOC_HTML}

info : ${DOC_INFO}

pdf : ${DOC_PDF}

standoff-en.info : standoff-en.texi version.texi
	makeinfo standoff-en.texi

standoff-en.pdf : standoff-en.texi version.texi
	texi2pdf standoff-en.texi

docs/en/index.html : standoff-en.texi version.texi
	makeinfo --html --output=docs/en/ standoff-en.texi

standoff-de.info : standoff-de.texi version.texi
	makeinfo standoff-de.texi

standoff-de.pdf : standoff-de.texi version.texi
	texi2pdf standoff-de.texi

docs/de/index.html : standoff-de.texi version.texi
	makeinfo --html --output=docs/de/ standoff-de.texi

standoff-praktikum.pdf : standoff-de.texi version.texi
	texi2pdf -o standoff-praktikum.pdf -t "@set PRAKTIKUM" standoff-de.texi

docs/arb/index.html : standoff-de.texi version.texi
	makeinfo --html -D PRAKTIKUM --output=docs/arb standoff-de.texi

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
	rm *.tar *.elc ${DOC_TMP} ${DOC_INFO} ${DOC_PDF}; \
	rm -Rf ${RM_DIRS}
