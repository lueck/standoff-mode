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

DOC_EN := standoff-en.info \
	standoff-en/index.html \
	standoff-en.pdf

# Note: The manual for the ``Praktikum'' must be generated before the
# regular manual, because the praktikum manual overwrites the files
# for regular manual as temporary files
DOC_DE := standoff-praktikum.pdf \
	standoff-praktikum/index.html \
	standoff-de.info \
	standoff-de/index.html \
	standoff-de.pdf

RM_DIRS := standoff-en standoff-de standoff-praktikum

DOC_TMP := standoff-de\.{aux,cp,dvi,fn,ky,log,pg,tmp,toc,tp,vr}

ELC := $(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	emacs -Q -batch -L `pwd` -f batch-byte-compile $<

doc-en : ${DOC_EN}

standoff-en.info : standoff-en.texi version.texi
	makeinfo standoff-en.texi

standoff-en.pdf : standoff-en.texi version.texi
	texi2pdf standoff-en.texi

standoff-en/index.html : standoff-en.texi version.texi
	makeinfo --html standoff-en.texi

doc-de : ${DOC_DE}

standoff-de.info : standoff-de.texi version.texi
	makeinfo standoff-de.texi

standoff-de.pdf : standoff-de.texi version.texi
	texi2pdf standoff-de.texi

standoff-de/index.html : standoff-de.texi version.texi
	makeinfo --html standoff-de.texi

standoff-praktikum.pdf : standoff-de.texi version.texi
	texi2pdf -o standoff-praktikum.pdf -t "@set PRAKTIKUM" standoff-de.texi

standoff-praktikum/index.html : standoff-de.texi version.texi
	makeinfo --html -D PRAKTIKUM -o standoff-praktikum standoff-de.texi

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
	rm *.tar *.elc ${DOC_TMP} ${DOC_DE}; \
	rm -Rf ${RM_DIRS}
