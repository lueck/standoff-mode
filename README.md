[![License GPLv3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

[![MELPA](https://melpa.org/packages/standoff-mode-badge.svg)](https://melpa.org/#/standoff-mode)

# standoff-mode #

`standoff-mode` turns [GNU Emacs](http://www.gnu.org/software/emacs/)
into a tagger and lets you create (semantic) annotations on texts
in a stand-off manner. It is written for use in the field of digital
humanities and the manual annotation of training data for named-entity
recognition.

There are several tools for creating stand-off markup. Most of them
need to be deployed on a server in a network environment, which may be
a barrier. In contrast `standoff-mode` does not need a networking
environment. It wants to enable one to get hands on annotating texts
right away. It produces a local file with external markup but
connecting a relational database is on the roadmap of development.

Your annotation data don't get locked in with `standoff-mode`. There
is a commandline program that internalizes your external markup into
the source document again:
[standoff-tools](https://github.com/lueck/standoff-tools).

`standoff-mode` doesn't want to integrate everything under one
cover. It's just a tagger, a tool for the manual annotation of
texts. Statistical analysis etc. must be done by another tool.

Since it was written for the field of digital humanities, literature
studies in particular, `standoff-mode` works not only with plain text
input (source) files, but also with XML. So semantic stand-off markup
produced with it may reference structural markup coded in TEI/P5,
which may be of advantage for further processing.

## Stand-off Markup ##

Stand-off markup is also known as external markup and means:

- Stand-off markup refers to a source document by some kind of
  pointers. `standoff-mode` uses character offsets.

- It is contained in an external document (or a database).

- The source document is left unchanged and may be read-only.

- The source document may contain markup too, called internal
  markup. Stand-Off Mode facilitates reading of XML source documents
  by hiding tags and showing glyphs for character references.

Cf. the
[TEI/P5 guidelines on stand-off markup](http://www.tei-c.org/release/doc/tei-p5-doc/de/html/SA.html#SASO)
and the [OpenAnnotation](http://www.openannotation.org/spec/core/)
ontology.

## Features of `standoff-mode`##

- allows discontinuous markup

- allows relations between markup elements (RDF-like directed graphs)

- allows attributes on markup elements

- allows text comments anchored on one or several markup elements

- generate config for your annotation schema from OWL by XSLT

- allows to customize the restrictiveness of the annotations, either
  to the annotation schema plugged in via config (apriori), or the
  schema already used (a posteriori), or free

- offers completion of user input of markup types, relation predicates
  and attribute names

- hide the fdq-names (IRIs) of markup types, predicates and attributes
  behind labels (from OWL or RDFS), customizable

- customization of highlighting faces

- everything can be done with the keyboard an key-codes, no need to
  use the mouse

- several pluggable back-ends (under development)

- manual based on GNU Texinfo, English (under development) and German

- makes XML-files readable for humans by optionally hiding XML-tags
  and by substituting character references with glyphs. (Note:
  Switching between hide/show is a computationally expensive task for
  Emacs and may consume a minute of your time if you're working on a
  file with some hundred kBytes.)

## Roadmap ##

`standoff-mode` is under active development. Here's the roadmap:

- text comments

- SQL back-end

## Requirements ##

Only [GNU Emacs](http://www.gnu.org/software/emacs/#Obtaining) is
required. After the installation of the editor the `standoff-mode`
package has to be installed. It was tested on Windows, Linux and Mac,
with versions 24.3 and 24.5.

If you want to store your markup in SQL-tables or as RDF-triples, a
RDBMS or a SPARQL-endpoint is required.

## Installation ##

`standoff-mode` is on the Milkypostman's Emacs Lisp Package Archive
(MELPA), what makes it very easy to install.

If MELPA is not already in your list of package repositories add the
following line to your init file:

	(add-to-list 'package-archives
	             '("melpa" . "https://melpa.org/packages/"))

Then make the packages from melpa available and call `package-install`:

	M-x package-refresh-contents RET
	M-x package-install RET standoff-mode RET

Alternatively you can use the menu for installing packages:

	M-x package-list-packages RET

Then search `standoff-mode`, use `i` to mark it for installation and
`x` to install the marked package(s).

For configuration und usage see the info files (currently german
only):

	C-h i m stand-off RET

## Documentation

There is a manual in german language which is installed with the
package und can be viewed with Emacs' info-browser. Type

	C-h i m standoff RET

An english manual will follow soon.

The same documentation is available as
PDF
[here](http://beispiel.germanistik.rub.de/@@/doc/standoff-de.pdf). There
is also a
more
[verbose version](http://beispiel.germanistik.rub.de/@@/doc/standoff-praktikum.pdf) of
it, which addresses students of Fern-University of Hagen, Germany. And
there
are
[installation instructions](http://beispiel.germanistik.rub.de/@@/doc/Emacs-Installation.pdf) for
both, GNU Emacs and standoff-mode.


## License ##

GPL 3

<!--  LocalWords:  SQL RDF SPARQL OpenAnnotation roadmap TEI glyphs
 -->
<!--  LocalWords:  SASO config XSLT apriori posteriori fdq IRIs RDFS
 -->
<!--  LocalWords:  Texinfo RDBMS
 -->
