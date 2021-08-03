SHELL := /bin/bash

BASE_DIR ?= .
SOURCE_DIR ?= $(BASE_DIR)/src
MARKUP_DIR ?= $(SOURCE_DIR)
MD5_DIR ?= $(SOURCE_DIR)
RANGES_DIR ?= $(BASE_DIR)/ranges # should contain nothing but ranges and derivates, see clean* rule!
TRIPLES ?= $(BASE_DIR)/data.ttl
LOCAL2PLAIN ?= cat


SOURCE_SUFFIX ?= TEI-P5.xml
SOURCE_DOCS ?= $(shell find $(SOURCE_DIR) -regextype sed -regex ".*\.$(SOURCE_SUFFIX)$$" -type f)

RMLMAPPER_JAR ?= ~/src/rmlmapper-java/target/rmlmapper-*-all.jar

MARKUP := $(shell find $(MARKUP_DIR) -name "*.json" -type f)
MARKUP_RDF := $(patsubst %,%.ttl,$(MARKUP))
MARKUP_RANGES_SH := $(patsubst %.json.ttl,%.ranges.sh,$(MARKUP_RDF))

MD5_DOCS := $(shell find $(MD5_DIR) -regextype sed -regex ".*/[a-fA-F0-9]\{32\}" -type f)
MD5_META := $(patsubst %,%.meta.ttl,$(MD5_DOCS))

RANGES := $(shell find $(RANGES_DIR) -regextype sed -regex ".*/[a-fA-F0-9-]\{36\}" -type f)
RANGES_TXT := $(patsubst %,%.txt,$(RANGES))
RANGES_TCF := $(patsubst %,%.tcf,$(RANGES))

WEBLICHT_URL ?= https://weblicht.sfs.uni-tuebingen.de/WaaS/api/1.0/chain/process
WEBLICHT_CHAIN ?= weblicht/de/chain42891928686544276.xml


all: meta markup_rdf markup_ranges_sh $(TRIPLES)

.PHONY: md5src
md5src:
	$(foreach s,$(SOURCE_DOCS),./cp-sources.sh $(MD5_DIR) $(s);)


%.meta.ttl: %
	./tei2rdf.sh $< > $@

.PHONY: meta
meta: $(MD5_META)


%.json.ttl: %.json
	./som2rdf.sh -o $@ $<

.PHONY: markup_rdf
markup_rdf: $(MARKUP_RDF)


%.ranges.sh: %.json.ttl
	./extract-ranges.sh $(MD5_DIR)/ $(RANGES_DIR)/ $< 2> $@

.PHONY: markup_ranges_sh
markup_ranges_sh: $(MARKUP_RANGES_SH)


%.txt: %
	./plain.sh $< | $(LOCAL2PLAIN) > $@

.PHONY: txt
txt: $(RANGES_TXT)

%.tcf:	%.txt
	weblicht/waas.sh -c $(WEBLICHT_CHAIN) $< > $@ 2> >(tee -a $@.log >&2)

.PHONY: tcf
tcf: $(RANGES_TCF)


$(TRIPLES): $(MARKUP_RDF) $(MD5_META)
	cat $^ > $@

.PHONY: triples
triples: $(TRIPLES)


.PHONY: clean*
clean*:
	rm -Rf $(MD5_META)
	rm -Rf $(MD5_DOCS)
	rm -Rf $(MARKUP_RDF)
	rm -Rf $(MARKUP_RANGES_SH) $(RANGES_DIR)/*
