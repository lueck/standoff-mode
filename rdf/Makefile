SOURCE_DIR ?= src

SOMS := $(shell find $(SOURCE_DIR) -name "*.json" -type f)
SOURCES := $(patsubst %.json,%,$(SOMS))

MD5_DIR ?= src
MD5_DOCS := $(shell find $(MD5_DIR) -regextype sed -regex ".*/[a-fA-F0-9]\{32\}" -type f)
MD5_META := $(patsubst %,%.meta.ttl,$(MD5_DOCS))


all:	$(MD5_META)

.PHONY: md5src
md5src:
	$(foreach s,$(SOURCES),./cp-sources.sh $(MD5_DIR) $(s);)


%.meta.ttl: %
	./tei2rdf.sh $< > $@



clean*:
	rm -Rf $(MD5_META)
	rm -Rf $(MD5_DOCS)
