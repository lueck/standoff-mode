standoff-arb-schema.el: arb.owl ../owl2config.xsl
	xsltproc \
	--output standoff-arb-schema.el \
	--param config-file \"standoff-arb-schema.el\" \
	--param lang \"de\" \
	--param provide \"true\" \
	../owl2config.xsl \
	arb.owl

arb-schema.tex: arb.owl ../owl2manual.xsl owl2manual.xsl
	xsltproc \
	-output arb-schema.tex \
	-param lang \"de\" \
	-param dimension-comment \"true\" \
	owl2manual.xsl \
	arb.owl

arb-schema.pdf: arb-schema.tex
	pdflatex arb-schema
