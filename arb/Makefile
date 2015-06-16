standoff-arb-schema.el: arb.owl ../owl2config.xsl
	xalan -xsl ../owl2config.xsl \
	-in arb.owl \
	-out standoff-arb-schema.el \
	-param config-file \"standoff-arb-schema.el\" \
	-param lang \"de\" \
	-param provide \"true\"

arb-schema.tex: arb.owl ../owl2manual.xsl owl2manual.xsl
	xalan -xsl owl2manual.xsl \
	-in arb.owl \
	-out arb-schema.tex \
	-param lang \"de\" \
	-param dimension-comment \"true\"

arb-schema.pdf: arb-schema.tex
	pdflatex arb-schema
