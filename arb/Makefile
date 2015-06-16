standoff-arb-schema.el: arb.owl
	xalan -xsl ../owl2config.xsl \
	-in arb.owl \
	-out standoff-arb-schema.el \
	-param config-file \"standoff-arb-schema.el\" \
	-param lang \"de\" \
	-param provide \"true\"

arb-schema.tex: arb.owl
	xalan -xsl ../owl2manual.xsl \
	-in arb.owl \
	-out arb-schema.tex
	-param lang \"de\"

arb-schema.pdf: arb-schema.tex
	pdflatex arb-schema
