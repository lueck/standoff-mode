<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE rdf:RDF [
    <!ENTITY arb "http://arb.fernuni-hagen.de/owl/beispiel#" >
    <!ENTITY dim "http://arb.fernuni-hagen.de/owl/dimension#" >
    <!ENTITY som "http://github.com/lueck/standoff-mode/owl#" >
]>

<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:som="http://github.com/lueck/standoff-mode/owl#"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:owl="http://www.w3.org/2002/07/owl#"
    xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
    xmlns:arb="http://arb.fernuni-hagen.de/owl/beispiel#"
    xmlns:dim="http://arb.fernuni-hagen.de/owl/dimension#"
    version="1.0">

  <xsl:param name="dimension-comment" select="number('0')"/>
  
  <!-- TODO: can we get that into owl? not using Class -->
  <dim:registry>
    <dim:dimension iri="&dim;Grundpraedikat">
      <dim:label xml:lang="de">Grundprädikate</dim:label>
      <dim:comment xml:lang="de">Von den Grundprädikaten sind alle
      anderen Prädikate abgeleitet.</dim:comment>
    </dim:dimension>
    <dim:dimension iri="&dim;konzeptuell">
      <dim:label xml:lang="de">Konzeptuelle Dimension</dim:label>
      <dim:comment xml:lang="de">Innerhalb der konzeptuellen Dimension
      geht es um den Stellenwert eines Beispiels im Rahmen der
      Darstellung.</dim:comment>
    </dim:dimension>
    <dim:dimension iri="&dim;rhetorisch">
      <dim:label xml:lang="de">Rhetorische Dimension</dim:label>
      <dim:comment xml:lang="de">Wie, in welcher Weise dienen die
      Beispiele dazu, dass der Text sein Ziel erreicht (ohne, dass
      diese Beispiele in epistemologischer und/oder konzeptueller
      Hinsicht eingesetzt werden)? Die rhetorische Dimension im
      engeren Sinne tritt in den Vordergrund, wenn Beispiele dazu
      dienen, dass der Text sein Ziel erreicht, ohne dass sie in
      epistemologischer und/oder konzeptioneller Hinsicht eingesetzt
      werden. Der klassische Fall liegt vor, wenn mit Hilfe von
      Beispielen die Geltung einer Regel unterstellt wird, die aber
      nicht als der eigentliche Gegenstand des Diskurses expliziert,
      sondern eher unterstellt oder insinuiert wird.</dim:comment>
    </dim:dimension>
    <dim:dimension iri="&dim;normativ">
      <dim:label xml:lang="de">Normative Dimension</dim:label>
      <dim:comment xml:lang="de">Welche Werte nimmt das Beispiel
      (direkt oder indirekt) in Anspruch? Das Beispiel spielt in einer
      normativen Dimension von Texten, wenn es entweder indirekt oder
      direkt Werte in Anspruch nimmt, die natürlich sowohl positiv wie
      auch negativ konnotiert sein können. Menschen werden gerne als
      Vorbilder oder als abschreckende Beispiele vorgestellt usw.,
      aber innerhalb der normativen Dimension werden auch Beispiele
      für Kanonisches, Schönes oder Wahres gegeben.</dim:comment>
    </dim:dimension>
    <dim:dimension iri="&dim;epistemologisch">
      <dim:label xml:lang="de">Epistemologische Dimension</dim:label>
      <dim:comment xml:lang="de">Welche Funktion kommt den Beispielen
      im Gang der Argumentation zu, welchen Erkenntniswert haben sie?
      Die epistemologische ist zunächst auf den einzelnen Text und
      dessen Argumentationszusammenhang bezogen.</dim:comment>
    </dim:dimension>
    <dim:dimension iri="&dim;Reproduktion">
      <dim:label xml:lang="de">Reproduktionsdimension</dim:label>
      <dim:comment xml:lang="de">Wie werden mit Beispielen komplexe
      Zusammenhänge im Sinne der Mnemotechnik wie auch im Sinne der
      Verteilung und Verbreitung von Wissen und Handeln reproduzierbar
      gemacht?  Das Beispiel erweist eine Reproduktionsdimension
      sowohl im Sinne der Mnemotechnik als auch der Verteilung, wenn
      mit seiner Hilfe Wissen aktualisiert, vervielfältigt und
      gesichert wird. Sie tritt in einzelnen Texten oder auf der Ebene
      eines Diskurses auf. So können Beispiele komplexere
      Zusammenhänge aus dem Wissen eines Adressaten aufrufen oder auch
      im Sinne der Diskursdeixis an (zuvor) entfaltete Zusammenhänge
      verweisen.</dim:comment>
    </dim:dimension>
    <dim:dimension iri="&dim;diskursiv">
      <dim:label xml:lang="de">Diskursive Dimension</dim:label>
      <dim:comment xml:lang="de">Innerhalb der diskursiven Dimension
      (im engeren Sinne) geht es um Vorkommen, Häufungen, Reihungen
      und Wiederaufnahmen von Beispielen im Text. Einige, aber nicht
      alle dieser Eigenschaften bedürfen der Annotation bzw. sind ihr
      zugänglich.</dim:comment>
    </dim:dimension>
  </dim:registry>

  <xsl:include href="../owl2manual.xsl"/>

  <xsl:template match="/">
    <xsl:call-template name="preamble"/>
    <xsl:call-template name="classes"/>
    <xsl:call-template name="object-property-dimensions"/>
    <xsl:call-template name="closing"/>
  </xsl:template>

  <xsl:template match="/" mode="preamble-addon">
    \renewcommand{\ClassesName}{Klassen (Markup-Typen)}
    \renewcommand{\ObjectPropertiesName}{Prädikate}
    \renewcommand{\AllowedSubjectsName}{Subjekt-Klassen}
    \renewcommand{\AllowedObjectsName}{Objekt-Klassen}
  </xsl:template>
  
  <!-- make a table of object properties -->
  <xsl:template name="object-property-dimensions">
    <!-- $owl to get back to the source document -->
    <xsl:variable name="owl" select="/"/>
    <xsl:text>
    \section{\ObjectPropertiesName}
    \label{sec:object-properties}
    </xsl:text>
    <!--xsl:for-each select="/rdf:RDF/owl:ObjectProperty|rdf:RDF/rdfs:ObjectProperty">
      <xsl:if test="arb:dimension[@rdf:resource = '&dim;rhetorisch']">
	<xsl:call-template name="object-property">
	  <xsl:with-param name="property" select="."/>
	  </xsl:call-template>
	<xsl:text>\hline
	</xsl:text>
      </xsl:if>
    </xsl:for-each-->
    <xsl:for-each select="document('')/xsl:stylesheet/dim:registry/dim:dimension">
      <xsl:variable name="dimension" select="@iri"/>
      <xsl:text>\subsection{</xsl:text>
      <xsl:value-of select="dim:label[@xml:lang = $lang]"/>
      <xsl:text>}
      </xsl:text>
      <xsl:if test="boolean($dimension-comment)">
	<xsl:value-of select="dim:comment[@xml:lang = $lang]"/>
      </xsl:if>
      <xsl:text>
	
      \begin{longtabu} to \linewidth {|l|X|}
      </xsl:text>
      <xsl:for-each select="$owl/rdf:RDF/owl:ObjectProperty|rdf:RDF/rdfs:ObjectProperty">
	<xsl:if test="arb:dimension[@rdf:resource = $dimension]">
	  <!--xsl:value-of select="@rdf:about"/-->
	  <xsl:call-template name="object-property">
	    <xsl:with-param name="property" select="."/>
	  </xsl:call-template>
	  <xsl:text>\hline
	  </xsl:text>
	</xsl:if>
      </xsl:for-each>
      <xsl:text>
      \end{longtabu}
      </xsl:text>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
