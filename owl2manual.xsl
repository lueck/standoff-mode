<?xml version="1.0" encoding="utf-8"?>

<!-- xsl-stylesheet for generating a config file for standoff-mode from owl

Copyright (C) 2015 Christian Lück

Author: Christian Lück <christian.lueck@ruhr-uni-bochum.de>
URL: https://github.com/lueck/standoff-mode

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this file. If not, see <http://www.gnu.org/licenses/>.

-->

<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:som="http://github.com/lueck/standoff-mode/owl#"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:owl="http://www.w3.org/2002/07/owl#"
    xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
    version="1.0">

  <xsl:output method="text"/>

  <xsl:param name="lang" select="number('0')"/>

  <!-- set to * for natural width of first column's text -->
  <xsl:param name="firstColWidth" select="'\setlength{6em}'"/>

  <!-- escape " (ascii 34) with that in labels an comments, because
       ascii 34 is an active character in latex -->
  <xsl:param name="escape-quotes" select="'¶'"/>

  <xsl:template match="/">
    <xsl:call-template name="preamble"/>
    <xsl:call-template name="classes"/>
    <xsl:call-template name="object-properties"/>
    <xsl:call-template name="closing"/>
  </xsl:template>

  <xsl:template name="preamble">
    <xsl:text>\documentclass{scrartcl}
    \KOMAoption{fontsize}{10pt}
    \KOMAoption{DIV}{15}
    \KOMAoption{twoside}{false}
    \KOMAoption{headings}{small}
    \KOMAoption{headinclude}{false}
    \KOMAoption{footinclude}{false}
    \usepackage[T1]{fontenc}
    \usepackage[utf8]{inputenc}
    \usepackage[english,ngerman]{babel}
    \usepackage{hyperref}
    \usepackage{url}
    \usepackage{longtable}
    \usepackage{tabu}
    \usepackage{multirow}
    \usepackage{csquotes}
    \usepackage{underscore}%% handles _ as \underscore in text mode
    \MakeOuterQuote{</xsl:text>
    <xsl:value-of select="$escape-quotes"/>
    <xsl:text>}

    \author{}
    %\date{}
    \title{</xsl:text>
    <xsl:call-template name="resource-label">
      <xsl:with-param name="resource" select="/rdf:RDF/owl:Ontology"/>
    </xsl:call-template>
    <xsl:text>}

    \newcommand{\ClassesName}{Classes}
    \newcommand{\ObjectPropertiesName}{Object Properties}
    \newcommand{\AllowedSubjectsName}{Subject classes}
    \newcommand{\AllowedObjectsName}{Object classes}

    \newcommand{\textLabel}[1]{\texttt{#1}}
    
    \urldef{\urlowlequivalentProperty}
    \url{http://www.w3.org/TR/owl-ref/#equivalentProperty-def}
    
    </xsl:text>
    <xsl:apply-templates select="/" mode="preamble-addon"/>
    <xsl:text>
    \begin{document}
    \maketitle
    </xsl:text>
  </xsl:template>

  <xsl:template name="preamble-addon"/>

  <xsl:template name="closing">
    <xsl:text>
    \end{document}
    </xsl:text>
  </xsl:template>

  <!-- make the IRI for a resource -->
  <xsl:template name="resource-iri">
    <xsl:param name="resource"/>
    <xsl:value-of select="translate($resource/@rdf:about, '#_&amp;', '+++')"/>
  </xsl:template>
  
  <!-- make a label (or other xml:lang-aware annotation) -->
  <xsl:template name="resource-label">
    <xsl:param name="resource"/>
    <xsl:param name="annotation" select="'rdfs:label'"/>
    <xsl:choose>
      <xsl:when test="boolean($lang)">
	<xsl:if test="$resource/*[name()=$annotation][@xml:lang = $lang]">
	  <xsl:variable name="text" select="$resource/*[name()=$annotation][@xml:lang = $lang]"/>
	  <xsl:value-of select="translate($text, '&#34;', $escape-quotes)"/>
	</xsl:if>
      </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="$resource/*[name()=$annotation]">
	  <xsl:value-of select="text()"/>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
    
  <!-- make a tex label form a iri, translate tex's active characters
       to colon -->
  <xsl:template name="tex-label-from-iri">
    <xsl:param name="iri"/>
    <xsl:value-of select="translate($iri, '#?&amp;_\', ':::::')"/>
  </xsl:template>

  <xsl:template name="tex-link-from-iri">
    <xsl:param name="iri"/>
    <xsl:text>\hyperlink{</xsl:text>
    <xsl:call-template name="tex-label-from-iri">
      <xsl:with-param name="iri" select="$iri"/>
    </xsl:call-template>
    <xsl:text>}{</xsl:text>
    <xsl:call-template name="resource-label">
      <xsl:with-param name="resource"
		      select="//rdfs:Class[@rdf:about = $iri]|
			      //owl:Class[@rdf:about = $iri]|
			      //rdfs:Property[@rdf:about = $iri]|
			      //owl:ObjectProperty[@rdf:about = $iri]"/>
    </xsl:call-template>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <!-- make a description of a resource -->
  <xsl:template name="resource-description">
    <xsl:param name="resource"/>
    <xsl:param name="rows-appended" select="0"/>
    <xsl:text>\hline
    \multirow{</xsl:text>
    <xsl:value-of select="2+$rows-appended"/>
    <xsl:text>}{</xsl:text>
    <xsl:value-of select="$firstColWidth"/>
    <xsl:text>}{\hypertarget{</xsl:text>
    <xsl:call-template name="tex-label-from-iri">
      <xsl:with-param name="iri" select="@rdf:about"/>
    </xsl:call-template>
    <xsl:text>}{\textLabel{</xsl:text>
    <xsl:call-template name="resource-label">
      <xsl:with-param name="resource" select="$resource"/>
    </xsl:call-template>
    <xsl:text>}}} &amp; \texttt{</xsl:text>
    <xsl:call-template name="resource-iri">
      <xsl:with-param name="resource" select="$resource"/>
    </xsl:call-template>
    <xsl:text>} \\ \cline{2-2}
    &amp; </xsl:text>
    <xsl:call-template name="resource-label">
      <xsl:with-param name="resource" select="$resource"/>
      <xsl:with-param name="annotation" select="'rdfs:comment'"/>
    </xsl:call-template>
    <xsl:text> \\ </xsl:text>
    <xsl:call-template name="description-linking-resources">
      <xsl:with-param name="resource" select="$resource"/>
      <xsl:with-param name="propertyName" select="'rdfs:seeAlso'"/>
      <xsl:with-param name="label" select="'See also'"/>
    </xsl:call-template>
    <!-- add \hline or \cline out of this template -->
  </xsl:template>

  <!-- make a row describing a property of a resource linking to other
       resources -->
  <xsl:template name="description-linking-resources">
    <xsl:param name="resource"/>
    <xsl:param name="propertyName"/>
    <xsl:param name="label" select="$propertyName"/>
    <xsl:if test="$resource/*[name() = $propertyName]">
      <xsl:text>\cline{2-2}
      &amp; </xsl:text>
      <xsl:value-of select="$label"/>
      <xsl:text>: </xsl:text>
      <xsl:for-each select="$resource/*[name() = $propertyName]">
	<xsl:call-template name="tex-link-from-iri">
	  <xsl:with-param name="iri" select="@rdf:resource"/>
	</xsl:call-template>
      </xsl:for-each>
      <xsl:text> \\ </xsl:text>
    </xsl:if>
  </xsl:template>
  
  <!-- make a row describing a property of a resource linking to other
       resources consuming the rdfs:subPropertyOf-->
  <xsl:template name="description-linking-inherited-resources">
    <xsl:param name="resource"/>
    <xsl:param name="propertyName"/>
    <xsl:param name="label" select="$propertyName"/>
    <xsl:if test="$resource/*[name() = $propertyName]">
      <xsl:text>\cline{2-2}
      &amp; </xsl:text>
      <xsl:value-of select="$label"/>
      <xsl:text>: </xsl:text>
      
      <xsl:for-each select="$resource/*[name() = $propertyName]">
	<xsl:call-template name="tex-link-from-iri">
	  <xsl:with-param name="iri" select="@rdf:resource"/>
	</xsl:call-template>
      </xsl:for-each>
      
      <xsl:text> \\ </xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- make a table of classes -->
  <xsl:template name="classes">
    <xsl:text>
    \section{\ClassesName}
    \label{sec:classes}
    \begin{tabu} to \linewidth {|l|X|}
    </xsl:text>
    <xsl:for-each select="/rdf:RDF/owl:Class|/rdf:RDF/rdfs:Class">
      <xsl:call-template name="class">
	<xsl:with-param name="class" select="."/>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:text>
    \end{tabu}
    </xsl:text>
  </xsl:template>
  
  <!-- make a table row for a class -->
  <xsl:template name="class">
    <xsl:param name="class"/>
    <xsl:call-template name="resource-description">
      <xsl:with-param name="resource" select="$class"/>
    </xsl:call-template>
    <xsl:text>\hline
    </xsl:text>
  </xsl:template>

  <!-- make a table of object properties -->
  <xsl:template name="object-properties">
    <xsl:text>
    \section{\ObjectPropertiesName}
    \label{sec:object-properties}
    \begin{longtabu} to \linewidth {|l|X|}
    </xsl:text>
    <xsl:for-each select="/rdf:RDF/owl:ObjectProperty|rdf:RDF/rdfs:ObjectProperty">
      <xsl:call-template name="object-property">
	<xsl:with-param name="property" select="."/>
      </xsl:call-template>
      <xsl:text>\hline
      </xsl:text>
    </xsl:for-each>
    <xsl:text>
    \end{longtabu}
    </xsl:text>
  </xsl:template>

  <!-- make a table row for an object property -->
  <xsl:template name="object-property">
    <xsl:param name="property"/>
    <xsl:call-template name="resource-description">
      <xsl:with-param name="resource" select="$property"/>
    </xsl:call-template>
    <xsl:call-template name="description-linking-resources">
      <xsl:with-param name="resource" select="$property"/>
      <xsl:with-param name="propertyName" select="'owl:equivalentProperty'"/>
      <xsl:with-param name="label" select="'owl:equivalentProperty'"/>
    </xsl:call-template>
    <xsl:call-template name="description-linking-resources">
      <xsl:with-param name="resource" select="$property"/>
      <xsl:with-param name="propertyName" select="'rdfs:subPropertyOf'"/>
      <xsl:with-param name="label" select="'rdfs:subPropertyOf'"/>
    </xsl:call-template>
    <xsl:call-template name="description-linking-resources">
      <xsl:with-param name="resource" select="$property"/>
      <xsl:with-param name="propertyName" select="'owl:inverseOf'"/>
      <xsl:with-param name="label" select="'owl:inverseOf'"/>
    </xsl:call-template>
    <!-- allowed subjects and objects -->
    <xsl:text>\cline{2-2}
    &amp; \begin{tabu}{X||X}
    \AllowedSubjectsName{}: </xsl:text>
    <xsl:apply-templates mode="som-allowedSubject"/>
    <xsl:text> &amp; \AllowedObjectsName{}: </xsl:text>
    <xsl:apply-templates mode="som-allowedObject"/>
    <xsl:text> \\ \end{tabu} \\ </xsl:text>
    <!--xsl:text>\hline
    </xsl:text-->
  </xsl:template>


  <!-- allowed subjects, recurse superproperties -->
  <xsl:template match="som:allowedSubject" mode="som-allowedSubject">
    <xsl:call-template name="tex-link-from-iri">
      <xsl:with-param name="iri" select="@rdf:resource"/>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="rdfs:subPropertyOf" mode="som-allowedSubject">
    <xsl:variable name="resource" select="@rdf:resource"/>
    <xsl:apply-templates
	select="//owl:ObjectProperty[@rdf:about = $resource]"
	mode="som-allowedSubject"/>
  </xsl:template>
  <xsl:template match="@*|text()" mode="som-allowedSubject"/>
  
  <!-- allowed objects, recurse superproperties -->
  <xsl:template match="som:allowedObject" mode="som-allowedObject">
    <xsl:call-template name="tex-link-from-iri">
      <xsl:with-param name="iri" select="@rdf:resource"/>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="rdfs:subPropertyOf" mode="som-allowedObject">
    <xsl:variable name="resource" select="@rdf:resource"/>
    <xsl:apply-templates
	select="//owl:ObjectProperty[@rdf:about = $resource]"
	mode="som-allowedObject"/>
  </xsl:template>
  <xsl:template match="@*|text()" mode="som-allowedObject"/>
  
</xsl:stylesheet>
