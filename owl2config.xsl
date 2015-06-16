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

Usage:

with xalan on bash:

bash$ xalan -xsl owl2config.xsl -in path/to/owl-file -out myconfig.el [ -param config-file \"myconfig.el\"] [-param provide \"true\" ] [-param lang \"nl\"]

This will generate markup types and relation predicates from the ontology.

It will read owl:AnnotationProperty som:allowedSubject and som:allowedObject
in order to restrict predicates to a combination of subject and object classes.

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

  <xsl:param name="config-file">
    <xsl:text>standoff-config.el</xsl:text>
  </xsl:param>

  <xsl:param name="lang" select="number('0')"/>

  <xsl:param name="provide" select="number('0')"/>
  
  <xsl:template match="text()"/>
  
  <xsl:template match="/">
    <xsl:text>;;; </xsl:text>
    <xsl:value-of select="$config-file"/>
    <xsl:text> --- autogenerated configuration for standoff-mode
    &#10;;; This file was generated using owl2config.xsl&#10;
    &#10;;;; Code:&#10;
    </xsl:text>

    <xsl:if test="boolean($provide)">
      <xsl:text>&#10;(provide '</xsl:text>
      <xsl:value-of select="substring($config-file, 1, string-length($config-file)-3)"/>
      <xsl:text>)&#10;&#10;</xsl:text>
    </xsl:if>

    <!-- markup types -->
    <xsl:text>&#10;(setq standoff-markup-types-allowed '(</xsl:text>
    <xsl:apply-templates select="*" mode="standoff-markup-types-allowed"/>
    <xsl:text>&#10;  ))&#10;</xsl:text>

    <!-- labels for markup types -->
    <xsl:text>&#10;(setq standoff-markup-labels '(</xsl:text>
    <xsl:apply-templates select="*" mode="standoff-markup-labels"/>
    <xsl:text>&#10;  ))&#10;</xsl:text>

    <!-- relations -->
    <xsl:text>&#10;(setq standoff-relations-allowed '(</xsl:text>
    <xsl:apply-templates select="*" mode="standoff-relations-allowed"/>
    <xsl:text>&#10;  ))&#10;</xsl:text>

    <!-- labels for predicates -->
    <xsl:text>&#10;(setq standoff-predicate-labels '(</xsl:text>
    <xsl:apply-templates select="*" mode="standoff-predicate-labels"/>
    <xsl:text>&#10;  ))&#10;</xsl:text>

    <!-- faces for types -->
    <xsl:text>&#10;(setq standoff-markup-overlays '(</xsl:text>
    <xsl:apply-templates select="*" mode="standoff-markup-overlays"/>
    <xsl:text>)&#10;      standoff-markup-overlays-front '(</xsl:text>
    <xsl:apply-templates select="*" mode="standoff-markup-overlays"/>
    <xsl:text>)&#10;      standoff-markup-overlays-after '(</xsl:text>
    <xsl:apply-templates select="*" mode="standoff-markup-overlays"/>
    <xsl:text>&#10;  ))&#10;</xsl:text>

    <xsl:text>&#10;;;; </xsl:text>
    <xsl:value-of select="$config-file"/>
    <xsl:text> ends here.&#10;</xsl:text>
  </xsl:template>

  
  <!-- templates for standoff-markup-types-allowed -->
  
  <xsl:template match="owl:Class" mode="standoff-markup-types-allowed">
    <xsl:text>&#10;  "</xsl:text>
    <xsl:value-of select="@rdf:about"/>
    <xsl:text>"</xsl:text>
  </xsl:template>

  <xsl:template match="text()|@*" mode="standoff-markup-types-allowed"/>


  <!-- templates for standoff-markup-labels -->
  
  <xsl:template match="owl:Class" mode="standoff-markup-labels">
    <xsl:text>&#10;  ("</xsl:text>
    <xsl:value-of select="@rdf:about"/>
    <xsl:text>" . "</xsl:text>
    <!--xsl:value-of select="rdfs:label"/-->
    <xsl:apply-templates select="." mode="label"/>
    <xsl:text>")</xsl:text>
  </xsl:template>

  <xsl:template match="text()|@*" mode="standoff-markup-labels"/>


  <!-- label in language if lang parameter used, otherwise all labels -->

  <xsl:template match="rdfs:label" mode="label">
    <xsl:choose>
      <xsl:when test="boolean($lang)">
	<xsl:if test="@xml:lang = $lang">
	  <xsl:value-of select="."/>
	</xsl:if>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="text()|@*" mode="label"/>


  <!-- templates for standoff-relations-allowed -->

  <xsl:template match="owl:ObjectProperty" mode="standoff-relations-allowed">
    <xsl:text>&#10;  ((</xsl:text>
    <xsl:apply-templates mode="standoff-relations-allowed.subject"/>
    <xsl:text>) "</xsl:text>
    <xsl:value-of select="@rdf:about"/>
    <xsl:text>" (</xsl:text>
    <xsl:apply-templates mode="standoff-relations-allowed.object"/>
    <xsl:text>))</xsl:text>
  </xsl:template>

  <xsl:template match="text()|@*" mode="standoff-relations-allowed"/>

  <xsl:template match="text()|@*" mode="standoff-relations-allowed.subject"/>

  <!-- som:allowedSubject defines a subject -->
  <xsl:template match="som:allowedSubject" mode="standoff-relations-allowed.subject">
    <xsl:text>"</xsl:text>
    <xsl:value-of select="@rdf:resource"/>
    <xsl:text>" </xsl:text>
  </xsl:template>

  <!-- when it is a subPropertyOf we need to recurse the super-property -->
  <xsl:template match="rdfs:subPropertyOf" mode="standoff-relations-allowed.subject">
    <xsl:variable name="resource" select="@rdf:resource"/>
    <!--xsl:text>subPropertyOf: </xsl:text><xsl:value-of select="$resource"/-->
    <xsl:apply-templates
	select="../../owl:ObjectProperty[@rdf:about=$resource]"
	mode="standoff-relations-allowed.subject"/>
  </xsl:template>


  <xsl:template match="text()|@*" mode="standoff-relations-allowed.object"/>

  <xsl:template match="som:allowedObject" mode="standoff-relations-allowed.object">
    <xsl:text>"</xsl:text>
    <xsl:value-of select="@rdf:resource"/>
    <xsl:text>" </xsl:text>
  </xsl:template>

  <xsl:template match="rdfs:subPropertyOf" mode="standoff-relations-allowed.object">
    <xsl:variable name="resource" select="@rdf:resource"/>
    <!--xsl:text>subPropertyOf: </xsl:text><xsl:value-of select="$resource"/-->
    <xsl:apply-templates
	select="../../owl:ObjectProperty[@rdf:about=$resource]"
	mode="standoff-relations-allowed.object"/>
  </xsl:template>


  <!-- templates for standoff-predicate-labels -->

  <xsl:template match="owl:ObjectProperty" mode="standoff-predicate-labels">
    <xsl:text>&#10;  ("</xsl:text>
    <xsl:value-of select="@rdf:about"/>
    <xsl:text>" . "</xsl:text>
    <xsl:apply-templates select="." mode="label"/>
    <xsl:text>")</xsl:text>
  </xsl:template>
  
  <xsl:template match="text()|@*" mode="standoff-predicate-labels"/>


  <!-- templates for standoff-markup-overlays -->

  <xsl:template match="owl:Class|rdfs:Class" mode="standoff-markup-overlays">
    <xsl:if test="som:face">
      <xsl:text>&#10;  ("</xsl:text>
      <xsl:value-of select="@rdf:about"/>
      <xsl:text>" ('face '</xsl:text>
      <xsl:value-of select="som:face"/>
      <xsl:text>))</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="text()|@*" mode="standoff-markup-overlays"/>
  
</xsl:stylesheet>
