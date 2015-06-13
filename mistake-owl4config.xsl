<?xml version="1.0" encoding="utf-8"?>
<!-- xsl-stylesheet for generating a config file for standoff-mode from owl.

This stylesheet generates to much of a config. In addition to owl2config.xsl
it generates allowed subjects and objects for predicates from rdfs:domain and
rdfs:range. This really mistakes the sense of these definitions! But it might
server you well.. 

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

Usage: Cf. owl2config.xsl

-->
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:arb="http://beispiel.fernuni-hagen.de/ontologies/beispiel#"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:owl="http://www.w3.org/2002/07/owl#"
    xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
    version="1.0">
  <xsl:output method="text"/>

  <xsl:include href="owl2config.xsl"/>

  <!-- rdfs:domain defines a subject -->
  <xsl:template match="rdfs:domain" mode="standoff-relations-allowed.subject">
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

  
  <xsl:template match="rdfs:range" mode="standoff-relations-allowed.object">
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

</xsl:stylesheet>
