<?xml version="1.0" encoding="utf-8"?>

<!-- Program name: gen-schematron.xsl

Copyright © 2009 by Ladislav Lhotka, CESNET <lhotka@cesnet.cz>

Creates standalone Schematron schema from conceptual tree schema.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-->
<!DOCTYPE xsl:stylesheet [
<!ENTITY std-annot
"nma:must|@nma:key|@nma:unique|@nma:max-elements|@nma:when">
<!ENTITY std-todo "descendant::rng:ref|descendant::rng:element
[nma:must or @nma:key or @nma:unique or @nma:max-elements or @nma:when]">
<!ENTITY refint-annot "@nma:leafref">
<!ENTITY refint-todo "descendant::rng:ref|descendant::rng:element
[@nma:leafref]">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                xmlns:nma="urn:ietf:params:xml:ns:netmod:dsdl-annotations:1"
                version="1.0">

  <xsl:include href="gen-common.xsl"/>

  <!-- Namespace URIs -->
  <xsl:param name="rng-uri">http://relaxng.org/ns/structure/1.0</xsl:param>
  <xsl:param
      name="nmt-uri">urn:ietf:params:xml:ns:netmod:conceptual-tree:1</xsl:param>
  <xsl:param
      name="dtdc-uri">http://relaxng.org/ns/compatibility/annotations/1.0</xsl:param>
  <xsl:param name="dc-uri">http://purl.org/dc/terms</xsl:param>
  <xsl:param
      name="nma-uri">urn:ietf:params:xml:ns:netmod:dsdl-annotations:1</xsl:param>
  <xsl:param name="nc-uri">urn:ietf:params:xml:ns:netconf:base:1.0</xsl:param>
  <xsl:param name="en-uri">urn:ietf:params:xml:ns:netconf:notification:1.0</xsl:param>

  <xsl:template name="assert-element">
    <xsl:param name="test"/>
    <xsl:param name="message">
      <xsl:value-of
          select="concat('Condition &quot;', $test, '&quot; must be true')"/>
    </xsl:param>
    <xsl:element name="sch:assert">
      <xsl:attribute name="test">
        <xsl:value-of select="$test"/>
      </xsl:attribute>
      <xsl:value-of select="$message"/>
    </xsl:element>
  </xsl:template>

  <xsl:template name="netconf-part">
    <xsl:choose>
      <xsl:when
          test="$target='get-reply' or
                $target='getconf-reply'">/nc:rpc-reply/nc:data</xsl:when>
      <xsl:when test="$target='rpc'">/nc:rpc</xsl:when>
      <xsl:when test="$target='notif'">/en:notification</xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="nc-namespace">
    <xsl:choose>
      <xsl:when test="$target='get-reply' or $target='getconf-reply'
                      or $target='rpc'">
          <sch:ns uri="{$nc-uri}" prefix="nc"/>
      </xsl:when>
      <xsl:when test="$target='notif'">
          <sch:ns uri="{$en-uri}" prefix="en"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="append-path">
    <!-- Concat $start and XPath of the context element in the data tree -->
    <xsl:param name="start">
      <xsl:call-template name="netconf-part"/>
    </xsl:param>
    <xsl:value-of select="$start"/>
    <xsl:for-each select="ancestor-or-self::rng:element
                          [not(starts-with(@name,'nmt:'))]">
      <xsl:value-of select="concat('/',@name)"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="yam-namespaces">
    <!-- Make <ns> elements for all YANG module namespaces by
         excluding others declared in the input schema -->
    <xsl:for-each
        select="namespace::*[not(name()='xml' or .=$rng-uri or
                .=$nmt-uri or .=$dtdc-uri or .=$dc-uri or
                .=$nma-uri)]">
      <sch:ns uri="{.}" prefix="{name()}"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="uniq-expr-comp">
    <xsl:param name="key"/>
    <xsl:value-of select="concat($key,'=current()/',$key)"/>
  </xsl:template>

  <xsl:template name="check-dup-expr">
    <xsl:param name="nodelist"/>
    <xsl:choose>
      <xsl:when test="contains($nodelist,' ')">
        <xsl:call-template name="uniq-expr-comp">
          <xsl:with-param name="key"
                          select="substring-before($nodelist, ' ')"/>
        </xsl:call-template>
        <xsl:text> and </xsl:text>
        <xsl:call-template name="check-dup-expr">
          <xsl:with-param name="nodelist"
                          select="substring-after($nodelist,' ')"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>  <!-- just one node -->
        <xsl:call-template name="uniq-expr-comp">
          <xsl:with-param name="key"
                          select="$nodelist"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="phases-declaration">
    <xsl:element name="sch:phase">
      <xsl:attribute name="id">full</xsl:attribute>
      <xsl:element name="sch:active">
        <xsl:attribute name="pattern">standard</xsl:attribute>
      </xsl:element>
      <xsl:element name="sch:active">
        <xsl:attribute name="pattern">ref-integrity</xsl:attribute>
      </xsl:element>
    </xsl:element>
    <xsl:element name="sch:phase">
      <xsl:attribute name="id">noref</xsl:attribute>
      <xsl:element name="sch:active">
        <xsl:attribute name="pattern">standard</xsl:attribute>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template match="/">
    <xsl:call-template name="check-input-pars"/>
    <xsl:element name="sch:schema">
      <xsl:apply-templates select="rng:grammar"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="rng:grammar">
    <xsl:call-template name="yam-namespaces"/>
    <xsl:call-template name="nc-namespace"/>
    <xsl:call-template name="phases-declaration"/>
    <xsl:element name="sch:pattern">
      <xsl:attribute name="id">standard</xsl:attribute>
      <xsl:apply-templates mode="std-abstract"
                           select="rng:define//rng:element[&std-annot;]"/>
      <xsl:apply-templates mode="std"
          select="rng:start/rng:element[@name='nmt:netmod-tree']"/>
    </xsl:element>
    <xsl:element name="sch:pattern">
      <xsl:attribute name="id">ref-integrity</xsl:attribute>
      <xsl:apply-templates mode="refint-abstract"
                           select="rng:define//rng:element[&refint-annot;]"/>
      <xsl:apply-templates mode="refint"
          select="rng:start/rng:element[@name='nmt:netmod-tree']"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="rng:element[@name='nmt:netmod-tree']"
                mode="std">
    <xsl:choose>
      <xsl:when test="$target='dstore' or $target='get-reply'
                      or $target='getconf-reply'">
        <xsl:apply-templates select="rng:element[@name='nmt:top']"
                             mode="std"/>
      </xsl:when>
      <xsl:when test="$target='rpc'">
        <xsl:apply-templates select="key('rpc',$name)" mode="std"/>
      </xsl:when>
      <xsl:when test="$target='notif'">
          <xsl:apply-templates
              mode="std"
              select="rng:element[@name='nmt:notifications']/
                      rng:element[rng:element/@name=$name]"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="rng:element[@name='nmt:netmod-tree']"
                mode="refint">
    <xsl:choose>
      <xsl:when test="$target='get-reply' or $target='getconf-reply'">
        <xsl:apply-templates select="rng:element[@name='nmt:top']"
                             mode="refint"/>
      </xsl:when>
      <xsl:when test="$target='rpc'">
        <xsl:apply-templates select="key('rpc',$name)" mode="refint"/>
      </xsl:when>
      <xsl:when test="$target='notif'">
          <xsl:apply-templates
              mode="refint"
              select="rng:element[@name='nmt:notifications']/
                      rng:element[rng:element/@name=$name]"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="rng:element[@name='nmt:top']" mode="std">
    <xsl:apply-templates select="&std-todo;" mode="std"/>
  </xsl:template>

  <xsl:template match="rng:element[@name='nmt:top']" mode="refint">
    <xsl:apply-templates select="&refint-todo;" mode="refint"/>
  </xsl:template>

  <xsl:template match="rng:element" mode="std-abstract">
    <xsl:element name="sch:rule">
      <xsl:attribute name="id">
        <xsl:value-of select="concat('std-',generate-id())"/>
      </xsl:attribute>
      <xsl:attribute name="abstract">true</xsl:attribute>
      <xsl:apply-templates select="&std-annot;"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="rng:element" mode="refint-abstract">
    <xsl:element name="sch:rule">
      <xsl:attribute name="id">
        <xsl:value-of select="concat('refint-',generate-id())"/>
      </xsl:attribute>
      <xsl:attribute name="abstract">true</xsl:attribute>
      <xsl:apply-templates select="&refint-annot;"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="rng:element" mode="std">
    <xsl:element name="sch:rule">
      <xsl:attribute name="context">
        <xsl:call-template name="append-path"/>
      </xsl:attribute>
      <xsl:apply-templates select="&std-annot;"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="rng:element" mode="refint">
    <xsl:element name="sch:rule">
      <xsl:attribute name="context">
        <xsl:call-template name="append-path"/>
      </xsl:attribute>
      <xsl:apply-templates select="&refint-annot;"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="rng:ref" mode="std">
    <xsl:apply-templates select="." mode="std-ref"/>
  </xsl:template>

  <xsl:template match="rng:ref" mode="refint">
    <xsl:apply-templates select="." mode="refint-ref"/>
  </xsl:template>

  <xsl:template match="rng:ref" mode="std-ref">
    <xsl:apply-templates select="//rng:define[@name=current()/@name]"
                         mode="std">
      <xsl:with-param name="dstart">
        <xsl:call-template name="append-path">
          <xsl:with-param name="start">
            <xsl:call-template name="netconf-part"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="rng:ref" mode="refint-ref">
    <xsl:apply-templates select="//rng:define[@name=current()/@name]"
                         mode="refint">
      <xsl:with-param name="dstart">
        <xsl:call-template name="append-path">
          <xsl:with-param name="start">
            <xsl:call-template name="netconf-part"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="rng:define" mode="std">
    <xsl:param name="dstart"/>
    <xsl:apply-templates select="&std-todo;"
                         mode="std-ref">
      <xsl:with-param name="estart" select="$dstart"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="rng:define" mode="refint">
    <xsl:param name="dstart"/>
    <xsl:apply-templates select="&refint-todo;"
                         mode="refint-ref">
      <xsl:with-param name="estart" select="$dstart"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="rng:element" mode="std-ref">
    <xsl:param name="estart"/>
    <xsl:element name="sch:rule">
      <xsl:attribute name="context">
        <xsl:call-template name="append-path">
          <xsl:with-param name="start" select="$estart"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:element name="sch:extends">
        <xsl:attribute name="rule">
          <xsl:value-of select="concat('std-',generate-id())"/>
        </xsl:attribute>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template match="rng:element" mode="refint-ref">
    <xsl:param name="estart"/>
    <xsl:element name="sch:rule">
      <xsl:attribute name="context">
        <xsl:call-template name="append-path">
          <xsl:with-param name="start" select="$estart"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:element name="sch:extends">
        <xsl:attribute name="rule">
          <xsl:value-of select="concat('refint-',generate-id())"/>
        </xsl:attribute>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template match="nma:must">
    <xsl:choose>
      <xsl:when test="nma:error-message">
        <xsl:call-template name="assert-element">
          <xsl:with-param name="test" select="@assert"/>
          <xsl:with-param name="message" select="nma:error-message"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="assert-element">
          <xsl:with-param name="test" select="@assert"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="@nma:key">
    <xsl:call-template name="list-unique">
      <xsl:with-param
          name="message">Duplicate key of list</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="@nma:unique">
    <xsl:call-template name="list-unique">
      <xsl:with-param
          name="message">Violated uniqueness for list</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="list-unique">
    <xsl:param name="message"/>
    <xsl:element name="sch:report">
      <xsl:attribute name="test">
        <xsl:value-of
            select="concat('preceding-sibling::',../@name,'[')"/>
        <xsl:call-template name="check-dup-expr">
          <xsl:with-param name="nodelist" select="."/>
        </xsl:call-template>
        <xsl:text>]</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="concat($message, ' ',../@name)"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="@nma:max-elements">
    <xsl:call-template name="assert-element">
      <xsl:with-param
          name="test"
          select="concat('count(../',../@name,')&lt;=',.)"/>
      <xsl:with-param
          name="message"
          select="concat('List &quot;',../@name,
                  '&quot; - item count must be at most ',.)"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="@nma:min-elements">
    <xsl:call-template name="assert-element">
      <xsl:with-param
          name="test"
          select="concat('count(../',../@name,')&gt;=',.)"/>
      <xsl:with-param
          name="message"
          select="concat('List &quot;',../@name,
                  '&quot; - item count must be at least ',.)"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="@nma:when">
    <xsl:call-template name="assert-element">
      <xsl:with-param
          name="test"
          select="concat('(',.,') or not(..)')"/>
      <xsl:with-param
          name="message"
          select="concat('Node &quot;',../@name,
                  '&quot; requires &quot;',.,'&quot;')"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="@nma:leafref">
    <xsl:call-template name="assert-element">
      <xsl:with-param name="test" select="concat(.,'=.')"/>
      <xsl:with-param
          name="message"
          select="concat('Leafref &quot;',../@name,
                  '&quot; must have the same value as &quot;',.,'&quot;')"/>
    </xsl:call-template>
  </xsl:template>

</xsl:stylesheet>
