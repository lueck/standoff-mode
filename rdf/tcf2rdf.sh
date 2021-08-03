#!/usr/bin/env bash

outfile=/dev/stdout
format=turtle
rmlmapper_jar=$RMLMAPPER_JAR

usage () {
    echo "$1"
    cat <<EOF
USAGE: $0 [ -o OUTFILE ] [ -f FORMAT ] [ -j JAR ] TCF-FILE
  -o   send RDF output to OUTFILE; defaults to $outfile
  -f   any output format rmlmapper knows about; defaults to $format
  -j   path to rmlmapper jar

This is essentially a wrapper around rmlmapper.

EOF
    exit $2
}

while (($# > 1)); do
    case $1 in
	-o)
	    outfile=$2
	    shift
	    shift
	    ;;
	-f)
	    format=$2
	    shift
	    shift
	    ;;
	-*)
	    usage "" 2
	    ;;
    esac
done

if [ $# -lt 1 ]; then usage "" 1; fi
[ -f $1 ] || usage "ERROR: '$1' no such file" 1

infile=$1
range=$(basename -s .tcf $infile)

rmlfile=$(mktemp)

sed "s#INPUT\.tcf#$infile#g; s#INPUT#$range#g" rml4tcf.ttl > $rmlfile

java -jar $rmlmapper_jar -s $format -m "$rmlfile" -o $outfile

rm $rmlfile

