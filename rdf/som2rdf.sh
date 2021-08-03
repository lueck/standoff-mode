#!/usr/bin/env bash

outfile=/dev/stdout
format=turtle
rmlmapper_jar=$RMLMAPPER_JAR

usage () {
    echo "$1"
    cat <<EOF
USAGE: $0 [ -o OUTFILE ] [ -f FORMAT ] [ -j JAR ] JSON-FILE
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

temp_dir=$(mktemp -d)
rmlfile=$temp_dir/rml.ttl
temp_in=$temp_dir/annotations.json

# fix standoff-mode output
sed "s/;/,/g" $infile > $temp_in

sed "s*annotations.json*$temp_in*g" rml4json.ttl > $rmlfile

java -jar $rmlmapper_jar -s $format -m "$rmlfile" -o $outfile

rm -Rf $temp_dir
