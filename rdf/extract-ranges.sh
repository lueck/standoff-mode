#!/usr/bin/env bash

usage () {
    echo $1
    cat <<EOF
USAGE: $0 SOURCE_DIR RANGE_DIR ANNOTATIONS


Example: $0 ~/triples/src/ ~/triples/ranges/ ~/tagged/Kant-KpV.TEI-P5.xml.json
EOF

    exit 1;
}

if [ $# -lt 3 ]; then usage ""; fi

export SOURCE_DIR=$1
export RANGE_DIR=$2
ANNOTATIONS=$3

if [ ! -d $SOURCE_DIR ]; then usage "ERROR: $SOURCE_DIR: no such directory"; fi
if [ ! -d $RANGE_DIR ]; then usage "ERROR: $RANGE_DIR: no such directory"; fi
if [ ! -f $ANNOTATIONS ]; then usage "ERROR: $ANNOTATIONS: no such file"; fi

read -r -d '' SPARQL <<EOF
PREFIX som: <http://github.com/lueck/standoff-mode/owl#>

SELECT ?source, ?uuid, ?start, ?end
{
?range a som:markupRange .
?range som:sourceDocument ?source .
?range som:uuid ?uuid .
?range som:sourceStart ?start .
?range som:sourceEnd ?end .
}
EOF

temp_dir=$(mktemp -d)

roqet -i sparql -e "$SPARQL" -D $ANNOTATIONS -r tsv | tail -n +2 | awk -f extract-range.awk > $temp_dir/ranges.sh

source $temp_dir/ranges.sh

rm -Rf $temp_dir
