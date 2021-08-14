#!/usr/bin/env bash

rmlmapper_jar=$RMLMAPPER_JAR
format=turtle
infile=$1
outfile=/dev/stdout

temp_dir=$(mktemp -d)
rmlfile=$temp_dir/rml.ttl

checksum=$(basename $infile)
path=$(readlink -f $infile | xargs dirname)

echo "Using temp dir $temp_dir"
echo "Using infile $infile"

# temp_in=$temp_dir/annotations.json

# cp $infile $temp_in

sed "s*\(rml:source \"\)*\1$path/*g; s*ff97cb8a71544e035fbf538201b52d57*$checksum*g" rml4tei.ttl > $rmlfile

java -jar $rmlmapper_jar -s $format -m $rmlfile -o $outfile

rm -Rf $temp_dir
