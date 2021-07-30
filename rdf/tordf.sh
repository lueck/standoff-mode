#!/usr/bin/env bash

rmlmapper_jar=$RMLMAPPER_JAR
format=turtle
infile=$1
outfile=$infile.ttl # /dev/stdout

temp_dir=$(mktemp -d)
rmlfile=$temp_dir/rml.ttl
temp_in=$temp_dir/annotations.json


echo "Using temp dir $temp_dir"
echo "Using infile $infile"

# temp_in=$temp_dir/annotations.json

# cp $infile $temp_in

sed "s/;/,/g" $infile > $temp_in

sed "s~annotations.json~$temp_in~g" rml4json.ttl > $rmlfile

java -jar $rmlmapper_jar -s $format -m "$rmlfile" -o $outfile
