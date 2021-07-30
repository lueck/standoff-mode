#!/usr/bin/env bash

usage () {
    echo $1
    cat <<EOF
USAGE: $0 DIR FILE_PATTERN

This will copy each file matching FILE_PATTERN to DIR where the file
name will be the md5 hash value.

Example: $0 ~/triples/src ~/tagged/*.TEI-P5.xml
EOF

    exit 1;
}

if [ $# -lt 2 ]; then usage ""; fi
if [ ! -d $1 ]; then usage "ERROR: $1: no such directory"; fi

hashdir=$1

shift

for i in "$@"; do cp -nv $i ${hashdir}/$(md5sum $i | awk '{print $1}'); done
