#!/bin/sh

perl -C -pe 's/&#x([0-9a-fA-F]+);/chr(hex($1))/eg; s/<[^>]*>//g' $1
