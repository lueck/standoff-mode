## INPUT: sourcefile,range-uuid,start-offset,end-offset
## OUTPUT: a command to get the range from the source file
## USAGE:
## export SOURCE_DIR=mysources/
## export RANGE_DIR=myranges/
## roqet -i sparql ranges.sparql -D annotations.ttl -r tsv | tail -n +2 | awk -f extract-range.awk
{printf "cat %s$(echo '%s' | awk -F'/' '{print $(NF)}' | sed 's/>//g') | iconv -t UTF-32 | tail -c +%s | head -c %s | iconv -f UTF-32 > %s%s;\n", ENVIRON["SOURCE_DIR"], $1, ($3*4)+1, ($4-$3)*4, ENVIRON["RANGE_DIR"], $2}
