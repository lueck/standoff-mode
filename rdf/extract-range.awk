## INPUT: sourcefile,range-uuid,start-offset,end-offset
## OUTPUT: a command to get the range from the source file
## USAGE:
## export SOURCE_DIR=mysources/
## export RANGE_DIR=myranges/
## roqet -i sparql ranges.sparql -D annotations.ttl -r tsv | tail -n +2 | awk -f extract-range.awk
{printf "tail -c %s %s$(echo '%s' | awk -F'/' '{print $(NF)}') | head -c %s > %s%s;\n", $3, ENVIRON["SOURCE_DIR"], $1, $4-$3, ENVIRON["RANGE_DIR"], $2}
