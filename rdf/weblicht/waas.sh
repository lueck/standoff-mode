#! /bin/bash

set -e
# subsequet commands that fail will stop the script

USAGE='usage: waas.sh [options] SRC
with options:
  [-h | --help]                   gives help
  [-c | --chain FILE]             use a chain file.
                                  Defaults to chain-berl+ims.xml

Send SRC file to WebLicht webservice and receive a annotations in Text
Corpus Format (TCF).

This is essantially a wrapper around curl. See
https://weblicht.sfs.uni-tuebingen.de/WaaS/

Your WebLicht API key must be present in the WEBLICHTKEY environment
variable. To set it from file contents run the following code in your
terminal.

export WEBLICHTKEY=$(cat keyfile)

'

die()
{
    echo "$1" >&$2
    exit $3
}

#basedir=$(dirname "$BASH_SOURCE")
# make this script work even if called as a symlink:
basedir=$(dirname $(readlink -f "$BASH_SOURCE"))

chain=$basedir/chain-berl+ims.xml

if [ -v $WEBLICHTKEY ]; then die "$USAGE" 2 1 ; fi

# parse command line parameters
while true; do
    case "$1" in
	-h | --help)
	    die "$USAGE" 1 0 ;;
	-c | --chain)
	    if test ! "$2"; then
		die "$USAGE" 2 1 ;
	    else
		chain=$2
		shift
	    fi ;;
	-*)
	    die "$USAGE" 2 1 ;;
	*)
	    break ;;
    esac
    shift
done


url="https://weblicht.sfs.uni-tuebingen.de/WaaS/api/1.0/chain/process"

echo "Sending request to $url ..." >&2

curl -X POST -F chains=@$chain -F content=@$1 -F apikey=$WEBLICHTKEY $url
