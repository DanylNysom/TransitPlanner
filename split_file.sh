#!/usr/bin/bash
# from http://stackoverflow.com/a/25194188
ORIG=$1
export ORIG
tail -n +2 $ORIG |
split -l 100000 - --additional-suffix=".txt" `basename -s '.txt' $ORIG` --filter='sh -c "{ head -n1 $ORIG; cat; } > $FILE"'
