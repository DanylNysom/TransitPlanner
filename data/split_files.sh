#!/usr/bin/bash
# from http://stackoverflow.com/a/25194188
CUTOFF_SIZE=100000
FILES=./*.txt
for file in $FILES
do
  if [ `wc -l <$file` -gt $CUTOFF_SIZE ]; then
    export file
    tail -n +2 $file |
    split -l $((CUTOFF_SIZE-1)) - --additional-suffix=".txt" `basename -s '.txt' $file` --filter='sh -c "{ head -n1 $file; cat; } > $FILE"'
    rm $file
  fi
done
