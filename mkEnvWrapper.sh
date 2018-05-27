#!/bin/bash

TARGET_FILE=${1:-"injectEnv.sh"}

echo generating env wrapper to $TARGET_FILE

echo '#!/bin/sh' > $TARGET_FILE
cat Makefile | awk '/export/,/\# end export/' | awk 'NR>1' | sed -e 's/#.*//' -e 's/*//;s/ *$//' -e 's/\$(/${/' -e 's/)/}/' -e 's/\(.*\) ?= \(.*\)$/export \1=${\1:-"\2"}/' -e 's/\(.*\) ?=/export \1=${\1:-""}/' >> $TARGET_FILE
echo '$@' >> $TARGET_FILE
chmod a+x $TARGET_FILE
