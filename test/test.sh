#!/bin/sh

good_progs=`find good/* -type f`
bad_progs=`find bad/* -type f`

echo ">>> TESTING BAD PROGRAMS:"
for f in $bad_progs;
do
    echo ">>> Testing $f"
    ../interpreter $f
done

echo ">>> TESTING CORRECT PROGRAMS:"
for f in $good_progs;
do
    echo ">>> Testing $f"
    ../interpreter $f
done
