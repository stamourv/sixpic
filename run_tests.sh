#!/bin/bash

# run all the tests

for X in tests/*/*.c ; do
    echo $X >&2 # to pass through any grep
    ./six-comp.scm $X || echo ERROR: $X
#    read
done
