#!/bin/sh

rm *.gz *.trc *.Rout src/*.i src/*.ii src/Makevars.in; rm -rf *.Rcheck; clear; R CMD build .
FLE=`ls RSAP_*.tar.gz`
echo "Eyeball the contents for wrong files:"
tar -tzvf $FLE

echo ""
echo "checking: $FLE"
R CMD check --install=fake --as-cran $FLE
