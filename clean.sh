#!/bin/sh

export NWRFCSDK_INCLUDE=/home/piers/code/sap/nwrfcsdk/include
export NWRFCSDK_LIBS=/home/piers/code/sap/nwrfcsdk/lib


#autoconf
#./configure \
#    --with-nwrfcsdk-include=/home/piers/code/sap/nwrfcsdk/include \
#    --with-nwrfcsdk-lib=/home/piers/code/sap/nwrfcsdk/lib

R CMD INSTALL --preclean .
R CMD INSTALL --clean .

rm src/RSAP.i*
rm src/RSAP.o
rm src/RSAP.so
rm rfc*.trc
rm *.Rout
rm RSAP_*.tar.gz
rm -rf autom4te.cache

