# original by Friedrich Leisch, much changed by BDR

AC_INIT([RSAP], 0.9, [piers@ompka.net])

dnl Select an optional include path, from a configure option
dnl or from an environment variable.
AC_ARG_WITH([nwrfcsdk-include],
            AC_HELP_STRING([--with-nwrfcsdk-include=INCLUDE_PATH],
                           [the location of SAP NW RFCSDK header files]),
            [nwrfcsdk_include_path=$withval])
RSAP_CPPFLAGS="-I."
if test [ -n "$nwrfcsdk_include_path" ] ; then
   RSAP_CPPFLAGS="-I. -I${nwrfcsdk_include_path}"
else
  if test [ -n "${NWRFCSDK_INCLUDE}" ] ; then
     RSAP_CPPFLAGS="-I. -I${NWRFCSDK_INCLUDE}"
  fi
fi

dnl ditto for a library path
RSAP_LIBS=" -lsapnwrfc -lsapucum "
AC_ARG_WITH([nwrfcsdk-lib],
            AC_HELP_STRING([--with-nwrfcsdk-lib=LIB_PATH],
                           [the location of SAP NW RFCSDK libraries]),
            [nwrfcsdk_lib_path=$withval])
if test [ -n "$nwrfcsdk_lib_path" ] ; then
   RSAP_LIBS=" -L${nwrfcsdk_lib_path} ${RSAP_LIBS}"
else 
  if test [ -n "${NWRFCSDK_LIBS}" ] ; then
     RSAP_LIBS=" -L${NWRFCSDK_LIBS} ${RSAP_LIBS}"
  fi
fi

dnl Now find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
echo "R_HOME is: ${R_HOME}"
CC=`"${R_HOME}/bin/R" CMD config CC`
CPP=`"${R_HOME}/bin/R" CMD config CPP`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
EXTCPPFLAGS=`"${R_HOME}/bin/R" CMD config --cppflags`
PKG_CPPFLAGS="${PKG_CPPFLAGS} ${RSAP_CPPFLAGS}"

CPPFLAGS="${CPPFLAGS} ${CFLAGS} ${EXTCPPFLAGS} ${PKG_CPPFLAGS}"
echo "All base CPP flags are: ${CPPFLAGS} "

dnl Check the headers can be found
AC_CHECK_HEADERS(sapnwrfc.h)
if test "${ac_cv_header_sapnwrfc_h}" = no; then
   AC_MSG_ERROR("SAPNWRFCSDK header sapnwrfc.h not found")
fi

PKG_LIBS=" ${RSAP_LIBS} "
save_LIBS="${LIBS}"
LIBS="${LIBS} ${PKG_LIBS}"
AC_TRY_LINK(
   [
    #include <sapnwrfc.h>
   ],[
   RfcInit();
   ],[
   AC_MSG_RESULT(yes! we have libsapnwrfc)
   ],[
       AC_MSG_ERROR([libsapnwrfc is not found!])
   ]
)
LIBS="${save_LIBS}"

dnl for unixODBC header
AC_CHECK_SIZEOF(long, 4)

PKG_CPPFLAGS="${PKG_CPPFLAGS} -DSAPwithUNICODE  -DSAPonUNIX   -DSAPwithTHREADS"
echo "NW RFC SDK flags: ${PKG_CPPFLAGS}"

dnl substitute PKG_CPPFLAGS and LIBS
AC_SUBST(PKG_CPPFLAGS)
AC_SUBST(PKG_LIBS)
AC_CONFIG_HEADERS([src/config.h])
dnl and do subsitution in the src/Makevars.in and src/config.h
echo "Platform is: ${R_PLATFORM}"

dnl extend the case statement for additional host types as we discover them
RSAP_BUILD=
case "${R_PLATFORM}" in
  *linux*)
    echo "we have Linux!"
RSAP_BUILD='

all: $(SHLIB)

$(SHLIB): RSAP.o

RSAP.o: RSAP.c
	$(CC) -E -c $(ALL_CFLAGS) $(ALL_CPPFLAGS) RSAP.c > RSAP.ii
	perl ../tools/u16lit.pl -le RSAP.ii
	$(CC) -c $(ALL_CFLAGS) $(ALL_CPPFLAGS) RSAP.i
'
    ;;
  *)
    echo "We dont have Linux :-("
   ;;
esac
AC_SUBST(RSAP_BUILD)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
