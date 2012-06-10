# original by Friedrich Leisch, much changed by BDR

AC_INIT([RSAP], 0.0.2, [piers@ompka.net])

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
AC_ARG_WITH([nwrfcsdk-lib],
            AC_HELP_STRING([--with-nwrfcsdk-lib=LIB_PATH],
                           [the location of SAP NW RFCSDK libraries]),
            [nwrfcsdk_lib_path=$withval])
if test [ -n "$nwrfcsdk_lib_path" ] ; then
   LIBS="-L${nwrfcsdk_lib_path} ${LIBS}"
else 
  if test [ -n "${NWRFCSDK_LIBS}" ] ; then
     LIBS="-L${NWRFCSDK_LIBS} ${LIBS}"
  fi
fi

dnl Now find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CPP=`"${R_HOME}/bin/R" CMD config CPP`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
RSAP_EXT_CPPFLAGS=`"${R_HOME}/bin/R" CMD config --cppflags`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
EXTCPPFLAGS=`"${R_HOME}/bin/R" CMD config --cppflags`

CPPFLAGS="${CPPFLAGS} ${RSAP_CPPFLAGS}"

dnl Check the headers can be found
AC_CHECK_HEADERS(sapnwrfc.h)
if test "${ac_cv_header_sapnwrfc_h}" = no; then
   AC_MSG_ERROR("SAPNWRFCSDK header sapnwrfc.h not found")
fi

LIBS="${LIBS} -lsapnwrfc -lsapucum "
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

dnl for unixODBC header
AC_CHECK_SIZEOF(long, 4)

RSAP_CPPFLAGS="${RSAP_CPPFLAGS} ${RSAP_EXT_CPPFLAGS} "
echo "HERE: ${RSAP_CPPFLAGS}"

dnl substitute RSAP_CPPFLAGS and LIBS
AC_SUBST(RSAP_CPPFLAGS)
AC_SUBST(LIBS)
AC_CONFIG_HEADERS([src/config.h])
dnl and do subsitution in the src/Makevars.in and src/config.h
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
