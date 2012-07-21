/*
 *     Copyright (c) 2012 - and onwards Piers Harding.
 *         All rights reserved.
 *
 *         */

/* RSAP low level interface
 *
 */
//#include <config.h>

#include <R.h>
#include <Rdefines.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Rdynload.h>


#define LST_EL(x,i) VECTOR_ELT((x),(i))
#define CHR_EL(x,i) CHAR_DEREF(STRING_ELT((x),(i)))
#define SET_CHR_EL(x,i,val)  SET_STRING_ELT((x),(i), (val))
#define SET_ROWNAMES(df,n)  setAttrib(df, R_RowNamesSymbol, n)
#define GET_CLASS_NAME(x)   GET_CLASS(x)
#define SET_CLASS_NAME(x,n) SET_CLASS(x, n)


#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <limits.h> /* for INT_MAX */

/* SAP flag for Windows NT or 95 */
#ifdef _WIN32
#  ifndef SAPonNT
#    define SAPonNT
#  endif
#endif

#include <sapnwrfc.h>

#if defined(SAPonNT)
#include "windows.h"
#endif


/*
 * local prototypes & declarations
 */
#define MAX_HANDLES 256

/* fake up a definition of bool if it doesnt exist */
#ifndef bool
typedef SAP_RAW    bool;
#endif

/* create my true and false */
#ifndef false
typedef enum { false, true } mybool;
#endif


typedef struct SAPNW_CONN_INFO_rec {
    RFC_CONNECTION_HANDLE handle;
    RFC_CONNECTION_PARAMETER * loginParams;
    unsigned loginParamsLength;
} SAPNW_CONN_INFO, *pSAPNW_CONN_INFO;

static unsigned int nHandles = 0; /* number of Handles opened in session */
static pSAPNW_CONN_INFO opened_handles[MAX_HANDLES+1];

typedef struct SAPNW_FUNC_DESC_rec {
    RFC_FUNCTION_DESC_HANDLE handle;
    SAPNW_CONN_INFO * conn_handle;
    char * name;
} SAPNW_FUNC_DESC;

typedef struct SAPNW_FUNC_rec {
    RFC_FUNCTION_HANDLE handle;
    SAPNW_FUNC_DESC * desc_handle;
} SAPNW_FUNC;

void get_field_value(DATA_CONTAINER_HANDLE hcont, RFC_FIELD_DESC fieldDesc, SEXP value, unsigned fld);
void set_field_value(DATA_CONTAINER_HANDLE hcont, RFC_FIELD_DESC fieldDesc, SEXP sp_value);
SEXP get_table_value(RFC_TABLE_HANDLE tableHandle);
void set_table_value(RFC_TABLE_HANDLE tableHandle, SEXP sp_value);


/* create a parameter space and zero it */
static void * make_space(int len){

    char * ptr;
    ptr = malloc( len + 2 );
    if ( ptr == NULL ) {
        return NULL;
    }
    memset(ptr, 0, len + 2);
    return ptr;
}


void makeDataFrame(SEXP data)
{
    S_EVALUATOR SEXP row_names, df_class_name;
    Sint i, n;
    char buf[1024];

    PROTECT(data);
    PROTECT(df_class_name = NEW_CHARACTER((Sint) 1));
    SET_CHR_EL(df_class_name, 0, COPY_TO_USER_STRING("data.frame"));

    /* row.names */
    n = GET_LENGTH(LST_EL(data, 0));    /* length(data[[1]]) */
    PROTECT(row_names = NEW_CHARACTER(n));
    for (i = 0; i < n; i++) {
        (void) sprintf(buf, "%d", i + 1);
        SET_CHR_EL(row_names, i, COPY_TO_USER_STRING(buf));
    }
    SET_ROWNAMES(data, row_names);
    SET_CLASS_NAME(data, df_class_name);
    UNPROTECT(3);
    return;
}


/*
 *     RFC_RC SAP_API RfcUTF8ToSAPUC(const RFC_BYTE *utf8, unsigned utf8Length,  SAP_UC *sapuc,  unsigned *sapucSize, unsigned *resultLength, RFC_ERROR_INFO *info);
 *
*/

SAP_UC * u8to16c(char * str) {
    RFC_RC rc;
    RFC_ERROR_INFO errorInfo;
    SAP_UC *sapuc;
    unsigned sapucSize, resultLength;

    sapucSize = strlen(str) + 1;
    sapuc = mallocU(sapucSize);
    memsetU(sapuc, 0, sapucSize);

    resultLength = 0;

    rc = RfcUTF8ToSAPUC((RFC_BYTE *)str, strlen(str), sapuc, &sapucSize, &resultLength, &errorInfo);
    return sapuc;
}


SAP_UC * u8to16(SEXP str) {
    RFC_RC rc;
    RFC_ERROR_INFO errorInfo;
    SAP_UC *sapuc;
    unsigned sapucSize, resultLength;

    sapucSize = LENGTH(str) + 1;
    sapuc = mallocU(sapucSize);
    memsetU(sapuc, 0, sapucSize);

    resultLength = 0;

    rc = RfcUTF8ToSAPUC((RFC_BYTE *)translateCharUTF8(str), LENGTH(str), sapuc, &sapucSize, &resultLength, &errorInfo);
    return sapuc;
}


SAP_UC * u8to16r(SEXP str) {
    RFC_RC rc;
    RFC_ERROR_INFO errorInfo;
    SAP_UC *sapuc;
    unsigned sapucSize, resultLength;

    sapucSize = LENGTH(str) + 1;
    sapuc = mallocU(sapucSize);
    memsetU(sapuc, 0, sapucSize);

    resultLength = 0;

    rc = RfcUTF8ToSAPUC((RFC_BYTE *)(str), LENGTH(str), sapuc, &sapucSize, &resultLength, &errorInfo);
    return sapuc;
}


SEXP u16to8c(SAP_UC * str, int len) {
    RFC_RC rc;
    RFC_ERROR_INFO errorInfo;
    unsigned utf8Size, resultLength;
    char * utf8;
    SEXP ans;

    utf8Size = len * 4;
    utf8 = malloc(utf8Size + 2);
    memset(utf8, 0, utf8Size + 2);

    resultLength = 0;

    rc = RfcSAPUCToUTF8(str, len, (RFC_BYTE *)utf8, &utf8Size, &resultLength, &errorInfo);
    PROTECT(ans = mkCharLenCE(utf8, resultLength, CE_UTF8));
    free(utf8);
    UNPROTECT(1);
    return ans;
}


/*
    RFC_RC SAP_API RfcSAPUCToUTF8(const SAP_UC *sapuc,  unsigned sapucLength, RFC_BYTE *utf8, unsigned *utf8Size,  unsigned *resultLength, RFC_ERROR_INFO *info);
*/
SEXP u16to8(SAP_UC * str) {
    RFC_RC rc;
    RFC_ERROR_INFO errorInfo;
    unsigned utf8Size, resultLength;
    char * utf8;
    SEXP ans;

    utf8Size = strlenU(str) * 4;
    utf8 = malloc(utf8Size + 2);
    memset(utf8, 0, utf8Size + 2);

    resultLength = 0;

    rc = RfcSAPUCToUTF8(str, strlenU(str), (RFC_BYTE *)utf8, &utf8Size, &resultLength, &errorInfo);
    PROTECT(ans = mkCharLenCE(utf8, resultLength, CE_UTF8));
    free(utf8);
    UNPROTECT(1);
    return ans;
}




SEXP get_time_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_TIME timeBuff;
    SEXP sp_val;

    rc = RfcGetTime(hcont, name, timeBuff, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetTime (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    sp_val = u16to8c(timeBuff, 6);
    return sp_val;
}


SEXP get_date_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_DATE dateBuff;
    SEXP sp_val;

    rc = RfcGetDate(hcont, name, dateBuff, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetDate (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    sp_val = u16to8c(dateBuff, 8);
    return sp_val;
}


SEXP get_int_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_INT rfc_int;
    SEXP ans;

    rc = RfcGetInt(hcont, name, &rfc_int, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetInt (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    PROTECT(ans = ScalarInteger((int) rfc_int));
    UNPROTECT(1);
    return ans;
}


SEXP get_int1_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_INT1 rfc_int1;
    SEXP ans;

    rc = RfcGetInt1(hcont, name, &rfc_int1, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetInt1 (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    PROTECT(ans = ScalarInteger((int) rfc_int1));
    UNPROTECT(1);
    return ans;
}


SEXP get_int2_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_INT2 rfc_int2;
    SEXP ans;

    rc = RfcGetInt2(hcont, name, &rfc_int2, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetInt2 (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    PROTECT(ans = ScalarInteger((int) rfc_int2));
    UNPROTECT(1);
    return ans;
}


SEXP get_float_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_FLOAT rfc_float;
    SEXP ans;

    rc = RfcGetFloat(hcont, name, &rfc_float, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetFloat (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    PROTECT(ans = ScalarReal((double) rfc_float));
    UNPROTECT(1);
    return ans;
}


SEXP get_string_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    SEXP sp_val;
    unsigned strLen, retStrLen;
    char * buffer;

    rc = RfcGetStringLength(hcont, name, &strLen, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetStringLength (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    // bail out if string is empty 
    if (strLen == 0) {
        return u16to8(cU(""));
    }

    buffer = make_space(strLen*4);
    rc = RfcGetString(hcont, name, (SAP_UC *)buffer, strLen + 2, &retStrLen, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetString (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    sp_val = u16to8c((SAP_UC *)buffer, retStrLen);
    free(buffer);
    return sp_val;
}


SEXP get_xstring_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    SEXP sp_val;
    unsigned strLen, retStrLen;
    char * buffer;

    rc = RfcGetStringLength(hcont, name, &strLen, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetStringLength in XSTRING (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    // bail out if string is empty 
    if (strLen == 0) {
    	fprintf(stderr, "doing empty string\n");
    	return u16to8(cU("xXx"));
    }
    buffer = make_space(strLen);
    rc = RfcGetXString(hcont, name, (SAP_RAW *)buffer, strLen, &retStrLen, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetXString (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    sp_val = u16to8((SAP_UC *)buffer);
    free(buffer);
    return sp_val;
}



SEXP get_num_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, unsigned ulen){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    char * buffer;
    SEXP rfc_float;

    buffer = make_space(ulen*2); // seems that you need 2 null bytes to terminate a string ...
    rc = RfcGetNum(hcont, name, (RFC_NUM *)buffer, ulen, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetNum (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    PROTECT(rfc_float = ScalarReal((double) atof(buffer)));
    free(buffer);
    UNPROTECT(1);
    return rfc_float;
}


SEXP get_bcd_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    unsigned strLen, retStrLen;
    char * buffer;
    SEXP rfc_float;

    // select a random long length for a BCD 
    strLen = 100;

    buffer = make_space(strLen*2);
    rc = RfcGetString(hcont, name, (SAP_UC *)buffer, strLen, &retStrLen, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "(bcd)Problem with RfcGetString (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    PROTECT(rfc_float = ScalarReal((double) atof(buffer)));
    free(buffer);
    UNPROTECT(1);
    return rfc_float;
}


SEXP get_char_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, unsigned ulen){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    char * buffer;
    SEXP sp_val;
    buffer = make_space(ulen*4); // seems that you need 2 null bytes to terminate a string ...
    rc = RfcGetChars(hcont, name, (RFC_CHAR *)buffer, ulen, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetChars (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    sp_val = u16to8((SAP_UC *)buffer);
    free(buffer);
    return sp_val;
}


SEXP get_byte_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, unsigned len){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    char * buffer;
    SEXP sp_val;

    buffer = make_space(len);
    rc = RfcGetBytes(hcont, name, (SAP_RAW *)buffer, len, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetBytes (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    sp_val = u16to8c((SAP_UC *)buffer, len);
    free(buffer);
    return sp_val;
}


SEXP get_structure_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_STRUCTURE_HANDLE line;
    RFC_TYPE_DESC_HANDLE typeHandle;
    RFC_FIELD_DESC fieldDesc;
    unsigned fieldCount, i;
    SEXP ans, ansnames;

    rc = RfcGetStructure(hcont, name, &line, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetStructure (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    typeHandle = RfcDescribeType(line, &errorInfo);
    if (typeHandle == NULL) {
		errorcall(R_NilValue, "Problem with RfcDescribeType (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    rc = RfcGetFieldCount(typeHandle, &fieldCount, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcGetFieldCount (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

	PROTECT(ans = allocVector(VECSXP, fieldCount));
	PROTECT(ansnames = allocVector(STRSXP, fieldCount));

    for (i = 0; i < fieldCount; i++) {
        rc = RfcGetFieldDescByIndex(typeHandle, i, &fieldDesc, &errorInfo);
        if (rc != RFC_OK) {
    		errorcall(R_NilValue, "Problem with RfcGetFieldDescByIndex (%s): %d / %s / %s\n",
    								CHAR(u16to8(name)),
    								errorInfo.code,
    								CHAR(u16to8(errorInfo.key)),
    								CHAR(u16to8(errorInfo.message)));
        }

        // process each field type ...
        get_field_value(line, fieldDesc, ans, i);
        SET_STRING_ELT(ansnames, i, u16to8(fieldDesc.name));
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}


void get_field_value(DATA_CONTAINER_HANDLE hcont, RFC_FIELD_DESC fieldDesc, SEXP value, unsigned fld) {
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_TABLE_HANDLE tableHandle;

	// set the column type
	switch (fieldDesc.type) {
		case RFCTYPE_FLOAT:
		case RFCTYPE_NUM:
		case RFCTYPE_BCD:
			SET_VECTOR_ELT(value, fld, allocVector(REALSXP, 1));
			break;
		case RFCTYPE_TABLE:
		case RFCTYPE_STRUCTURE:
		case RFCTYPE_BYTE:
			SET_VECTOR_ELT(value, fld, allocVector(VECSXP, 1));
			break;
		case RFCTYPE_INT:
		case RFCTYPE_INT2:
		case RFCTYPE_INT1:
			SET_VECTOR_ELT(value, fld, allocVector(INTSXP, 1));
			break;
		case RFCTYPE_XMLDATA:
			fprintf(stderr, "shouldnt get a XMLDATA type parameter - abort\n");
			exit(1);
			break;
		case RFCTYPE_DATE:
		case RFCTYPE_TIME:
		case RFCTYPE_STRING:
		case RFCTYPE_XSTRING:
		case RFCTYPE_CHAR:
		default:
			SET_VECTOR_ELT(value, fld, allocVector(STRSXP, 1));
			break;
	}

	// set the column type
	switch (fieldDesc.type) {
		case RFCTYPE_FLOAT:
			REAL(VECTOR_ELT(value, fld))[0] = REAL(get_float_value(hcont, fieldDesc.name))[0];
			break;
		case RFCTYPE_NUM:
			REAL(VECTOR_ELT(value, fld))[0] = REAL(get_num_value(hcont, fieldDesc.name, fieldDesc.nucLength))[0];
			break;
		case RFCTYPE_BCD:
			REAL(VECTOR_ELT(value, fld))[0] = REAL(get_bcd_value(hcont, fieldDesc.name))[0];
			break;
		case RFCTYPE_TABLE:
            rc = RfcGetTable(hcont, fieldDesc.name, &tableHandle, &errorInfo);
            if (rc != RFC_OK) {
        		errorcall(R_NilValue, "Problem with RfcGetTable (%s): %d / %s / %s\n",
        								CHAR(u16to8(fieldDesc.name)),
        								errorInfo.code,
        								CHAR(u16to8(errorInfo.key)),
        								CHAR(u16to8(errorInfo.message)));
            }
			SET_VECTOR_ELT(VECTOR_ELT(value, fld), 0, get_table_value(tableHandle));
			break;
		case RFCTYPE_STRUCTURE:
			SET_VECTOR_ELT(VECTOR_ELT(value, fld), 0, get_structure_value(hcont, fieldDesc.name));
			break;
		case RFCTYPE_BYTE:
			SET_STRING_ELT(VECTOR_ELT(value, fld), 0, get_byte_value(hcont, fieldDesc.name, fieldDesc.nucLength));
			break;
		case RFCTYPE_INT:
			INTEGER(VECTOR_ELT(value, fld))[0] = INTEGER(get_int_value(hcont, fieldDesc.name))[0];
			break;
		case RFCTYPE_INT2:
			INTEGER(VECTOR_ELT(value, fld))[0] = INTEGER(get_int2_value(hcont, fieldDesc.name))[0];
			break;
		case RFCTYPE_INT1:
			INTEGER(VECTOR_ELT(value, fld))[0] = INTEGER(get_int1_value(hcont, fieldDesc.name))[0];
			break;
		case RFCTYPE_DATE:
			SET_STRING_ELT(VECTOR_ELT(value, fld), 0, get_date_value(hcont, fieldDesc.name));
			break;
		case RFCTYPE_TIME:
			SET_STRING_ELT(VECTOR_ELT(value, fld), 0, get_time_value(hcont, fieldDesc.name));
			break;
		case RFCTYPE_STRING:
			SET_STRING_ELT(VECTOR_ELT(value, fld), 0, get_string_value(hcont, fieldDesc.name));
			break;
		case RFCTYPE_XSTRING:
			SET_STRING_ELT(VECTOR_ELT(value, fld), 0, get_xstring_value(hcont, fieldDesc.name));
			break;
		case RFCTYPE_CHAR:
		default:
			SET_STRING_ELT(VECTOR_ELT(value, fld), 0, get_char_value(hcont, fieldDesc.name, fieldDesc.nucLength));
			break;
	}

	return;
}


SEXP get_table_value(RFC_TABLE_HANDLE tableHandle){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    unsigned tabLen, r, fieldCount, fld;
    RFC_STRUCTURE_HANDLE line;
    RFC_TYPE_DESC_HANDLE typeHandle;
    RFC_FIELD_DESC fieldDesc;
    RFC_TABLE_HANDLE childTableHandle;
    SEXP value, valuenames;

    rc = RfcGetRowCount(tableHandle, &tabLen, NULL);
    if (rc != RFC_OK) {
        errorcall(R_NilValue, "(get)Problem with RfcGetRowCount: %d / %s / %s\n",
                        errorInfo.code,
                        CHAR(u16to8(errorInfo.key)),
                        CHAR(u16to8(errorInfo.message)));
    }

    if (tabLen > 0 ) {
		RfcMoveTo(tableHandle, 0, NULL);
		line = RfcGetCurrentRow(tableHandle, NULL);
		typeHandle = RfcDescribeType(line, &errorInfo);
		if (typeHandle == NULL) {
			errorcall(R_NilValue, "(get)Problem with RfcDescribeType: %d / %s / %s\n",
							errorInfo.code,
							CHAR(u16to8(errorInfo.key)),
							CHAR(u16to8(errorInfo.message)));
		}

		rc = RfcGetFieldCount(typeHandle, &fieldCount, &errorInfo);
		if (rc != RFC_OK) {
			errorcall(R_NilValue, "(get)Problem with RfcGetFieldCount: %d / %s / %s\n",
							errorInfo.code,
							CHAR(u16to8(errorInfo.key)),
							CHAR(u16to8(errorInfo.message)));
		}

		// allocate a vector to contain the column vectors
		PROTECT(value = allocVector(VECSXP, fieldCount));
		PROTECT(valuenames = allocVector(STRSXP, fieldCount));
		for(fld = 0; fld < fieldCount; fld++) {
			rc = RfcGetFieldDescByIndex(typeHandle, fld, &fieldDesc, &errorInfo);
			if (rc != RFC_OK) {
				errorcall(R_NilValue, "(get)Problem with RfcGetFieldDescByIndex (%d): %d / %s / %s\n",
						fld,
						errorInfo.code,
						CHAR(u16to8(errorInfo.key)),
						CHAR(u16to8(errorInfo.message)));
			}
			// collect name
			SET_STRING_ELT(valuenames, fld, u16to8(fieldDesc.name));

			// set the column type
			switch (fieldDesc.type) {
				case RFCTYPE_FLOAT:
				case RFCTYPE_NUM:
				case RFCTYPE_BCD:
					SET_VECTOR_ELT(value, fld, allocVector(REALSXP, tabLen));
					break;
				case RFCTYPE_TABLE:
				case RFCTYPE_STRUCTURE:
				case RFCTYPE_BYTE:
					SET_VECTOR_ELT(value, fld, allocVector(VECSXP, tabLen));
					break;
				case RFCTYPE_INT:
				case RFCTYPE_INT2:
				case RFCTYPE_INT1:
					SET_VECTOR_ELT(value, fld, allocVector(INTSXP, tabLen));
					break;
				case RFCTYPE_XMLDATA:
					fprintf(stderr, "shouldnt get a XMLDATA type parameter - abort\n");
					exit(1);
					break;
				case RFCTYPE_DATE:
				case RFCTYPE_TIME:
				case RFCTYPE_STRING:
				case RFCTYPE_XSTRING:
				case RFCTYPE_CHAR:
				default:
					SET_VECTOR_ELT(value, fld, allocVector(STRSXP, tabLen));
					break;
			}
		}
		setAttrib(value, R_NamesSymbol, valuenames);


		for (r = 0; r < tabLen; r++){
			RfcMoveTo(tableHandle, r, NULL);
			line = RfcGetCurrentRow(tableHandle, NULL);
			for (fld = 0; fld < fieldCount; fld++) {
				rc = RfcGetFieldDescByIndex(typeHandle, fld, &fieldDesc, &errorInfo);
				if (rc != RFC_OK) {
					errorcall(R_NilValue, "(get)Problem with RfcGetFieldDescByIndex (%d): %d / %s / %s\n",
							fld,
							errorInfo.code,
							CHAR(u16to8(errorInfo.key)),
							CHAR(u16to8(errorInfo.message)));
				}

//				fprintfU(stderr, cU("field (%d/%d): %s - type(%d)\n"), r, fld, fieldDesc.name, fieldDesc.type);

				// set the column type
				switch (fieldDesc.type) {
					case RFCTYPE_FLOAT:
						REAL(VECTOR_ELT(value, fld))[r] = REAL(get_float_value(tableHandle, fieldDesc.name))[0];
						break;
					case RFCTYPE_NUM:
						REAL(VECTOR_ELT(value, fld))[r] = REAL(get_num_value(tableHandle, fieldDesc.name, fieldDesc.nucLength))[0];
						break;
					case RFCTYPE_BCD:
						REAL(VECTOR_ELT(value, fld))[r] = REAL(get_bcd_value(tableHandle, fieldDesc.name))[0];
						break;
					case RFCTYPE_TABLE:
			            rc = RfcGetTable(line, fieldDesc.name, &childTableHandle, &errorInfo);
			            if (rc != RFC_OK) {
			        		errorcall(R_NilValue, "Problem with RfcGetTable (%s): %d / %s / %s\n",
			        								CHAR(u16to8(fieldDesc.name)),
			        								errorInfo.code,
			        								CHAR(u16to8(errorInfo.key)),
			        								CHAR(u16to8(errorInfo.message)));
			            }
						SET_VECTOR_ELT(VECTOR_ELT(value, fld), r, get_table_value(childTableHandle));
						break;
					case RFCTYPE_STRUCTURE:
						SET_VECTOR_ELT(VECTOR_ELT(value, fld), r, get_structure_value(tableHandle, fieldDesc.name));
						break;
					case RFCTYPE_BYTE:
						SET_STRING_ELT(VECTOR_ELT(value, fld), r, get_byte_value(tableHandle, fieldDesc.name, fieldDesc.nucLength));
						break;
					case RFCTYPE_INT:
						INTEGER(VECTOR_ELT(value, fld))[r] = INTEGER(get_int_value(tableHandle, fieldDesc.name))[0];
						break;
					case RFCTYPE_INT2:
						INTEGER(VECTOR_ELT(value, fld))[r] = INTEGER(get_int2_value(tableHandle, fieldDesc.name))[0];
						break;
					case RFCTYPE_INT1:
						INTEGER(VECTOR_ELT(value, fld))[r] = INTEGER(get_int1_value(tableHandle, fieldDesc.name))[0];
						break;
					case RFCTYPE_DATE:
						SET_STRING_ELT(VECTOR_ELT(value, fld), r, get_date_value(tableHandle, fieldDesc.name));
						break;
					case RFCTYPE_TIME:
						SET_STRING_ELT(VECTOR_ELT(value, fld), r, get_time_value(tableHandle, fieldDesc.name));
						break;
					case RFCTYPE_STRING:
						SET_STRING_ELT(VECTOR_ELT(value, fld), r, get_string_value(tableHandle, fieldDesc.name));
						break;
					case RFCTYPE_XSTRING:
						SET_STRING_ELT(VECTOR_ELT(value, fld), r, get_xstring_value(tableHandle, fieldDesc.name));
						break;
					case RFCTYPE_CHAR:
					default:
						SET_STRING_ELT(VECTOR_ELT(value, fld), r, get_char_value(tableHandle, fieldDesc.name, fieldDesc.nucLength));
						break;
				}
			}
		}
		makeDataFrame(value);
		UNPROTECT(1);
    }
    else {
        // empty table - empty value
        PROTECT(value = allocVector(VECSXP, 0));
    }

    UNPROTECT(1);
    return value;
}


void get_parameter_value(SEXP sp_name, RFC_FUNCTION_DESC_HANDLE func_desc_handle, RFC_FUNCTION_HANDLE func_handle, SEXP param, int idx){
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_PARAMETER_DESC paramDesc;
    RFC_TABLE_HANDLE tableHandle;
    SAP_UC *p_name;
    SEXP sp_pvalue;

    // get the parameter description 
    rc = RfcGetParameterDescByName(func_desc_handle, (p_name = u8to16(sp_name)), &paramDesc, &errorInfo);

    // bail on a bad call for parameter description 
    if (rc != RFC_OK) {
        free(p_name);
		errorcall(R_NilValue, "Problem with RfcGetParameterDescByName (%s): %d / %s / %s\n",
								CHAR(sp_name),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

//    fprintf(stderr, "the return type is %d \n", paramDesc.type);
    switch (paramDesc.type) {
        case RFCTYPE_DATE:
            sp_pvalue = get_date_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, NEW_CHARACTER(1));
            SET_STRING_ELT(VECTOR_ELT(param, idx), 0, sp_pvalue);
            break;
        case RFCTYPE_TIME:
            sp_pvalue = get_time_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, NEW_CHARACTER(1));
            SET_STRING_ELT(VECTOR_ELT(param, idx), 0, sp_pvalue);
            break;
        case RFCTYPE_NUM:
            sp_pvalue = get_num_value(func_handle, p_name, paramDesc.nucLength);
            SET_VECTOR_ELT(param, idx, allocVector(REALSXP, 1));
            REAL(VECTOR_ELT(param, idx))[0] = REAL(sp_pvalue)[0];
            break;
        case RFCTYPE_BCD:
            sp_pvalue = get_bcd_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, allocVector(REALSXP, 1));
            REAL(VECTOR_ELT(param, idx))[0] = REAL(sp_pvalue)[0];
            break;
        case RFCTYPE_CHAR:
            sp_pvalue = get_char_value(func_handle, p_name, paramDesc.nucLength);
            SET_VECTOR_ELT(param, idx, NEW_CHARACTER(1));
            SET_STRING_ELT(VECTOR_ELT(param, idx), 0, sp_pvalue);
            break;
        case RFCTYPE_BYTE:
            sp_pvalue = get_byte_value(func_handle, p_name, paramDesc.nucLength);
            SET_VECTOR_ELT(param, idx, NEW_CHARACTER(1));
            SET_STRING_ELT(VECTOR_ELT(param, idx), 0, sp_pvalue);
            break;
        case RFCTYPE_FLOAT:
            sp_pvalue = get_float_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, allocVector(REALSXP, 1));
            REAL(VECTOR_ELT(param, idx))[0] = REAL(sp_pvalue)[0];
            break;
        case RFCTYPE_INT:
            sp_pvalue = get_int_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, allocVector(INTSXP, 1));
            INTEGER(VECTOR_ELT(param, idx))[0] = INTEGER(sp_pvalue)[0];
            break;
        case RFCTYPE_INT2:
            sp_pvalue = get_int2_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, allocVector(INTSXP, 1));
            INTEGER(VECTOR_ELT(param, idx))[0] = INTEGER(sp_pvalue)[0];
            break;
        case RFCTYPE_INT1:
            sp_pvalue = get_int1_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, allocVector(INTSXP, 1));
            INTEGER(VECTOR_ELT(param, idx))[0] = INTEGER(sp_pvalue)[0];
            break;
        case RFCTYPE_STRUCTURE:
            sp_pvalue = get_structure_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, sp_pvalue);
            break;
        case RFCTYPE_TABLE:
            rc = RfcGetTable(func_handle, p_name, &tableHandle, &errorInfo);
            if (rc != RFC_OK) {
        		errorcall(R_NilValue, "Problem with RfcGetTable (%s): %d / %s / %s\n",
        								CHAR(sp_name),
        								errorInfo.code,
        								CHAR(u16to8(errorInfo.key)),
        								CHAR(u16to8(errorInfo.message)));
            }
            sp_pvalue = get_table_value(tableHandle);
            SET_VECTOR_ELT(param, idx, sp_pvalue);
            break;
        case RFCTYPE_XMLDATA:
            fprintf(stderr, "shouldnt get a XMLDATA type parameter - abort\n");
            exit(1);
            break;
        case RFCTYPE_STRING:
            sp_pvalue = get_string_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, NEW_CHARACTER(1));
            SET_STRING_ELT(VECTOR_ELT(param, idx), 0, sp_pvalue);
            break;
        case RFCTYPE_XSTRING:
            sp_pvalue = get_xstring_value(func_handle, p_name);
            SET_VECTOR_ELT(param, idx, NEW_CHARACTER(1));
            SET_STRING_ELT(VECTOR_ELT(param, idx), 0, sp_pvalue);
            break;
        default:
            fprintf(stderr, "This type is not implemented (%d) - abort\n", paramDesc.type);
            exit(1);
            break;
    }
    free(p_name);

    return;
//    return sp_pvalue;
}


void set_date_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    SAP_UC *p_value;
    RFC_DATE date_value;

    sp_value = AS_CHARACTER(sp_value);

    if(!isString(sp_value))
    	errorcall(R_NilValue, "RfcSetDate (%s): not a Scalar\n", CHAR(u16to8(name)));
    //if (length(sp_value) != 8)
    //	errorcall(R_NilValue, "RfcSetDate invalid date format (%s): %d\n", CHAR(u16to8(name)), length(sp_value));
    p_value = u8to16(STRING_ELT(sp_value,0));
    memcpy((char *)date_value+0, (char *)p_value, 16);
    free(p_value);

    rc = RfcSetDate(hcont, name, date_value, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetDate (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    return;
}


void set_time_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    SAP_UC *p_value;
    RFC_TIME time_value;

    sp_value = AS_CHARACTER(sp_value);

    if(!isString(sp_value))
    	errorcall(R_NilValue, "RfcSetTime (%s): not a String\n", CHAR(u16to8(name)));
    //if (length(sp_value) != 6)
    // 	errorcall(R_NilValue, "RfcSetTime invalid input date format (%s): %s\n", CHAR(u16to8(name)), CHAR(sp_value));
    p_value = u8to16(STRING_ELT(sp_value,0));
    memcpy((char *)time_value+0, (char *)p_value, 12);
    free(p_value);

    rc = RfcSetTime(hcont, name, time_value, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetTime (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_num_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value, unsigned max){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    SAP_UC *p_value;

    sp_value = AS_CHARACTER(sp_value);

    if (length(sp_value) > max)
    	errorcall(R_NilValue, "RfcSetTime invalid input date format (%s): %s\n", CHAR(u16to8(name)), CHAR(sp_value));

    p_value = u8to16(STRING_ELT(sp_value,0));
    rc = RfcSetNum(hcont, name, (RFC_NUM *)p_value, strlenU(p_value), &errorInfo);
    free(p_value);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetNum (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_bcd_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    SAP_UC *p_value;

    sp_value = AS_CHARACTER(sp_value);

    if(!isString(sp_value))
    	errorcall(R_NilValue, "set_bcd_value (%s): not a String\n", CHAR(u16to8(name)));
    p_value = u8to16(STRING_ELT(sp_value,0));
    rc = RfcSetString(hcont, name, p_value, strlenU(p_value), &errorInfo);
    free(p_value);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "(bcd)Problem with RfcSetString (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_char_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value, unsigned max){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    SAP_UC *p_value;

    sp_value = AS_CHARACTER(sp_value);

    if(!isString(sp_value)) {
    	errorcall(R_NilValue, "RfcSetChar (%s): not a Stringr\n", CHAR(u16to8(name)));
    }
    if (length(sp_value) > max) {
    	errorcall(R_NilValue, "RfcSetChar string too long (%s): %s\n", CHAR(u16to8(name)), CHAR(sp_value));
    }

    p_value = u8to16(STRING_ELT(sp_value,0));
    rc = RfcSetChars(hcont, name, p_value, strlenU(p_value), &errorInfo);
    free(p_value);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetChars (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }
    return;
}


void set_byte_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value, unsigned max){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;

    sp_value = AS_CHARACTER(sp_value);

    if(!isString(sp_value))
    	errorcall(R_NilValue, "RfcSetByte (%s): not a String\n", CHAR(u16to8(name)));
    if (length(sp_value) > max)
    	errorcall(R_NilValue, "RfcSetByte string too long (%s): %s\n", CHAR(u16to8(name)), CHAR(sp_value));
    rc = RfcSetBytes(hcont, name, (SAP_RAW *)translateCharUTF8(STRING_ELT(sp_value,0)), LENGTH(sp_value), &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetBytes (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_float_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;

    sp_value = AS_NUMERIC(sp_value);

    if(!isReal(sp_value))
    	errorcall(R_NilValue, "RfcSetFloat (%s): not a Scalar or Int\n", CHAR(u16to8(name)));
    rc = RfcSetFloat(hcont, name, (RFC_FLOAT) REAL(sp_value)[0], &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetFloat (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_int_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;

    sp_value = AS_INTEGER(sp_value);

    if(TYPEOF(sp_value) != INTSXP)
    	errorcall(R_NilValue, "RfcSetInt (%s): not an Integer\n", CHAR(u16to8(name)));
    rc = RfcSetInt(hcont, name, (RFC_INT) INTEGER(sp_value)[0], &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetInt (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_int1_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;

    sp_value = AS_INTEGER(sp_value);

    if(TYPEOF(sp_value) != INTSXP)
    	errorcall(R_NilValue, "RfcSetInt1 (%s): not an Integer\n", CHAR(u16to8(name)));
    if (INTEGER(sp_value)[0] > 255)
    	errorcall(R_NilValue, "RfcSetInt1 invalid input value too big on (%s): %s\n", CHAR(u16to8(name)), CHAR(sp_value));
    rc = RfcSetInt1(hcont, name, (RFC_INT1) INTEGER(sp_value)[0], &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetInt1 (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_int2_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;

    sp_value = AS_INTEGER(sp_value);

    if(TYPEOF(sp_value) != INTSXP)
    	errorcall(R_NilValue, "RfcSetInt2 (%s): not an Integer\n", CHAR(u16to8(name)));
    if (INTEGER(sp_value)[0] > 4095)
    	errorcall(R_NilValue, "RfcSetInt2 invalid input value too big on (%s): %s\n", CHAR(u16to8(name)), CHAR(sp_value));
    rc = RfcSetInt2(hcont, name, (RFC_INT2) INTEGER(sp_value)[0], &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetInt2 (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_string_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    SAP_UC *p_value;

    sp_value = AS_CHARACTER(sp_value);

    if(!isString(sp_value))
    	errorcall(R_NilValue, "RfcSetString (%s): not a String\n", CHAR(u16to8(name)));
    p_value = u8to16(STRING_ELT(sp_value,0));
    rc = RfcSetString(hcont, name, (SAP_UC *)p_value, strlenU(p_value), &errorInfo);
    free(p_value);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetString (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_xstring_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;

    sp_value = AS_CHARACTER(sp_value);

    if(!isString(sp_value))
    	errorcall(R_NilValue, "RfcSetXString (%s): not a String\n", CHAR(u16to8(name)));
    rc = RfcSetXString(hcont, name, (SAP_RAW *)translateCharUTF8(STRING_ELT(sp_value,0)), LENGTH(sp_value), &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "Problem with RfcSetXString (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    return;
}


void set_structure_value(DATA_CONTAINER_HANDLE hcont, SAP_UC *name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_STRUCTURE_HANDLE line;
    RFC_TYPE_DESC_HANDLE typeHandle;
    RFC_FIELD_DESC fieldDesc;
    SAP_UC *p_name;
    unsigned i, idx;
    SEXP elmt, names;

    idx = length(sp_value);
    names = sp_value;

    rc = RfcGetStructure(hcont, name, &line, &errorInfo);
    if (rc != RFC_OK) {
		errorcall(R_NilValue, "(set)Problem with RfcGetStructure (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    typeHandle = RfcDescribeType(line, &errorInfo);
    if (typeHandle == NULL) {
		errorcall(R_NilValue, "(set)Problem with RfcDescribeType (%s): %d / %s / %s\n",
								CHAR(u16to8(name)),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

    for (i = 0; i < idx; i++) {
        elmt = VECTOR_ELT(sp_value, i);
        name = (SAP_UC *) u8to16(STRING_ELT(getAttrib(names, R_NamesSymbol),i));

        rc = RfcGetFieldDescByName(typeHandle, (p_name = u8to16(STRING_ELT(getAttrib(names, R_NamesSymbol),i))), &fieldDesc, &errorInfo);
        if (rc != RFC_OK) {
    		errorcall(R_NilValue, "(set)Problem with RfcGetFieldDescByName (%s/%s): %d / %s / %s\n",
    								CHAR(u16to8(name)),
    								CHAR(STRING_ELT(getAttrib(names, R_NamesSymbol),i)),
    								errorInfo.code,
    								CHAR(u16to8(errorInfo.key)),
    								CHAR(u16to8(errorInfo.message)));
        }
        // XXX dodgey copy back of field name !!!!!
//        memcpy(fieldDesc.name, p_name, strlenU(p_name)*2+2);
        free(p_name);
        set_field_value(line, fieldDesc, elmt);
    }

    return;
}


void set_field_value(DATA_CONTAINER_HANDLE hcont, RFC_FIELD_DESC fieldDesc, SEXP sp_value){
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_TABLE_HANDLE tableHandle;

    //fprintfU(stderr, cU("set field name: %s \n"), fieldDesc.name);
    switch (fieldDesc.type) {
        case RFCTYPE_DATE:
            set_date_value(hcont, fieldDesc.name, sp_value);
            break;
        case RFCTYPE_TIME:
            set_time_value(hcont, fieldDesc.name, sp_value);
            break;
        case RFCTYPE_NUM:
            set_num_value(hcont, fieldDesc.name, sp_value, fieldDesc.nucLength);
            break;
        case RFCTYPE_BCD:
            set_bcd_value(hcont, fieldDesc.name, sp_value);
            break;
        case RFCTYPE_CHAR:
            set_char_value(hcont, fieldDesc.name, sp_value, fieldDesc.nucLength);
            break;
        case RFCTYPE_BYTE:
            set_byte_value(hcont, fieldDesc.name, sp_value, fieldDesc.nucLength);
            break;
        case RFCTYPE_FLOAT:
            set_float_value(hcont, fieldDesc.name, sp_value);
            break;
        case RFCTYPE_INT:
            set_int_value(hcont, fieldDesc.name, sp_value);
            break;
        case RFCTYPE_INT2:
            set_int2_value(hcont, fieldDesc.name, sp_value);
            break;
        case RFCTYPE_INT1:
            set_int1_value(hcont, fieldDesc.name, sp_value);
            break;
        case RFCTYPE_STRUCTURE:
            set_structure_value(hcont, fieldDesc.name, sp_value);
            break;
        case RFCTYPE_TABLE:
            rc = RfcGetTable(hcont, fieldDesc.name, &tableHandle, &errorInfo);
            if (rc != RFC_OK) {
        		errorcall(R_NilValue, "(set_tabl_value)Problem with RfcGetTable (%s): %d / %s / %s\n",
        								CHAR(u16to8(fieldDesc.name)),
        								errorInfo.code,
        								CHAR(u16to8(errorInfo.key)),
        								CHAR(u16to8(errorInfo.message)));
            }
            set_table_value(tableHandle, sp_value);
            break;
        case RFCTYPE_XMLDATA:
            fprintf(stderr, "shouldnt get a XMLDATA type parameter - abort\n");
            exit(1);
            break;
        case RFCTYPE_STRING:
            set_string_value(hcont, fieldDesc.name, sp_value);
            break;
        case RFCTYPE_XSTRING:
            set_xstring_value(hcont, fieldDesc.name, sp_value);
            break;
        default:
            fprintf(stderr, "Set field - This type is not implemented (%d) - abort\n", fieldDesc.type);
            exit(1);
            break;
    }

    return;
}


void set_table_value(RFC_TABLE_HANDLE tableHandle, SEXP sp_value){
	RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_STRUCTURE_HANDLE line;
    RFC_TYPE_DESC_HANDLE typeHandle;
    RFC_FIELD_DESC fieldDesc;
    unsigned r, idx, fieldCount, fld;
    SAP_UC * p_name;
    SEXP names, sp_name;

    if(TYPEOF(sp_value) != VECSXP)
    	errorcall(R_NilValue, "set_table_value not an ARRAY\n");

    if (length(sp_value) <= 0) {
    	// empty table columns
    	return;
    }

    // get first column and determine length of table
    idx = length(VECTOR_ELT(sp_value, 0));
    if (idx <= 0) {
    	// empty table columns
    	return;
    }

    // get field names
    names = getAttrib(sp_value, R_NamesSymbol);

    for (r = 0; r < idx; r++) {
        line = RfcAppendNewRow(tableHandle, &errorInfo);
        if (line == NULL) {
    		errorcall(R_NilValue, "(set_table_value)Problem with RfcAppendNewRow: %d / %s / %s\n",
    								errorInfo.code,
    								CHAR(u16to8(errorInfo.key)),
    								CHAR(u16to8(errorInfo.message)));
        }
        typeHandle = RfcDescribeType(line, &errorInfo);
        if (typeHandle == NULL) {
    		errorcall(R_NilValue, "(set_table_value)Problem with RfcDescribeType: %d / %s / %s\n",
    								errorInfo.code,
    								CHAR(u16to8(errorInfo.key)),
    								CHAR(u16to8(errorInfo.message)));
        }

        // get the number of fields per row
		rc = RfcGetFieldCount(typeHandle, &fieldCount, &errorInfo);
		if (rc != RFC_OK) {
			errorcall(R_NilValue, "(set table)Problem with RfcGetFieldCount: %d / %s / %s\n",
							errorInfo.code,
							CHAR(u16to8(errorInfo.key)),
							CHAR(u16to8(errorInfo.message)));
		}


	    // loop through all Input/Changing/tables parameters and set the values in the call
	    fieldCount = LENGTH(names);

	    // some might not have parameters like RFC_PING
        // loop through the fields of the table row
		for (fld = 0; fld < fieldCount; fld++) {
			sp_name = STRING_ELT(names, fld);
	        rc = RfcGetFieldDescByName(typeHandle, (p_name = u8to16(sp_name)), &fieldDesc, &errorInfo);
	        if (rc != RFC_OK) {
	    		errorcall(R_NilValue, "(set_table_line)Problem with RfcGetFieldDescByName (%s): %d / %s / %s\n",
	    								CHAR(sp_name),
	    								errorInfo.code,
	    								CHAR(u16to8(errorInfo.key)),
	    								CHAR(u16to8(errorInfo.message)));
	        }

			// must equal columns
			if (length(VECTOR_ELT(sp_value, fld)) != idx) {
				errorcall(R_NilValue, "(set table) Problem with VECTOR length not the same for all columns: %d / %d \n",
						length(VECTOR_ELT(sp_value, fld)),
						fieldCount);
			}

	        // XXX dodgey copy back of field name !!!!!
	//        memcpy(fieldDesc.name, p_name, strlenU(p_name)*2+2);
	        set_field_value(line, fieldDesc, VECTOR_ELT(VECTOR_ELT(sp_value, fld), r));
	        free(p_name);
		}
    }
    return;
}


void set_parameter_value(RFC_FUNCTION_DESC_HANDLE func_desc_handle, RFC_FUNCTION_HANDLE func_handle, SEXP sp_name, SEXP sp_value){

    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_TABLE_HANDLE tableHandle;
    RFC_PARAMETER_DESC paramDesc;
    SAP_UC *p_name;

    // get the parameter description 
    rc = RfcGetParameterDescByName(func_desc_handle, (p_name = u8to16(sp_name)), &paramDesc, &errorInfo);

    // bail on a bad call for parameter description 
    if (rc != RFC_OK) {
        free(p_name);
		errorcall(R_NilValue, "(Set)Problem with RfcGetParameterDescByName (%s): %d / %s / %s\n",
								CHAR(sp_name),
								errorInfo.code,
								CHAR(u16to8(errorInfo.key)),
								CHAR(u16to8(errorInfo.message)));
    }

//    fprintfU(stderr, cU("parameter name: %s \n"), p_name);
    switch (paramDesc.type) {
        case RFCTYPE_DATE:
            set_date_value(func_handle, p_name, sp_value);
            break;
        case RFCTYPE_TIME:
            set_time_value(func_handle, p_name, sp_value);
            break;
        case RFCTYPE_NUM:
            set_num_value(func_handle, p_name, sp_value, paramDesc.nucLength);
            break;
        case RFCTYPE_BCD:
            set_bcd_value(func_handle, p_name, sp_value);
            break;
        case RFCTYPE_CHAR:
            set_char_value(func_handle, p_name, sp_value, paramDesc.nucLength);
            break;
        case RFCTYPE_BYTE:
            set_byte_value(func_handle, p_name, sp_value, paramDesc.nucLength);
            break;
        case RFCTYPE_FLOAT:
            set_float_value(func_handle, p_name, sp_value);
            break;
        case RFCTYPE_INT:
            set_int_value(func_handle, p_name, sp_value);
            break;
        case RFCTYPE_INT2:
            set_int2_value(func_handle, p_name, sp_value);
            break;
        case RFCTYPE_INT1:
            set_int1_value(func_handle, p_name, sp_value);
            break;
        case RFCTYPE_STRUCTURE:
            set_structure_value(func_handle, p_name, sp_value);
            break;
        case RFCTYPE_TABLE:
            rc = RfcGetTable(func_handle, p_name, &tableHandle, &errorInfo);
            if (rc != RFC_OK) {
        		errorcall(R_NilValue, "(set_tabl_value)Problem with RfcGetTable (%s): %d / %s / %s\n",
        								CHAR(u16to8(p_name)),
        								errorInfo.code,
        								CHAR(u16to8(errorInfo.key)),
        								CHAR(u16to8(errorInfo.message)));
            }
            set_table_value(tableHandle, sp_value);
            break;
        case RFCTYPE_XMLDATA:
            fprintf(stderr, "shouldnt get a XMLDATA type parameter - abort\n");
            exit(1);
            break;
        case RFCTYPE_STRING:
            set_string_value(func_handle, p_name, sp_value);
            break;
        case RFCTYPE_XSTRING:
            set_xstring_value(func_handle, p_name, sp_value);
            break;
        default:
            fprintf(stderr, "This type is not implemented (%d) - abort\n", paramDesc.type);
            exit(1);
            break;
    }
    free(p_name);
    return;
}



SEXP RSAPRFCConnect(SEXP args)
{
    SEXP names;
    SEXP ptr;
    SEXP ans;
    SEXP elmt;
    S_EVALUATOR SEXP df_class_name;

    RFC_ERROR_INFO errorInfo;
    SAPNW_CONN_INFO *hptr;
    RFC_CONNECTION_PARAMETER * loginParams;
    int idx, i;

    hptr = malloc(sizeof(SAPNW_CONN_INFO));
    hptr->handle = NULL;
    idx = length(args);
    names = args;

    if (idx < 1) {
        Rprintf("No connection parameters\n");
        return(R_NilValue);
    }

    loginParams = malloc(idx*sizeof(RFC_CONNECTION_PARAMETER));
    memset(loginParams, 0,idx*sizeof(RFC_CONNECTION_PARAMETER));

    for (i = 0; i < idx; i++) {
        elmt = VECTOR_ELT(args, i);
        loginParams[i].name = (SAP_UC *) u8to16(STRING_ELT(getAttrib(names, R_NamesSymbol),i));
        loginParams[i].value = (SAP_UC *) u8to16(STRING_ELT(elmt, 0));
    }
    hptr->handle = RfcOpenConnection(loginParams, idx, &errorInfo);

    if (hptr->handle == NULL) {
        for (i = 0; i < idx; i++) {
            free((char *) loginParams[i].name);
            free((char *) loginParams[i].value);
        }
        free(loginParams);
    }
    if (hptr->handle == NULL) {
        errorcall(R_NilValue, "RFC connection open failed: %d / %s / %s\n",
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
    }
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = nHandles;
    ptr = R_MakeExternalPtr(hptr, install("RSAP_handle"), R_NilValue);
    PROTECT(ptr);
    setAttrib(ans, install("handle_ptr"), ptr);
    if(nHandles <= MAX_HANDLES) opened_handles[nHandles] = hptr;
    PROTECT(df_class_name = NEW_CHARACTER((Sint) 1));
    SET_CHR_EL(df_class_name, 0, COPY_TO_USER_STRING("RSAP_Connector"));
    SET_CLASS_NAME(ans, df_class_name);
    UNPROTECT(3);
    return ans;
}


/************************************************
 *
 *		DISCONNECT
 *
 * **********************************************/

SEXP RSAPValidHandle(SEXP handle)
{
    RFC_RC rc;
    RFC_ERROR_INFO errorInfo;
    SEXP ptr = getAttrib(handle, install("handle_ptr"));
    SAPNW_CONN_INFO *hptr = R_ExternalPtrAddr(ptr);
//    Rprintf("checking handle %p\n", hptr);
    rc = RfcPing(hptr->handle, &errorInfo);
    if (rc != RFC_OK) {
        return R_NilValue;
    } else {
        return ScalarInteger(1);
    }
}


SEXP RSAPClose(SEXP handle)
{
    RFC_RC rc;
    RFC_ERROR_INFO errorInfo;
    SEXP exptr = getAttrib(handle, install("handle_ptr"));
    SAPNW_CONN_INFO *hptr = R_ExternalPtrAddr(exptr);
//    Rprintf("(CLose)got handle %p\n", hptr);

    rc = RfcCloseConnection(hptr->handle, &errorInfo);
    hptr->handle = NULL;
    free(hptr);
    R_ClearExternalPtr(exptr);
    if (asInteger(handle) <= MAX_HANDLES) opened_handles[asInteger(handle)] = NULL;
    if (rc != RFC_OK) {
        errorcall(R_NilValue, "Problem closing RFC connection handle: %d / %s / %s\n",
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
    }
    return Rf_ScalarLogical(1);
}


SEXP RSAPGetInfo(SEXP handle)
{
    RFC_ATTRIBUTES attribs;
    RFC_ERROR_INFO errorInfo;
    RFC_RC rc = RFC_OK;
    SEXP exptr = getAttrib(handle, install("handle_ptr"));
    SAPNW_CONN_INFO *hptr = R_ExternalPtrAddr(exptr);

    SEXP ans, ansnames;
    int i=0;

    rc = RfcGetConnectionAttributes(hptr->handle, &attribs, &errorInfo);

    // bail on a bad return code
    if (rc != RFC_OK) {
        errorcall(R_NilValue, "Problem getting connection attributes: %d / %s / %s\n",
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
    }

    // else return a list of connection attributes
    PROTECT(ans = allocVector(STRSXP, 20));
    PROTECT(ansnames = allocVector(STRSXP, 20));
    SET_STRING_ELT(ans, i, u16to8(attribs.dest));
    SET_STRING_ELT(ansnames, i++, mkChar("dest"));
    SET_STRING_ELT(ans, i, u16to8(attribs.host));
    SET_STRING_ELT(ansnames, i++, mkChar("host"));
    SET_STRING_ELT(ans, i, u16to8(attribs.partnerHost));
    SET_STRING_ELT(ansnames, i++, mkChar("partnerHost"));
    SET_STRING_ELT(ans, i, u16to8(attribs.sysNumber));
    SET_STRING_ELT(ansnames, i++, mkChar("sysNumber"));
    SET_STRING_ELT(ans, i, u16to8(attribs.sysId));
    SET_STRING_ELT(ansnames, i++, mkChar("sysId"));
    SET_STRING_ELT(ans, i, u16to8(attribs.client));
    SET_STRING_ELT(ansnames, i++, mkChar("client"));
    SET_STRING_ELT(ans, i, u16to8(attribs.user));
    SET_STRING_ELT(ansnames, i++, mkChar("user"));
    SET_STRING_ELT(ans, i, u16to8(attribs.language));
    SET_STRING_ELT(ansnames, i++, mkChar("language"));
    SET_STRING_ELT(ans, i, u16to8(attribs.trace));
    SET_STRING_ELT(ansnames, i++, mkChar("trace"));
    SET_STRING_ELT(ans, i, u16to8(attribs.isoLanguage));
    SET_STRING_ELT(ansnames, i++, mkChar("isoLanguage"));
    SET_STRING_ELT(ans, i, u16to8(attribs.codepage));
    SET_STRING_ELT(ansnames, i++, mkChar("codepage"));
    SET_STRING_ELT(ans, i, u16to8(attribs.partnerCodepage));
    SET_STRING_ELT(ansnames, i++, mkChar("partnerCodepage"));
    SET_STRING_ELT(ans, i, u16to8(attribs.rfcRole));
    SET_STRING_ELT(ansnames, i++, mkChar("rfcRole"));
    SET_STRING_ELT(ans, i, u16to8(attribs.type));
    SET_STRING_ELT(ansnames, i++, mkChar("type"));
    SET_STRING_ELT(ans, i, u16to8(attribs.rel));
    SET_STRING_ELT(ansnames, i++, mkChar("rel"));
    SET_STRING_ELT(ans, i, u16to8(attribs.partnerType));
    SET_STRING_ELT(ansnames, i++, mkChar("partnerType"));
    SET_STRING_ELT(ans, i, u16to8(attribs.partnerRel));
    SET_STRING_ELT(ansnames, i++, mkChar("partnerRel"));
    SET_STRING_ELT(ans, i, u16to8(attribs.kernelRel));
    SET_STRING_ELT(ansnames, i++, mkChar("kernelRel"));
    SET_STRING_ELT(ans, i, u16to8(attribs.cpicConvId));
    SET_STRING_ELT(ansnames, i++, mkChar("cpicConvId"));
    SET_STRING_ELT(ans, i, u16to8(attribs.progName));
    SET_STRING_ELT(ansnames, i++, mkChar("progName"));

    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}


SEXP RSAPInvoke(SEXP handle, SEXP func, SEXP parms)
{
    RFC_ERROR_INFO errorInfo;
    RFC_RC rc = RFC_OK;
    SEXP exptr = getAttrib(handle, install("handle_ptr"));
    SAPNW_CONN_INFO *hptr = R_ExternalPtrAddr(exptr);
//    Rprintf("got handle %p\n", hptr);

    SEXP ans, ansnames, names, name, value;
    SAP_UC * fname;
    RFC_FUNCTION_DESC_HANDLE func_desc_handle;
    RFC_FUNCTION_HANDLE func_handle;
    RFC_TABLE_HANDLE tableHandle;
    SAP_UC *p_name;
    int i;
    RFC_PARAMETER_DESC parm_desc;
    unsigned idx;

    if (TYPEOF(func) != STRSXP) {
        errorcall(R_NilValue, "Function name is not a string\n");
    }

    if(TYPEOF(parms) != VECSXP) {
        errorcall(R_NilValue, "invoke: parameters not a HASH\n");
    }

//    Rprintf("function to lookup %s\n", CHAR(STRING_ELT(func, 0)));
    func_desc_handle = RfcGetFunctionDesc(hptr->handle, fname = u8to16(STRING_ELT(func,0)), &errorInfo);
    free((char *)fname);

    // bail on a bad lookup
    if (func_desc_handle == NULL) {
        errorcall(R_NilValue, "Problem looking up RFC (%s): %d / %s / %s\n",
        							CHAR(STRING_ELT(func,0)),
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
    }


    // create function call container
    func_handle = RfcCreateFunction(func_desc_handle, &errorInfo);

    // bail on a create problem
    if (func_handle == NULL) {
        errorcall(R_NilValue, "Problem looking up RFC (%s): %d / %s / %s\n",
        							CHAR(STRING_ELT(func,0)),
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
    }

    // loop through all Input/Changing/tables parameters and set the values in the call 
    idx = LENGTH(parms);

    //Rprintf("no. parameters %d\n", idx);
    names = getAttrib(parms, R_NamesSymbol);

    // some might not have parameters like RFC_PING
    for (i = 0; i < idx; i++) {
        value = VECTOR_ELT(parms, i);
        p_name = (SAP_UC *) u8to16(STRING_ELT(getAttrib(parms, R_NamesSymbol),i));
        rc = RfcGetParameterDescByName(func_desc_handle, p_name, &parm_desc, &errorInfo);
        // bail on a bad RfcGetParameterDescByName 
        if (rc != RFC_OK) {
            errorcall(R_NilValue, "Problem in RfcGetParameterDescByName (%s): %d / %s / %s\n",
            						CHAR(STRING_ELT(func,0)),
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
        }

        // set parameter
        //fprintfU(stderr, cU("Parameter (%d): %s - direction: (%d) - type(%d)\n"), i, parm_desc.name, parm_desc.direction, parm_desc.type);
        name = STRING_ELT(names,i);
        switch(parm_desc.direction) {
            case RFC_EXPORT:
                break;
            case RFC_IMPORT:
            case RFC_CHANGING:
                set_parameter_value(func_desc_handle, func_handle, name, value);
                break;
            case RFC_TABLES:
                if(TYPEOF(value) != VECSXP)
                    errorcall(R_NilValue, "invoke outbound parameter (%s): not an ARRAY\n", CHAR(name));
                rc = RfcGetTable(func_handle, p_name, &tableHandle, &errorInfo);
                if (rc != RFC_OK) {
                    errorcall(R_NilValue, "(set)Problem with RfcGetTable (%s): %d / %s / %s\n",
                                    CHAR(name),
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
                }
                set_table_value(tableHandle, value);
                break;
            default:
                fprintf(stderr, "should not get here!\n");
                exit(1);
                break;
        }
        free(p_name);
    }


    rc = RfcInvoke(hptr->handle, func_handle, &errorInfo);


    // bail on a bad RFC Call 
    if (rc != RFC_OK) {
        errorcall(R_NilValue, "Problem Invoking RFC (%s): %d / %s / %s\n",
        			CHAR(STRING_ELT(func,0)),
                    errorInfo.code,
                    CHAR(u16to8(errorInfo.key)),
                    CHAR(u16to8(errorInfo.message)));
    }

    // Get the parameter details 
    rc = RfcGetParameterCount(func_desc_handle, &idx, &errorInfo);
    if (rc != RFC_OK) {
        errorcall(R_NilValue, "Problem in RfcGetParameterCount (%s): %d / %s / %s\n",
        						CHAR(STRING_ELT(func,0)),
                                errorInfo.code,
                                CHAR(u16to8(errorInfo.key)),
                                CHAR(u16to8(errorInfo.message)));
    }

//    fprintf(stderr, "return parameters: %d \n", idx);
    PROTECT(ans = allocVector(VECSXP, idx));
    PROTECT(ansnames = allocVector(STRSXP, idx));
    for (i = 0; i < idx; i++) {
        rc = RfcGetParameterDescByIndex(func_desc_handle, i, &parm_desc, &errorInfo);
        // bail on a bad RfcGetParameterDescByIndex
        if (rc != RFC_OK) {
            errorcall(R_NilValue, "Problem in RfcGetParameterDescByIndex (%s): %d / %s / %s\n",
            						CHAR(STRING_ELT(func,0)),
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
        }
//        fprintfU(stderr, cU("Parameter (%d): %s - direction: (%d) - type(%d)\n"), i, parm_desc.name, parm_desc.direction, parm_desc.type);
        name = u16to8(parm_desc.name);
        switch(parm_desc.direction) {
            case RFC_IMPORT:
                break;
            case RFC_EXPORT:
            case RFC_CHANGING:
                get_parameter_value(name, func_desc_handle, func_handle, ans, i);
                break;
            case RFC_TABLES:
                rc = RfcGetTable(func_handle, parm_desc.name, &tableHandle, &errorInfo);
                if (rc != RFC_OK) {
                    errorcall(R_NilValue, "(get)Problem with RfcGetTable (%s): %d / %s / %s\n",
                                    CHAR(name),
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
                }
//            	continue;
                value = get_table_value(tableHandle);
                SET_VECTOR_ELT(ans, i, value);
                break;
        }
        SET_STRING_ELT(ansnames, i, name);
    }

    // cleanup the call container
    rc = RfcDestroyFunction(func_handle, &errorInfo);
    if (rc != RFC_OK) {
        errorcall(R_NilValue, "Problem in RfcDestroyFuncton (%s): %d / %s / %s\n",
        							CHAR(STRING_ELT(func,0)),
                                    errorInfo.code,
                                    CHAR(u16to8(errorInfo.key)),
                                    CHAR(u16to8(errorInfo.message)));
    }

    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}


/* called from .onUnload */
SEXP RSAPTerm(void)
{
    return R_NilValue;
}


static const R_CallMethodDef CallEntries[] = {
    {"RSAPRFCConnect", (DL_FUNC) &RSAPRFCConnect, 1},
    {"RSAPValidHandle", (DL_FUNC) &RSAPValidHandle, 1},
    {"RSAPInvoke", (DL_FUNC) &RSAPInvoke, 3},
    {"RSAPClose", (DL_FUNC) &RSAPClose, 1},
    {"RSAPGetInfo", (DL_FUNC) &RSAPGetInfo, 1},
    {"RSAPTerm", (DL_FUNC) &RSAPTerm, 0},
    {NULL, NULL, 0}
};


void R_init_RSAP(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


