# file RSAP/R/RSAP.R
# copyright (C) 1999-2009  M. Lapsley and B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#  Low level wrappers for odbc driver
#
#
#
.onLoad <- function(libname, pkgname)
{
    if(is.null(getOption("dec")))
        options(dec = Sys.localeconv()["decimal_point"])
}

.onUnload <- function(libpath)
{
    .Call(C_RSAPTerm)
    library.dynam.unload("RSAP", libpath)
}


RSAPConnect <- function (...)
{
    args <- list(...)
    if (length(args) == 0) {
        stop("No arguments supplied")
    }
    if (typeof(args[[1]]) == "list") {
        args = args[[1]]
    }
    # did we get passed a config file?
    if (typeof(args[[1]]) == "character" && file.exists(args[[1]])) {
        library(yaml)
        config <- yaml.load_file(args[[1]])
        newargs <- list()
        for (x in names(config)) { newargs[[x]] <- as.character(config[[x]]); }
        return(RSAPConnect(newargs))
    }
    # format client
    if (exists("client", where=args)) {
        args[['client']] <- sprintf("%03d", as.integer(args[['client']]))
    }
    # format sysnr
    if (exists("sysnr", where=args)) {
        args[['sysnr']] <- sprintf("%02d", as.integer(args[['sysnr']]))
    }
    res <- .Call(C_RSAPRFCConnect, args)
    return(res)
}

RSAPshowArgs <-
    function(...)
{
    res <- .Call(C_RSAPshowArgs, list(...))
    #warning(res)
    return(res)
}

RSAPValidHandle <-  function(handle)
{
    if (!is.integer(handle)) {
        print("handle is not an integer")
        return(FALSE)
    }
    if (!is.element("handle_ptr", names(attributes(handle)))) {
        print("handle_ptr does not exist")
        return(FALSE)
    }
    res <- .Call(C_RSAPValidHandle, handle)
    return(res)
}


RSAPGetInfo <- function(handle)
{
    if(!RSAPValidHandle(handle))
       stop("argument is not a valid RSAP handle")
    res <- .Call(C_RSAPGetInfo, handle)
    return(res)
}


RSAPInvoke <- function(handle, func, parms)
{
    if(!RSAPValidHandle(handle))
       stop("argument is not a valid RSAP handle")
    res <- .Call(C_RSAPInvoke, handle, func, parms)
    return(res)
}


RSAPReadTable <- function(handle, rfc_table, options=list(), fields=list())
{
    if(!RSAPValidHandle(handle))
       stop("argument is not a valid RSAP handle")
    library(reshape)
    parms <- list('DELIMITER' = ';',
              'QUERY_TABLE' = rfc_table,
              'OPTIONS' = list('TEXT' = options),
              'FIELDS' = list('FIELDNAME' = fields)
              )
	res <- RSAPInvoke(handle, "RFC_READ_TABLE", parms)
	flds <- sub("\\s+$", "", res$FIELDS$FIELDNAME)
	data <- data.frame(res$DATA, colsplit(res$DATA$WA, split = ";", names = flds))
	
	for (i in 1:length(flds)) {
		f <- flds[i]
		typ <- res$FIELDS$TYPE[i]
		if (typ == 'N' || typ == 'I' || typ == 'P') {
			data[[f]] <- as.numeric(unlist(lapply(data[[f]], FUN=function (x) {sub("[^\\d\\.\\-\\,]", "", x)})));
		} else {
		    data[[f]] <- sub("\\s+$", "", data[[f]]);
		}
	}
	data$WA <- NULL
    return(data)
}


close.RSAP <- function(con, ...) RSAPClose(con)

RSAPClose <- function(handle)
{
    if(!RSAPValidHandle(handle))
       stop("argument is not a valid RSAP handle")
    res <- .Call(C_RSAPClose, handle)
    return(res)
}
