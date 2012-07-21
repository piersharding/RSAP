\name{RSAP-package}
\alias{RSAP}
\title{SAP RFC Connectivity}

\description{
  Package \pkg{RSAP} implements SAP RFC connectivity.

  See the package manual for details of installation and use.  (This
  will show up as a vignette, and can be accessed \emph{via}
  \code{RShowDoc("RSAP", package="RSAP")}.)
}

\details{
  Enable the use of SAP RFC connectivity to access SAP data, similar to
  connecting to a database.

  conn <- RSAPConnect(ashost="nplhost", sysnr="42",
                      client="001", user="developer", 
                      passwd="developer", lang="EN", 
                      trace="1", lcheck="1")

  info = RSAPGetInfo(conn)
  print(info)

  parms <- list('BYPASS_BUFFER' = 'X',
                'MAX_ENTRIES' = 50,
                'TABLE_NAME' = 'T005')

  res <- RSAPInvoke(conn, "RFC_GET_TABLE_ENTRIES", parms)
  print(res$ENTRIES)
  RSAPClose(conn)

  All RFC table results are returned as data.frame.
}

\author{
  Piers Harding
}
\seealso{
  \code{\link{RSAPConnect}}, \code{\link{RSAPGetInfo}}, \code{\link{RSAPInvoke}}, \code{\link{RSAPReadTable}}, \code{\link{RSAPReadCube}}, \code{\link{RSAPClose}}
}
\keyword{IO}
\keyword{SAP}
