\name{RSAP-package}
\alias{RSAP}
\title{SAP RFC Connector for R}

\description{
  Package \pkg{RSAP} implements SAP RFC connectivity for R using the NW RFC SDK

  See the package manual for details of installation and use.

  The project is hosted at https://github.com/piersharding/RSAP
}

\details{
  Enable the use of SAP RFC connectivity to access SAP data, similar to
  connecting to a database.

  con <- RSAPConnect(ashost="nplhost", sysnr="42",
                      client="001", user="developer", 
                      passwd="developer", lang="EN", 
                      trace="1", lcheck="1")

  info = RSAPGetInfo(con)
  print(info)

  parms <- list('BYPASS_BUFFER' = 'X',
                'MAX_ENTRIES' = 50,
                'TABLE_NAME' = 'T005')

  res <- RSAPInvoke(con, "RFC_GET_TABLE_ENTRIES", parms)
  print(res$ENTRIES)
  RSAPClose(con)

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
