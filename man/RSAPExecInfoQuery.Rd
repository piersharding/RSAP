\name{RSAPExecInfoQuery}
\alias{RSAPExecInfoQuery}
\title{SAP RFC function calls}
\description{
  Execute a call to an Info Query, and return a data.frame of the results
}
\usage{
RSAPExecInfoQuery(con, infoprovider, infoquery)
}%"
\arguments{
  \item{con}{an Open SAP RFC Conneciton handle}
  \item{infoprovider}{The technical name of an infoprovider to read}
  \item{infoquery}{The technical name of an infoquery to read}
}
\details{

  \preformatted{
  con <- RSAPConnect(ashost="nplhost", sysnr="42",
                      client="001", user="developer", 
                      passwd="developer", lang="EN", 
                      trace="1", lcheck="1")

  res <- RSAPExecInfoQuery(con, "0D_NW_M01", "0D_FC_NW_M01_Q0002")

  print(res)

  RSAPClose(con)
  }
}

\note{
  Use transaction RSRT in SAP to find info providers, and queries.
}

\value{
  Returns a data.frame of the info query results
}
\author{
  Piers Harding
}
\seealso{
  \code{\link{RSAPConnect}}, \code{\link{RSAPGetInfo}}, \code{\link{RSAPInvoke}}, \code{\link{RSAPReadTable}}, \code{\link{RSAPClose}}
}
\examples{\dontrun{
# read the NW demo data info cube
  res <- RSAPExecInfoQuery(con, "0D_NW_T01", "20120716",chars=list("0D_NW_SORG", "0D_NW_PROD"), kfigures=list("0D_NW_NETV"))
}}
\keyword{IO}
\keyword{SAP}
