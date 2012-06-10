\name{RSAPInvoke}
\alias{RSAPInvoke}
\title{SAP RFC function calls}
\description{
  Open connections to an SAP System for RFC calls
}
\usage{
RSAPInvoke(conn, "funciton_name", parms)
}%"
\arguments{
  \item{conn}{an Open SAP RFC Conneciton handle}
  \item{function name}{The SAP RFC function to call}
  \item{parms}{a named list of parameters to pass into the function call}
}
\details{

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

}

\note{
  Not much to note here.
}

\value{
  Returns true or false
}
\author{
  Piers Harding
}
\seealso{
  \code{\link{RSAPConnect}}, \code{\link{RSAPGetInfo}}, \code{\link{RSAPClose}}
}
\examples{\dontrun{
# Close the connection
RSAPInvoke(conn, "RFC_FUNCTION_NAME", parms)
}}
\keyword{IO}
\keyword{SAP}
