\name{RSAPGetCube}
\alias{RSAPGetCube}
\title{SAP RFC function calls}
\description{
  Open connections to an SAP System for RFC calls
}
\usage{
RSAPGetCube(con, cube)
}%"
\arguments{
  \item{con}{an Open SAP RFC Conneciton handle}
  \item{cube}{The technical name of an infocube to read}
}
\details{

  \preformatted{
  con <- RSAPConnect(ashost="nplhost", sysnr="42",
                      client="001", user="developer", 
                      passwd="developer", lang="EN", 
                      trace="1", lcheck="1")

  res <- RSAPGetCube(con, "0D_NW_T01")

  print(res)

  RSAPClose(con)
  }
}

\note{
  You can run the RSAPListCubes(con) to get a list of cubes to query.
}

\value{
  Returns a data.frame of the info cube structure information
}
\author{
  Piers Harding
}
\seealso{
  \code{\link{RSAPConnect}}, \code{\link{RSAPGetInfo}}, \code{\link{RSAPInvoke}}, \code{\link{RSAPReadTable}}, \code{\link{RSAPClose}}
}
\examples{\dontrun{
# read the NW demo data info cube
  res <- RSAPGetCube(con, "0D_NW_T01")
}}
\keyword{IO}
\keyword{SAP}
