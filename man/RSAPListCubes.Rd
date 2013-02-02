\name{RSAPListCubes}
\alias{RSAPListCubes}
\title{SAP RFC function calls}
\description{
  List the available BI Cubes from a connected SAP system
}
\usage{
RSAPListCubes(con)
}%"
\arguments{
  \item{con}{an Open SAP RFC Connection handle}
}
\details{

  \preformatted{
  con <- RSAPConnect(ashost="nplhost", sysnr="42",
                      client="001", user="developer", 
                      passwd="developer", lang="EN", 
                      trace="1", lcheck="1")

  res <- RSAPListCubes(con)

  RSAPClose(con)
  }
}

\note{
  For each cube you can then run RSAPGetCube(con, '<cube name>') to get the details of the cube layout.
}

\value{
  Returns a data.frame of the cube list query
}
\author{
  Piers Harding
}
\seealso{
  \code{\link{RSAPConnect}}, \code{\link{RSAPGetInfo}}, \code{\link{RSAPInvoke}}, \code{\link{RSAPReadTable}}, \code{\link{RSAPClose}}
}
\examples{\dontrun{
# read the NW demo data info cube
  res <- RSAPListCubes(con)
}}
\keyword{IO}
\keyword{SAP}
