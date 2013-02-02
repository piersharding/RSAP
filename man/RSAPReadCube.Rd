\name{RSAPReadCube}
\alias{RSAPReadCube}
\alias{readCube}
\title{SAP RFC function calls}
\description{
  Open connections to an SAP System for RFC calls
}
\usage{
RSAPReadCube(con, cube, ref_date, chars=list(), kfigures=list(), options=list())
}%"
\arguments{
  \item{con}{an Open SAP RFC Conneciton handle}
  \item{cube}{The technical name of an infocube to read}
  \item{ref_date}{The reference date for data selction from the infocube}
  \item{chars}{list of characteristic technical names that you want in the result set}
  \item{kfigures}{A list of key figure technical names that you want in the result set}
  \item{options}{A list of options and their selection criteria based on the technical names of attributes with a syntax like ABAP SELECT-OPTIONS}
}
\details{

  \preformatted{
  con <- RSAPConnect(ashost="nplhost", sysnr="42",
                      client="001", user="developer", 
                      passwd="developer", lang="EN", 
                      trace="1", lcheck="1")

  res <- RSAPReadCube(con, "0D_NW_T01", "20120716", 
                            chars=list("0D_NW_SORG", "0D_NW_PROD"),
                            kfigures=list("0D_NW_NETV"),
                            options=list(CHANM=list('0D_NW_SORG'),SIGN=list('I'), COMPOP=list('EQ'), LOW=list('1514')))
  # or alias
  # res <- readCube(con, ...

  print(res)

  RSAPClose(con)
  }
}

\note{
  You can run the RSAPListCubes(con) to get a list of cubes to query.  For each cube you can then run RSAPGetCube(con, '<cube name>') to get the details of the cube layout.
}

\value{
  Returns a data.frame of the info cube query
}
\author{
  Piers Harding
}
\seealso{
  \code{\link{RSAPConnect}}, \code{\link{RSAPGetInfo}}, \code{\link{RSAPInvoke}}, \code{\link{RSAPReadTable}}, \code{\link{RSAPClose}}
}
\examples{\dontrun{
# read the NW demo data info cube
  res <- RSAPReadCube(con, "0D_NW_T01", "20120716",chars=list("0D_NW_SORG", "0D_NW_PROD"), kfigures=list("0D_NW_NETV"))
}}
\keyword{IO}
\keyword{SAP}
