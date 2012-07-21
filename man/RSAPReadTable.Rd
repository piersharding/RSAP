\name{RSAPReadTable}
\alias{RSAPReadTable}
\title{SAP RFC function calls}
\description{
  Open connections to an SAP System for RFC calls
}
\usage{
RSAPReadTable(conn, "table name", options=list(), fields=list())
}%"
\arguments{
  \item{conn}{an Open SAP RFC Conneciton handle}
  \item{table name}{The Data Dictionary name of a table to read}
  \item{options}{list of string values of SQL WHERE clause statements to apply to the table select}
  \item{fields}{A list of column names that you want returned from the table}
}
\details{

  conn <- RSAPConnect(ashost="nplhost", sysnr="42",
                      client="001", user="developer", 
                      passwd="developer", lang="EN", 
                      trace="1", lcheck="1")

  res <- RSAPReadTable(conn, "SFLIGHT2")
  print(res)

  RSAPClose(conn)

}

\note{
  Not much to note here.
}

\value{
  Returns a data.frame of the table contents selected
}
\author{
  Piers Harding
}
\seealso{
  \code{\link{RSAPConnect}}, \code{\link{RSAPGetInfo}}, \code{\link{RSAPInvoke}}, \code{\link{RSAPClose}}
}
\examples{\dontrun{
# read the flight data demo table
res <- RSAPReadTable(conn, "SFLIGHTS2", options=list("CARRID = 'AA'"), fields=list('CARRID', 'CONNID', 'FLDATE', 'PRICE'))
}}
\keyword{IO}
\keyword{SAP}
