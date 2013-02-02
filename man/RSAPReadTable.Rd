\name{RSAPReadTable}
\alias{RSAPReadTable}
\alias{readTable}
\title{SAP RFC function calls}
\description{
  Open connections to an SAP System for RFC calls
}
\usage{
RSAPReadTable(con, saptable, options=list(), fields=list())
}%"
\arguments{
  \item{con}{an Open SAP RFC Conneciton handle}
  \item{saptable}{The Data Dictionary name of a table to read}
  \item{options}{list of string values of SQL WHERE clause statements to apply to the table select}
  \item{fields}{A list of column names that you want returned from the table}
}
\details{
  \preformatted{
  con <- RSAPConnect(ashost="nplhost", sysnr="42",
                      client="001", user="developer", 
                      passwd="developer", lang="EN", 
                      trace="1", lcheck="1")

  res <- RSAPReadTable(con, "SFLIGHT2")
  # or use alias
  # res <- readTable(con, "SFLIGHT2")

  print(res)

  RSAPClose(con)
  }

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
res <- RSAPReadTable(con, "SFLIGHTS2", options=list("CARRID = 'AA'"), fields=list('CARRID', 'CONNID', 'FLDATE', 'PRICE'))
}}
\keyword{IO}
\keyword{SAP}
