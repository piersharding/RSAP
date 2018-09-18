\name{RSAPReadTable}
\alias{RSAPReadTable}
\alias{readTable}
\title{SAP RFC function calls}
\description{
  Open connections to an SAP System for RFC calls
}
\usage{
RSAPReadTable(con, saptable, options=list(), fields=list(),
                             delimiter=';', nrows=-1, skip=0)
}%"
\arguments{
  \item{con}{an Open SAP RFC Connection handle.}
  \item{saptable}{the Data Dictionary name of a table to read.}
  \item{options}{list of string values of SQL WHERE clause statements to apply to the table select.}
  \item{fields}{a list of column names that you want returned from the table.}
  \item{delimiter}{the column delimiter character. As values in rows returned by the RFC handler are not enquoted, it may be necessary to use a character that is less likely to occur in strings than the default semicolon.}
  \item{nrows}{integer: the maximum number of rows to retrieve. The default indicates all rows to be retrieved.}
  \item{skip}{integer: the number of lines of the table to skip before beginning to retrieve data.}
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
  Depending on the NW RFC version used in the installation of RSAP, there could be limits on the parameters nrows and skip based on their internal types. For example, in some older versions of NW RFC the internal parameter ROWSKIPS used by skip has SAP-type INT4 and is thus limited to 999999.
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
res <- RSAPReadTable(con, "SFLIGHTS2",
                     options=list("CARRID = 'AA'"),
                     fields=list('CARRID', 'CONNID', 'FLDATE',
                                 'PRICE'))
}}
\keyword{IO}
\keyword{SAP}
