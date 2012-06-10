\name{RSAPGetInfo}
\alias{RSAPGetInfo}
\title{SAP RFC get connection details regarding partner SAP system}
\description{
  get connection details regarding partner SAP system
}
\usage{
RSAPGetInfo(conn)
}%"
\arguments{
  \item{conn}{an Open SAP RFC Conneciton handle}
}
\details{
  \code{RSAPGetInfo} get connection details regarding partner SAP system
}

\note{
  Not much to note here.
}

\value{
  Returns a named list of details about the connection
}
\author{
  Piers Harding
}
\seealso{
  \code{\link{RSAPConnect}}, \code{\link{RSAPClose}}, \code{\link{RSAPInvoke}}
}
\examples{\dontrun{
# Close the connection
info <- RSAPGetInfo(conn)
print(info)
}}
\keyword{IO}
\keyword{SAP}
