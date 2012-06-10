\name{RSAPClose}
\alias{RSAPClose}
\title{SAP RFC Open Connections}
\description{
  Close an open connection to an SAP System
}
\usage{
RSAPClose(conn)
}%"
\arguments{
  \item{conn}{an Open SAP RFC Conneciton handle}
}
\details{
  \code{RSAPClose} closes an RFC connection to a specified SAP system
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
  \code{\link{RSAPConnect}}, \code{\link{RSAPGetInfo}}, \code{\link{RSAPInvoke}}
}
\examples{\dontrun{
# Close the connection
RSAPClose(conn)
}}
\keyword{IO}
\keyword{SAP}
