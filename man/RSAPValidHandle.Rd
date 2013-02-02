\name{RSAPValidHandle}
\alias{RSAPValidHandle}
\title{SAP RFC check valid connection}
\description{
  Check a connection handle is valide
}
\usage{
RSAPValidHandle(con)
}%"
\arguments{
      \item{con}{an SAP RFC Connection handle}
}
\details{
  \code{RSAPValidHandle} check a connection handle to SAP is valid
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
RSAPValidHandle(con)
}}
\keyword{IO}
\keyword{SAP}
