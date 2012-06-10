\name{RSAPConnect}
\alias{RSAPConnect}
\title{SAP RFC Open Connections}
\description{
  Open connections to an SAP System for RFC calls
}
\usage{
RSAPConnect(ashost = "sap.host.name", user = "username", passwd = "<secret>", ...)

or

RSAPConnect('sap.yml')

}%"
\arguments{
  \item{...}{all SAP connection parameters for the NW RFC SDK}
  \item{'sap.yml'}{A YAML encoded file containing NW RFC SDK conneciton parameters.}
}
\details{
  \code{RSAPConnect} establishes an RFC connection to the specified SAP system
}

\note{
  Not much to note here.
}

\value{
  Returns an object that contains the RFC connection object that you can
  then use to pass to RSAPClose, RSAPInvoke, and RSAPGetInfo.
}
\author{
  Piers Harding
}
\seealso{
  \code{\link{RSAPClose}}, \code{\link{RSAPGetInfo}}, \code{\link{RSAPInvoke}}
}
\examples{\dontrun{
# full parameter specification
conn <- RSAPConnect(ashost="nplhost", sysnr="42", client="001", user="developer", passwd="developer", lang="EN", trace="1", lcheck="1")

# Use a YAML encoded parameter file
conn <- RSAPConnect("sap.yml")
}}
\keyword{IO}
\keyword{SAP}
