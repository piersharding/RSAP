\name{RSAPConnect}
\alias{RSAPConnect}
\title{SAP RFC Open Connections}
\description{
  Open connections to an SAP System for RFC calls
}
\usage{
RSAPConnect(...)

}%"
\arguments{
  \item{...}{all SAP connection parameters for the NW RFC SDK}
}
\details{
  \code{RSAPConnect} establishes an RFC connection to the specified SAP system.
  There are two styles of passing the connection parameters:
  - \code{RSAPConnect('sap.yml')} where the name of A YAML encoded file containing
    NW RFC SDK connection parameters is passed in
  - \code{RSAPConnect(ashost = "sap.host.name", user = "username", passwd = "<secret>", ...)}
    The individual connection parameters are passed as per the requirements of the NW RFC SDK.
    These parameters are typically:
     * ashost - the host name of SAP or a SAP Router string
     * sysnr - The SAP system number - relates to the port or service number
     * user - SAP login user that is RFC enabled
     * passwd - user password
     * lang - login language
     * lcheck - login check on connection - don't wait until the first call
     * trace - activate the NW RFC SDK tracing facility - will produce log files
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
con <- RSAPConnect(ashost="nplhost", sysnr="42", client="001", user="developer", passwd="developer", lang="EN", trace="1", lcheck="1")

# Use a YAML encoded parameter file
con <- RSAPConnect("sap.yml")
}}
\keyword{IO}
\keyword{SAP}
