\name{applyFilter}
\alias{applyFilter}
\title{Apply an RPM filter to the data}
\description{Restricts downstream analysis to only those guides with a
specified abundance in terms of mapped reads per million}
\usage{
applyFilter(Data, thresh)
}
\arguments{
  \item{Data}{Data (probably should make this a specific class, like "deepn")
  from \code{\link{import}} or \code{\link{importFromDeepn}}.}
  \item{thresh}{Reads per million (RPM) threshold to apply.}
}
\details{
More specifics on filter.
}
\value{
A \code{deepn} object.
}
\author{Patrick Breheny}
\seealso{
\code{\link{import}},
\code{\link{rpm}}
}
\examples{
\dontrun{applyFilter(Data, 3)}
}
}
