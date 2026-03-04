\name{get.big.matrix}
\alias{get.big.matrix}
\title{Retrieve a big.matrix object}
\description{
Load a big.matrix object from a descriptor file, descriptor object, or
return the object if already a big.matrix.
}
\usage{
get.big.matrix(fn, dir = "", verbose = FALSE)
}
\arguments{
\item{fn}{Description file, descriptor object, big.matrix, or filename.}
\item{dir}{Directory containing the backing file.}
\item{verbose}{Whether to display method information.}
}
\value{
A big.matrix object.
}
\seealso{\code{\link{prv.big.matrix}}}
\examples{
\dontrun{
bm <- get.big.matrix("descriptor.dsc")
}
}
\keyword{IO}
