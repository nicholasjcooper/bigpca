\name{thin}
\alias{thin}
\title{Reduce one dimension of a large matrix in a strategic way}
\description{
Thin the rows (or columns) of a large matrix or big.matrix to reduce the size
while retaining important information. Supports multiple methods including
uniform sampling, correlation-based selection, PCA-based selection, and
phenotype association methods.
}
\usage{
thin(bigMat, keep = 0.05, how = c("uniform", "correlation", "pca", "association"),
     dir = "", rows = TRUE, random = TRUE, hi.cor = TRUE, least = TRUE,
     pref = "thin", verbose = FALSE, ret.obj = TRUE, delete.existing = TRUE, ...)
}
\arguments{
\item{bigMat}{A big.matrix object, or any argument accepted by get.big.matrix().}
\item{keep}{Proportion or number of rows/columns to keep.}
\item{how}{Method to use: "uniform", "correlation", "pca", or "association".}
\item{dir}{Directory containing the filebacked.big.matrix.}
\item{rows}{Logical, whether to thin rows (TRUE) or columns (FALSE).}
\item{random}{Logical, whether to use random or uniform selection.}
\item{hi.cor}{Logical, for correlation method: most (TRUE) or least (FALSE) correlated.}
\item{least}{Logical, for association method: least (TRUE) or most (FALSE) associated.}
\item{pref}{Prefix for output files.}
\item{verbose}{Logical, whether to display extra information.}
\item{ret.obj}{Logical, whether to return a big.matrix object.}
\item{delete.existing}{Logical, whether to overwrite existing files.}
\item{...}{Additional arguments passed to selection methods.}
}
\value{
A thinned big.matrix with fewer rows or columns than the original.
}
\seealso{
\code{\link{uniform.select}}, \code{\link{subpc.select}}, 
\code{\link{subcor.select}}, \code{\link{select.least.assoc}},
\code{\link{big.select}}
}
\examples{
\dontrun{
orig.dir <- getwd(); setwd(tempdir())
bmat <- generate.test.matrix(5, big.matrix=TRUE)
thinned <- thin(bmat, keep=0.5, how="uniform")
setwd(orig.dir)
}
}
\keyword{array}
\keyword{manip}
