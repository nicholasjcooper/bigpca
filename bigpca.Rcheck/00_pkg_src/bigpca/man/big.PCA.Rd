\name{big.PCA}
\alias{big.PCA}
\title{PCA/Singular Value Decomposition for big.matrix}
\description{
Performs principal component analysis (PCA) or singular value decomposition (SVD)
on big.matrix objects or regular matrices. The function supports multiple methods
including IRLBA for partial SVD on large matrices.
}
\usage{
big.PCA(bigMat, dir = getwd(), pcs.to.keep = 50, thin = FALSE,
        SVD = TRUE, LAP = FALSE, center = TRUE, save.pcs = FALSE,
        use.bigalgebra = TRUE, pcs.fn = "PCsEVsFromPCA.RData",
        return.loadings = FALSE, verbose = FALSE, delete.existing = TRUE, ...)
}
\arguments{
\item{bigMat}{A big.matrix object, or any argument accepted by get.big.matrix().}
\item{dir}{Directory containing the filebacked.big.matrix.}
\item{pcs.to.keep}{Integer, number of principal components to keep.}
\item{thin}{Logical, whether to apply thinning before PCA.}
\item{SVD}{Logical, whether to use SVD methods (TRUE) or PCA methods (FALSE).}
\item{LAP}{Logical, whether to use La.svd() instead of svd().}
\item{center}{Logical or numeric vector for centering the matrix.}
\item{save.pcs}{Logical, whether to save results to a binary file.}
\item{use.bigalgebra}{Logical, whether to use bigalgebra for acceleration.}
\item{pcs.fn}{Filename for saving PCA results when save.pcs=TRUE.}
\item{return.loadings}{Logical, whether to return loadings.}
\item{verbose}{Logical, whether to display detailed progress.}
\item{delete.existing}{Logical, whether to overwrite intermediate files.}
\item{...}{Additional arguments passed to thin() when thin=TRUE.}
}
\value{
A bigpca_result object containing:
\item{scores}{Principal component scores (also accessible as $PCs)}
\item{eigenvalues}{Eigenvalues (also accessible as $Evalues)}
\item{loadings}{Loadings if return.loadings=TRUE}
\item{center}{Center values if centering was applied}
\item{metadata}{List with method info, dimensions, etc.}
}
\seealso{
\code{\link{thin}}, \code{\link{PC.correct}}, \code{\link{get.big.matrix}}
}
\examples{
\dontrun{
orig.dir <- getwd(); setwd(tempdir())
mat <- matrix(rnorm(200*100), ncol=100)
bmat <- as.big.matrix(mat, backingfile="test.bck",
  descriptorfile="test.dsc", backingpath=getwd())
result <- big.PCA(bmat, pcs.to.keep=10)
setwd(orig.dir)
}
}
\keyword{multivariate}
\keyword{array}
