\name{PC.correct}
\alias{PC.correct}
\title{Correct a big.matrix by principal components}
\description{
Remove variance captured by leading principal components from a dataset.
Useful for batch effect correction and removing confounding variation.
}
\usage{
PC.correct(pca.result, bigMat, dir = getwd(), num.pcs = 9, n.cores = 1,
         pref = "corrected", big.cor.fn = NULL, write = FALSE,
         sample.info = NULL, correct.sex = FALSE, add.int = FALSE,
         preserve.median = FALSE, tracker = TRUE, verbose = TRUE,
         delete.existing = TRUE)
}
\arguments{
\item{pca.result}{PCA result from big.PCA() or path to saved result.}
\item{bigMat}{big.matrix with corresponding samples to those in PCA.}
\item{dir}{Directory containing the big.matrix backing file.}
\item{num.pcs}{Number of PCs to correct for.}
\item{n.cores}{Number of cores for parallel processing.}
\item{pref}{Prefix for output file names.}
\item{big.cor.fn}{Explicit filename for corrected output.}
\item{write}{Whether to write result to file or return descriptor.}
\item{sample.info}{Data frame with phenotype information.}
\item{correct.sex}{Whether to include sex covariate in correction.}
\item{add.int}{Whether to preserve original means.}
\item{preserve.median}{Whether to preserve median instead of mean.}
\item{tracker}{Whether to display progress bar.}
\item{verbose}{Whether to display verbose output.}
\item{delete.existing}{Whether to remove existing files.}
}
\value{
A big.matrix descriptor pointing to the PC-corrected matrix.
}
\seealso{\code{\link{big.PCA}}}
\examples{
\dontrun{
result <- big.PCA(bmat)
corrected <- PC.correct(result, bmat, num.pcs=5)
}
}
\keyword{array}
\keyword{manip}
