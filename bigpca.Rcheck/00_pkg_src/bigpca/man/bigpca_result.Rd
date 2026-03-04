\name{bigpca_result}
\alias{bigpca_result}
\alias{print.bigpca_result}
\alias{$.bigpca_result}
\alias{[[.bigpca_result}
\title{PCA Result Object}
\description{
S3 class for storing PCA/SVD results with convenient accessors for
scores (PCs), eigenvalues (Evalues), and loadings.
}
\usage{
\method{print}{bigpca_result}(x, ...)
\method{$}{bigpca_result}(x, name)
\method{[[}{bigpca_result}(x, i, ...)
}
\arguments{
\item{x}{A bigpca_result object.}
\item{name}{Name of element to extract (supports aliases like "PCs", "Evalues").}
\item{i}{Index or name of element to extract.}
\item{...}{Additional arguments (not used).}
}
\details{
The bigpca_result object provides convenient accessors:
\itemize{
\item \code{$PCs} and \code{$scores} both return the principal component scores
\item \code{$Evalues} and \code{$eigenvalues} both return the eigenvalues
\item \code{$loadings} returns the loadings if computed
}
}
\value{
\item{scores}{PC scores matrix}
\item{eigenvalues}{Eigenvalues vector}
\item{loadings}{Loadings matrix (if requested)}
\item{center}{Centering information}
\item{metadata}{Run metadata including method used}
}
\examples{
\dontrun{
result <- big.PCA(bigMat)
print(result)
scores <- result$scores  # or result$PCs
eigenvalues <- result$eigenvalues  # or result$Evalues
}
}
\keyword{classes}
