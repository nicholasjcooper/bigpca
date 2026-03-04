\name{big.t}
\alias{big.t}
\title{Transpose function for big.matrix objects}
\description{
Transpose a big.matrix and return a new filebacked big.matrix.
}
\usage{
big.t(bigMat, dir = NULL, name = "t.bigMat", R.descr = NULL,
    max.gb = NA, verbose = FALSE, tracker = NA, file.ok = TRUE,
    delete.existing = TRUE)
}
\arguments{
\item{bigMat}{A big.matrix or descriptor/filename.}
\item{dir}{Directory for files.}
\item{name}{Base name for output files.}
\item{R.descr}{Name for RData descriptor file.}
\item{max.gb}{Maximum GB to process before flushing.}
\item{verbose}{Whether to display messages.}
\item{tracker}{Whether to use progress bar.}
\item{file.ok}{Whether to accept file paths.}
\item{delete.existing}{Whether to overwrite existing files.}
}
\value{
A transposed big.matrix.
}
\keyword{array}
\keyword{manip}
