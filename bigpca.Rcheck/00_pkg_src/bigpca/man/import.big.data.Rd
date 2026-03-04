\name{import.big.data}
\alias{import.big.data}
\title{Load a text file into a big.matrix object}
\description{
Fast import of text data into a file-backed big.matrix. Supports various
formats including matrix and long format data.
}
\usage{
import.big.data(input.fn = NULL, dir = getwd(), long = FALSE,
                rows.fn = NULL, cols.fn = NULL, pref = "",
                delete.existing = TRUE, ret.obj = FALSE,
                verbose = TRUE, row.names = NULL, col.names = NULL,
                dat.type = "double", ram.gb = 2, hd.gb = 1000,
                tracker = TRUE)
}
\arguments{
\item{input.fn}{Input file name(s).}
\item{dir}{Directory containing files.}
\item{long}{Whether data is in long format.}
\item{rows.fn}{File with row names.}
\item{cols.fn}{File with column names.}
\item{pref}{Prefix for output files.}
\item{delete.existing}{Whether to overwrite existing files.}
\item{ret.obj}{Whether to return descriptor object.}
\item{verbose}{Whether to display progress.}
\item{row.names}{Vector of row names.}
\item{col.names}{Vector of column names.}
\item{dat.type}{Data type for storage.}
\item{ram.gb}{RAM limit for processing.}
\item{hd.gb}{Hard disk space limit.}
\item{tracker}{Whether to display progress bar.}
}
\value{
Filename of descriptor or descriptor object depending on ret.obj.
}
\seealso{\code{\link{get.big.matrix}}}
\keyword{IO}
