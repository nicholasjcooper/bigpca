\name{bigpca-package}
\alias{bigpca-package}
\alias{bigpca}
\docType{package}
\title{
PCA, Transpose and Multicore Functionality for 'big.matrix' Objects
}
\description{
This package adds wrappers to add functionality for big.matrix objects 
 (see the bigmemory project). This allows fast scalable principle components analysis (PCA), or 
 singular value decomposition (SVD). There are also functions for transposing, using multicore 'apply'
  functionality, data importing, and for compact display of big.matrix objects. Most functions
 also work for standard matrices if RAM is sufficient.
}
\details{
\tabular{ll}{
Package: \tab bigpca\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2017-11-17\cr
License: \tab GPL (>= 2)\cr
}
The bigmemory project has provided a useful new data structure 'big.matrix', which allows
fast and efficient access to an object that is only limited by disk-space and not RAM capacity.
This package provides wrappers to extend the library of functions available for big.matrix objects.
The focus of this package are functions for multicore functionality and Principle Components 
Analysis (PCA)/Singular Value Decomposition (SVD). bmcapply() works similarly to mcapply but is for big.matrix
objects. There is a transpose function (which is not super-fast, but can be run with multiple cores
to improve speed).
There are several functions dedicated to PCA/SVD. These operations still require a large amount of RAM for large matrices, but the speed is greatly increased and there are useful tools allowing PCA/SVD of much larger 
matrices than would be feasible otherwise. There are also functions for determining the 'elbow' of the data,
 making scree plots, estimating variance explained for incomplete sets of eigenvalues, and for using the derived principle components for correction of a dataset. The PC correction algorithm is fast and can be run with 
multiple cores simultaneously.
There is also a new function prv.big.matrix() for compactly previewing large matrices, and get.big.matrix() for
flexibly retrieving a big.matrix object from a range of different formats.

List of key functions:
\itemize{
 \item{\emph{big.algebra.install.help }}{ install the big algebra package, or provide tips if it fails }
 \item{\emph{big.PCA }}{ PCA or SVD of a big.matrix object}
 \item{\emph{big.select }}{ select a subset of a big.matrix }
 \item{\emph{bmcapply }}{ multicore apply function for big.matrix }
 \item{\emph{estimate.eig.vpcs }}{ estimate uncalculated eigenvalues }
 \item{\emph{generate.test.matrix }}{ easily generate a random dataset for testing/simulation }
 \item{\emph{get.big.matrix }}{ obtain a big.matrix object via several possible methods }
 \item{\emph{import.big.data }}{ import data from text files efficiently into a big.matrix }
 \item{\emph{PC.correct }}{ correct a dataset (big.matrix) for n principle components }
 \item{\emph{pca.scree.plot }}{ draw a scree plot for a PCA / SVD }
 \item{\emph{prv.big.matrix }}{ compact preview for big.matrix objects }
 \item{\emph{quick.elbow }}{ calculate the elbow of a scree plot }
 \item{\emph{quick.pheno.assocs }}{ simple phenotype association test }
 \item{\emph{select.least.assoc }}{ choose subset of big.matrix variables least associated with a phenotype }
 \item{\emph{subcor.select }}{ choose a subset of a big.matrix that is most/least correlated with other variables }
 \item{\emph{subpc.select }}{ choose a subset of a big.matrix that is most representative of the principle components }
 \item{\emph{svn.bigalgebra.install }}{ install the big algebra package from SVN if command is available }
 \item{\emph{big.t }}{ transpose function for big.matrix (can be multicore) }
 \item{\emph{thin }}{ reduce the size of a big.matrix whilst preserving important data relationships }
 \item{\emph{uniform.select }}{ select a random or uniform subset of a big.matrix }
}
}
\author{
Nicholas Cooper

Maintainer: 
Nicholas Cooper <njcooper@gmx.co.uk>
}
\keyword{ package }
\keyword{ manip }
\keyword{ multivariate }
\keyword{ IO }
\keyword{ array }
\seealso{
\code{\link[NCmisc:NCmisc-package]{NCmisc}} ~~
}
\examples{
#' # create a test big.matrix object (file-backed)
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' bM <- filebacked.big.matrix(20, 50,
#'        dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
#'        backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
#' bM[1:20,] <- replicate(50,rnorm(20))
#' prv.big.matrix(bM)
#' # now transpose
#' tbM <- big.t(bM,dir=getwd(),verbose=T)
#' prv.big.matrix(tbM,row=10,col=4)
#' colSDs <- bmcapply(tbM,2,sd,n.cores=10)
#' rowSDs <- bmcapply(bM,1,sd,n.cores=10) # use up to 10 cores if available
#' ##  generate some data with reasonable intercorrelations ##
#' mat <- sim.cor(500,200,genr=function(n){ (runif(n)/2+.5) })
#' bmat <- as.big.matrix(mat)
#' # calculate PCA 
#' result <- big.PCA(bmat)
#' corrected <- PC.correct(result2,bmat)
#' corrected2 <- PC.correct(result2,bmat,n.cores=5)
#' all.equal(corrected,corrected2)
#' rm(tbM); rm(bM);rm(result); 
#' rm(corrected);rm(corrected2); rm(bmat)
#' clear_active_bms() # delete big.matrix objects in memory
#' unlink(c("test.bck","test.dsc"))
#' setwd(orig.dir)
}
