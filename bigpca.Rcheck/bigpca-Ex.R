pkgname <- "bigpca"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('bigpca')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("PC.correct")
### * PC.correct

flush(stderr()); flush(stdout())

### Name: PC.correct
### Title: Correct a big.matrix by principle components
### Aliases: PC.correct

### ** Examples

orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
if(file.exists("testMyBig.bck")) { unlink(c("testMyBig.bck","testMyBig.dsc")) }
mat2 <- sim.cor(500,200,genr=function(n){ (runif(n)/2+.5) })
bmat2 <- as.big.matrix(mat2,backingfile="testMyBig.bck",
 descriptorfile="testMyBig.dsc",  backingpath = getwd())
## calculate PCA ##
rm(bmat2) 
unlink(c("testMyBig.bck","testMyBig.dsc"))
setwd(orig.dir) # reset working dir to original



cleanEx()
nameEx("big.PCA")
### * big.PCA

flush(stderr()); flush(stdout())

### Name: big.PCA
### Title: PCA/Singular Value Decomposition for big.matrix
### Aliases: big.PCA

### ** Examples

# create an example matrix and its transpose
min.dim <- 200; nvar <- 500; subset.size <- 50
mat <- matrix(rnorm(min.dim*nvar),ncol=min.dim) 
prv.large(mat)
t.mat <- t(mat)
# create two alternative covariance matrices
MMs <- t.mat %*% mat
MsM <- mat %*% t.mat
# run singular value decomposition
pca <- svd(mat)   
D <- pca$d # singular values (=sqrt(eigenvalues))
V <- pca$v # right singular vector
U <- pca$u # left singular vector
sig <- mat-mat; diag(sig) <- D; 
MMs2 <- V %*% (t(sig) %*% sig) %*% t(V)
sig <- t.mat-t.mat; diag(sig) <- D; 
MsM2 <- U %*% (sig %*% t(sig)) %*% t(U)
# show that the covariance matrices are equal to the functions of 
# the left and right singular vectors
prv(MMs,MsM); prv(MMs2,MsM2)
pr <- princomp(mat) # PCA using eigendecomposition of cov matrix
L <- matrix(rep(0,40000),ncol=200); diag(L) <- pr[[1]]^2 # eigenvalues as diag
mat2 <- (pr[[2]]) %*% L %*%  solve(pr[[2]]) # = eigenvectors * eigenvalues * inv(eigenvectors)
prv.large(cov(mat)); prv.large(mat2) #  == COVmat (may be slight tolerance differences)
## Now demonstrate the correlation between SVD and PCA ##
# the right singular vector is highly correlated with the pca loadings:
median(abs(diag(cor(V,pr[["loadings"]]))))
# the left singular vector is highly correlated with the pca scores (eigenvectors):
median(abs(diag(cor(U,pr[["scores"]]))))
cor(pr$sdev,D) # the singular values are equivalent to the eigenvalues

## MAIN EXAMPLES ##
orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
if(file.exists("testMyBig.bck")) { unlink(c("testMyBig.bck","testMyBig.dsc")) }
bmat <- as.big.matrix(mat,backingfile="testMyBig.bck",
 descriptorfile="testMyBig.dsc",  backingpath = getwd())
result <- big.PCA(bmat) #,verbose=TRUE)
headl(result)
# plot the eigenvalues with a linear fit line and elbow placed at 13
Eigv <- pca.scree.plot(result$Evalues,M=bmat,elbow=6,printvar=FALSE)
rm(bmat) 
unlink(c("testMyBig.bck","testMyBig.dsc"))
##  generate some data with reasonable intercorrelations ##
mat2 <- sim.cor(500,200,genr=function(n){ (runif(n)/2+.5) })
bmat2 <- as.big.matrix(mat2,backingfile="testMyBig2.bck",
 descriptorfile="testMyBig2.dsc",  backingpath = getwd())
# calculate PCA on decreasing subset size 
result2 <- big.PCA(bmat2,thin=FALSE)
normal <- result2$PCs; rm(result2)
result3 <- big.PCA(bmat2,thin=TRUE,keep=.5, pref="t1")
thinned <- result3$PCs; rm(result3)
result4 <- big.PCA(bmat2,thin=TRUE,keep=.5, pref="t2", how="cor")
corred <- result4$PCs; rm(result4)
result5 <- big.PCA(bmat2,thin=TRUE,keep=.5, pref="t3", how="pca")
pced <- result5$PCs; rm(result5)
result6 <- big.PCA(bmat2,thin=TRUE,keep=.2, pref="t4")
thinner <- result6$PCs; rm(result6)
## correlate the resulting PCs with the un-thinned PCs
cors.thin.with.orig <- apply(cor(normal,thinned),1,max)
cors.corred.with.orig <- apply(cor(normal,corred),1,max)
cors.pced.with.orig <- apply(cor(normal,pced),1,max)
cors.thinner.with.orig <-apply(cor(normal,thinner),1,max)
plot(cors.thin.with.orig,type="l",col="red",ylim=c(0,1))
lines(cors.thinner.with.orig,col="orange")
lines(cors.corred.with.orig,col="lightblue")
lines(cors.pced.with.orig,col="lightgreen")
# can see that the first component is highly preserved,
# and next components, somewhat preserved; try using different thinning methods
rm(bmat2) 
unlink(c("testMyBig2.bck","testMyBig2.dsc"))
setwd(orig.dir)



cleanEx()
nameEx("big.algebra.install.help")
### * big.algebra.install.help

flush(stderr()); flush(stdout())

### Name: big.algebra.install.help
### Title: Attempt to install the bigalgebra package
### Aliases: big.algebra.install.help

### ** Examples




cleanEx()
nameEx("big.select")
### * big.select

flush(stderr()); flush(stdout())

### Name: big.select
### Title: Select a subset of a big.matrix
### Aliases: big.select

### ** Examples

orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
if(file.exists("sel.bck")) { unlink(c("sel.bck","sel.dsc")) }
bmat <- generate.test.matrix(5,big.matrix=TRUE)
# take a subset of the big.matrix without using deepcopy
sel <- big.select(bmat,c(1,2,8),c(2:10),
 deepC=FALSE,verbose=TRUE, delete.existing=TRUE)
prv.big.matrix(sel)
# now select the same subset using row/column names from text files
writeLines(rownames(bmat)[c(1,2,8)],con="bigrowstemp.txt")
writeLines(colnames(bmat)[c(2:10)],con="bigcolstemp.txt")
sel <- big.select(bmat, "bigrowstemp.txt","bigcolstemp.txt", delete.existing=TRUE, pref="sel2")
prv.big.matrix(sel)
rm(bmat)
rm(sel)  
unlink(c("bigcolstemp.txt","bigrowstemp.txt","sel.RData","sel2.bck","sel2.dsc"))
setwd(orig.dir) # reset working dir to original



cleanEx()
nameEx("big.t")
### * big.t

flush(stderr()); flush(stdout())

### Name: big.t
### Title: Transpose function for big.matrix objects
### Aliases: big.t

### ** Examples

orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
if(file.exists("test.bck")) { unlink(c("test.bck","test.dsc")) }
bM <- filebacked.big.matrix(200, 500,
       dimnames = list(paste("r",1:200,sep=""), paste("c",1:500,sep="")),
       backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
bM[1:200,] <- replicate(500,rnorm(200))
prv.big.matrix(bM)
tbM <- big.t(bM,verbose=TRUE)
prv.big.matrix(tbM)
rm(tbM)
rm(bM)  
unlink(c("t.bigMat.RData","t.bigMat.bck","t.bigMat.dsc","test.bck","test.dsc"))
setwd(orig.dir)



cleanEx()
nameEx("bigpca-package")
### * bigpca-package

flush(stderr()); flush(stdout())

### Name: bigpca-package
### Title: PCA, Transpose and Multicore Functionality for 'big.matrix'
###   Objects
### Aliases: bigpca-package bigpca
### Keywords: package manip multivariate IO array

### ** Examples

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



cleanEx()
nameEx("bmcapply")
### * bmcapply

flush(stderr()); flush(stdout())

### Name: bmcapply
### Title: A multicore 'apply' function for big.matrix objects
### Aliases: bmcapply

### ** Examples

orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
if(file.exists("test.bck")) { unlink(c("test.bck","test.dsc")) }
# set up a toy example of a big.matrix (functions most relevant when matrix is huge)
bM <- filebacked.big.matrix(20, 50,
       dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
       backingfile = "test9.bck",  backingpath = getwd(), descriptorfile = "test9.dsc")
bM[1:20,] <- replicate(50,rnorm(20))
prv.big.matrix(bM)
# compare native bigmemory column-wise function to multicore [native probably faster]
v1 <- colsd(bM) # native bigmemory function
v2 <- bmcapply(bM,2,sd,n.cores=2) # use up to 2 cores if available
print(all.equal(v1,v2))
# compare row-means approaches
v1 <- rowMeans(as.matrix(bM))
v2 <- bmcapply(bM,1,mean,n.cores=2) # use up to 2 cores if available
v3 <- bmcapply(bM,1,rowMeans,use.apply=FALSE)
print(all.equal(v1,v2)); print(all.equal(v2,v3))
# example using a custom combine function; taking the mean of column means
weight.means.to.scalar <- function(...) { X <- list(...); mean(unlist(X)) }
v1 <- bmcapply(bM, 2, sd, combine.fn=weight.means.to.scalar)
v2 <- mean(colsd(bM))
print(all.equal(v1,v2))
## note that this function works with normal matrices, however, multicore
# operation is only likely to benefit speed when operations take more than 10 seconds
# so this function will mainly help using large matrices or intensive functions
test.size <- 5 # try increasing this number, or use more intensive function than sd()
# to test relative speed for larger matrices
M <- matrix(runif(10^test.size),ncol=10^(test.size-2)) # normal matrix
system.time(bmcapply(M,2,sd,n.cores=2)) # use up to 2 cores if available
system.time(apply(M,2,sd)) # 
rm(bM) 
unlink(c("test9.bck","test9.dsc"))
setwd(orig.dir)



cleanEx()
nameEx("clear_active_bms")
### * clear_active_bms

flush(stderr()); flush(stdout())

### Name: clear_active_bms
### Title: Function to clear big.matrix objects in the calling environment
### Aliases: clear_active_bms

### ** Examples

clear_active_bms(ignore.os=TRUE, only.culprits=FALSE, list.only=TRUE) # list those in memory



cleanEx()
nameEx("estimate.eig.vpcs")
### * estimate.eig.vpcs

flush(stderr()); flush(stdout())

### Name: estimate.eig.vpcs
### Title: Estimate the variance percentages for uncalculated eigenvalues
### Aliases: estimate.eig.vpcs

### ** Examples

nsamp <- 100; nvar <- 300; subset.size <- 25; elbow <- 6
mat <- matrix(rnorm(nsamp*nvar),ncol=nsamp) 
# or use: # mat <- crimtab-rowMeans(crimtab) ; subset.size <- 10 # crimtab centred
prv.large(mat)
pca <- svd(mat,nv=subset.size,nu=0) # calculates subset of V, but all D
require(irlba)
pca2 <- irlba(mat,nv=subset.size,nu=0) # calculates subset of V & D
pca3 <- princomp(mat,cor=TRUE) # calculates all
# number of eigenvalues for svd is the smaller dimension of the matrix
eig.varpc <- estimate.eig.vpcs(pca$d^2,M=mat)$variance.pcs
cat("sum of all eigenvalue-variances=",sum(eig.varpc),"\n")
print(eig.varpc[1:elbow])
# number of eigenvalues for irlba is the size of the subset if < min(dim(M))
eig.varpc <- estimate.eig.vpcs((pca2$d^2)[1:subset.size],M=mat)$variance.pcs
print(eig.varpc[1:elbow])  ## using 1/x model, underestimates total variance
eig.varpc <- estimate.eig.vpcs((pca2$d^2)[1:subset.size],M=mat,linear=TRUE)$variance.pcs
print(eig.varpc[1:elbow])  ## using linear model, closer to exact answer
eig.varpc <- estimate.eig.vpcs((pca3$sdev^2),M=mat)$variance.pcs
print(eig.varpc[1:elbow])  ## different analysis, but fairly similar var.pcs



cleanEx()
nameEx("generate.test.matrix")
### * generate.test.matrix

flush(stderr()); flush(stdout())

### Name: generate.test.matrix
### Title: Generate a test matrix of random data
### Aliases: generate.test.matrix

### ** Examples

orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
mat <- (generate.test.matrix(5)); prv(mat)
lst <- (generate.test.matrix(5,3,big.matrix=TRUE,file.name="bigtest"))
mat <- lst[[1]]; prv(mat); headl(lst[2:3]); 
unlink(unlist(lst[2:3]))
setwd(orig.dir) # reset working dir to original



cleanEx()
nameEx("get.big.matrix")
### * get.big.matrix

flush(stderr()); flush(stdout())

### Name: get.big.matrix
### Title: Retrieve a big.matrix object
### Aliases: get.big.matrix

### ** Examples

# set up a toy example of a big.matrix 
orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
if(file.exists("test.bck")) { unlink(c("test.bck","test.dsc")) }
bM <- filebacked.big.matrix(20, 50,
       dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
       backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
bM[1:20,] <- replicate(50,rnorm(20))
# Now have a big matrix which can be retrieved using this function in 4 ways:
d.bM <- describe(bM)
save(d.bM,file="fn.RData")
bM1 <- get.big.matrix("test.dsc")
bM2 <- get.big.matrix(d.bM)
bM3 <- get.big.matrix("fn.RData")
bM4 <- get.big.matrix(bM)
prv.big.matrix(bM)
prv.big.matrix(bM1)
prv.big.matrix(bM2)
prv.big.matrix(bM3)
prv.big.matrix(bM4)
rm(bM) 
unlink(c("fn.RData","test.bck","test.dsc"))
setwd(orig.dir)



cleanEx()
nameEx("import.big.data")
### * import.big.data

flush(stderr()); flush(stdout())

### Name: import.big.data
### Title: Load a text file into a big.matrix object
### Aliases: import.big.data

### ** Examples

orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
# Collate all file names to use in this example #
all.fn <- c("rownames.txt","colnames.txt","functestdn.txt","funclongcol.txt","functest.txt",
 paste("rn",1:3,".txt",sep=""),paste("cn",1:3,".txt",sep=""),
 paste("split",1:3,".txt",sep=""),
 paste("splitmatCd",1:3,".txt",sep=""),paste("splitmatRd",1:3,".txt",sep=""),
 paste("splitmatC",1:3,".txt",sep=""), paste("splitmatR",1:3,".txt",sep=""))
any.already <- file.exists(all.fn)
if(any(any.already)) { 
 warning("files already exist in the working directory with the same names as some example files") }
# SETUP a test matrix and reference files # 
test.size <- 4 # try increasing this number for larger matrices
M <- matrix(runif(10^test.size),ncol=10^(test.size-2)) # normal matrix
write.table(M,sep="\t",col.names=FALSE,row.names=FALSE,
 file="functest.txt",quote=FALSE) # no dimnames
rown <- paste("rs",sample(10:99,nrow(M),replace=TRUE),sample(10000:99999,nrow(M)),sep="")
coln <- paste("ID",sample(1:9,ncol(M),replace=TRUE),sample(10000:99999,ncol(M)),sep="")
r.fn <- "rownames.txt"; c.fn <- "colnames.txt"
Mdn <- M; colnames(Mdn) <- coln; rownames(Mdn) <- rown
# with dimnames
write.table(Mdn,sep="\t",col.names=TRUE,row.names=TRUE,file="functestdn.txt",quote=FALSE) 
prv.large(Mdn)
writeLines(paste(as.vector(M)),con="funclongcol.txt")
in.fn <- "functest.txt"

### IMPORTING SIMPLE 1 FILE MATRIX ##
writeLines(rown,r.fn); writeLines(coln,c.fn)
#1. import without specifying row/column names
ii <- import.big.data(in.fn); prv.big.matrix(ii) # SLOWER without dimnames!
#2. import using row/col names from file
ii <- import.big.data(in.fn,cols.fn="colnames.txt",rows.fn="rownames.txt", pref="p1")
prv.big.matrix(ii)
#3. import by passing colnames/rownames as objects
ii <- import.big.data(in.fn, col.names=coln,row.names=rown, pref="p2")
prv.big.matrix(ii)

### IMPORTING SIMPLE 1 FILE MATRIX WITH DIMNAMES ##
#1. import without specifying row/column names, but they ARE in the file
in.fn <- "functestdn.txt"
ii <- import.big.data(in.fn, pref="p3"); prv.big.matrix(ii)

### IMPORTING SIMPLE 1 FILE MATRIX WITH MISORDERED rownames ##
rown2 <- rown; rown <- sample(rown);
# re-run test3 using in.fn with dimnames
ii <- import.big.data(in.fn, col.names=coln,row.names=rown, pref="p4")
prv.big.matrix(ii)
# restore rownames: 
rown <- rown2

### IMPORTING SIMPLE 1 FILE LONG FORMAT by columns ##
in.fn <- "funclongcol.txt"; #rerun test 2 # 
ii <- import.big.data(in.fn,cols.fn="colnames.txt",rows.fn="rownames.txt", pref="p5")
prv.big.matrix(ii)

### IMPORTING multifile LONG by cols ##
# create the dataset and references
splF <- factor(rep(c(1:3),ncol(M)*c(.1,.5,.4)))
colnL <- split(coln,splF); MM <- as.data.frame(t(M))
Ms2 <- split(MM,splF)
Ms2 <- lapply(Ms2,
   function(X) { X <- t(X); dim(X) <- c(nrow(M),length(X)/nrow(M)); X } )
# preview Ms2 - not run # lapply(Ms2,prv.large)
colfs <- paste("cn",1:length(colnL),".txt",sep="")
infs <- paste("split",1:length(colnL),".txt",sep="")
# create multiple column name files and input files
for(cc in 1:length(colnL)) { writeLines(colnL[[cc]],con=colfs[cc]) }
for(cc in 1:length(infs)) { 
  writeLines(paste(as.vector((Ms2[[cc]]))),con=infs[cc]) }
  
# Now test the import using colnames and rownames lists
ii <- import.big.data(infs, col.names=colnL,row.names=rown, pref="p6")
prv.big.matrix(ii)

### IMPORTING multifile MATRIX by rows ##
# create the dataset and references
splF <- factor(rep(c(1,2,3),nrow(M)*c(.1,.5,.4)))
rownL <- split(rown,splF)
Ms <- split(M,splF)
Ms <- lapply(Ms,function(X) { dim(X) <- c(length(X)/ncol(M),ncol(M)); X } )
# preview Ms - not run # lapply(Ms,prv.large)
# create multiple row name files and input files
rowfs <- paste("rn",1:length(rownL),".txt",sep="")
for(cc in 1:length(rownL)) { writeLines(rownL[[cc]],con=rowfs[cc]) }
infs <- paste("splitmatR",1:length(colnL),".txt",sep="")
for(cc in 1:length(infs)) { 
 write.table(Ms[[cc]],sep="\t",col.names=FALSE,row.names=FALSE,file=infs[cc],quote=FALSE) }
 
# Now test the import using colnames and rownames files
ii <- import.big.data(infs, col.names="colnames.txt",rows.fn=rowfs, pref="p7")
prv.big.matrix(ii)

# DELETE ALL FILES ##
unlink(all.fn[!any.already]) # prevent deleting user's files
## many files to clean up! ##
unlink(c("funclongcol.bck","funclongcol.dsc","functest.bck","functest.dsc",
 "functestdn.RData","functestdn.bck","functestdn.dsc","functestdn_file_rowname_list_check_this.txt",
 "split1.bck","split1.dsc","splitmatR1.bck","splitmatR1.dsc", paste0("p",2:7)))
setwd(orig.dir) # reset working dir to original



cleanEx()
nameEx("pca.scree.plot")
### * pca.scree.plot

flush(stderr()); flush(stdout())

### Name: pca.scree.plot
### Title: Make scree plots for any PCA
### Aliases: pca.scree.plot

### ** Examples

require(irlba)
nsamp <- 100; nvar <- 300; subset.size <- 25; elbow <- 6
mat <- matrix(rnorm(nsamp*nvar),ncol=nsamp) 
#this gives the full solution
pca <- svd(mat,nv=subset.size,nu=0)
pca2 <- irlba(mat,nv=subset.size,nu=0)
# show alternate fits for linear versus 1/x fit
pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=100,add.fit.line=TRUE,
               min.dim=min(dim(mat)),linear=TRUE, elbow=6, ylim=c(0,1400))
pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=100,add.fit.line=TRUE,
              min.dim=min(dim(mat)),linear=FALSE, elbow=40, ylim=c(0,1400))
subset.size <- 75
pca2 <- irlba(mat,nv=subset.size,nu=0)
pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=100,add.fit.line=TRUE,
              min.dim=min(dim(mat)),linear=TRUE, elbow=6, ylim=c(0,1400))
pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=100,add.fit.line=TRUE,
              min.dim=min(dim(mat)),linear=FALSE, elbow=40, ylim=c(0,1400))



cleanEx()
nameEx("prv.big.matrix")
### * prv.big.matrix

flush(stderr()); flush(stdout())

### Name: prv.big.matrix
### Title: Tidier display function for big matrix objects
### Aliases: prv.big.matrix

### ** Examples

orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
if(file.exists("test.bck")) { unlink(c("test.bck","test.dsc")) }
bM <- filebacked.big.matrix(20, 50,
       dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
       backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
bM[1:20,] <- replicate(50,rnorm(20))
prv.big.matrix(bM)
prv.big.matrix(bM,rows=10,cols=4)
rm(bM) 
unlink(c("test.dsc","test.bck"))  # clean up files
setwd(orig.dir)



cleanEx()
nameEx("quick.elbow")
### * quick.elbow

flush(stderr()); flush(stdout())

### Name: quick.elbow
### Title: Quickly estimate the 'elbow' of a scree plot (PCA)
### Aliases: quick.elbow

### ** Examples

# correlated data
mat <- sim.cor(100,50)
result <- princomp(mat)
eig <- result$sdev^2
elb.a <- quick.elbow(eig)
pca.scree.plot(eig,elbow=elb.a,M=mat) 
elb.b <- quick.elbow(eig,low=.05) # decrease 'low' to select more components
pca.scree.plot(eig,elbow=elb.b,M=mat) 
# random (largely independent) data, usually higher elbow #
mat2 <- generate.test.matrix(5,3)
result2 <- princomp(mat2)
eig2 <- result2$sdev^2
elb2 <- quick.elbow(result2$sdev^2)
pca.scree.plot(eig2,elbow=elb2,M=mat2)



cleanEx()
nameEx("quick.pheno.assocs")
### * quick.pheno.assocs

flush(stderr()); flush(stdout())

### Name: quick.pheno.assocs
### Title: Quick association tests for phenotype
### Aliases: quick.pheno.assocs

### ** Examples

bmat <- generate.test.matrix(5,big.matrix=TRUE)
pheno <- rep(1,ncol(bmat)); pheno[which(runif(ncol(bmat))<.5)] <- 2
ids <- colnames(bmat); samp.inf <- data.frame(phenotype=pheno); rownames(samp.inf) <- ids
both <- quick.pheno.assocs(bmat,samp.inf); prv(both)
Fs <- quick.pheno.assocs(bmat,samp.inf,verbose=TRUE,p.values=FALSE); prv(Fs)
Ps <- quick.pheno.assocs(bmat,samp.inf,F.values=FALSE); prv(Ps)



cleanEx()
nameEx("select.least.assoc")
### * select.least.assoc

flush(stderr()); flush(stdout())

### Name: select.least.assoc
### Title: Select subset of rows least associated with a categorical
###   variable
### Aliases: select.least.assoc

### ** Examples

bmat <- generate.test.matrix(5,big.matrix=TRUE)
pheno <- rep(1,ncol(bmat)); pheno[which(runif(ncol(bmat))<.5)] <- 2
most.correl <- select.least.assoc(bmat,phenotype=pheno,least=FALSE)
least.correl <- select.least.assoc(bmat,phenotype=pheno,least=TRUE)
cor(bmat[least.correl,][1,],pheno)  # least correlated
cor(bmat[most.correl,][1,],pheno)  # most correlated



cleanEx()
nameEx("subcor.select")
### * subcor.select

flush(stderr()); flush(stdout())

### Name: subcor.select
### Title: Selection of the most correlated variable subset
### Aliases: subcor.select

### ** Examples

mat <- matrix(rnorm(200*2000),ncol=200)
bmat <- as.big.matrix(mat)
ii1 <- subcor.select(bmat,.05,rows=TRUE) # thin down to 5% of the rows
ii2 <- subcor.select(bmat,45,rows=FALSE) # thin down to 45 columns
prv(ii1,ii2)
# show that rows=T is equivalent to rows=F of the transpose (random must be FALSE)
ii1 <- subcor.select(mat,.4,rows=TRUE,random=FALSE)
ii2 <- subcor.select(t(mat),.4,rows=FALSE,random=FALSE)
print(all.equal(ii1,ii2))



cleanEx()
nameEx("subpc.select")
### * subpc.select

flush(stderr()); flush(stdout())

### Name: subpc.select
### Title: Selection of a representative variable subset
### Aliases: subpc.select

### ** Examples

mat <- matrix(rnorm(200*2000),ncol=200) # normal matrix
bmat <- as.big.matrix(mat)              # big matrix
ii <- subpc.select(bmat,.05,rows=TRUE) # thin down to 5% of the rows
ii <- subpc.select(bmat,45,rows=FALSE) # thin down to 45 columns
# show that rows=T is equivalent to rows=F of the transpose (random must be FALSE)
ii1 <- subpc.select(mat,.4,rows=TRUE,random=FALSE)
ii2 <- subpc.select(t(mat),.4,rows=FALSE,random=FALSE)
print(all.equal(ii1,ii2))



cleanEx()
nameEx("svn.bigalgebra.install")
### * svn.bigalgebra.install

flush(stderr()); flush(stdout())

### Name: svn.bigalgebra.install
### Title: Attempt to install the bigalgebra package using SVN
### Aliases: svn.bigalgebra.install

### ** Examples




cleanEx()
nameEx("thin")
### * thin

flush(stderr()); flush(stdout())

### Name: thin
### Title: Reduce one dimension of a large matrix in a strategic way
### Aliases: thin

### ** Examples

orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
if(file.exists("thin.bck")) { unlink(c("thin.bck","thin.dsc")) }
bmat <- generate.test.matrix(5,big.matrix=TRUE)
prv.big.matrix(bmat)
# make 5% random selection:
lmat <- thin(bmat, pref="th2")
prv.big.matrix(lmat)
# make 10% most orthogonal selection (lowest correlations):
lmat <- thin(bmat,.10,"cor",hi.cor=FALSE, pref="th3")
prv.big.matrix(lmat)
# make 10% most representative selection:
lmat <- thin(bmat,.10,"PCA",ret.obj=FALSE, pref="th4") # return file name instead of object
print(lmat)
prv.big.matrix(lmat)
# make 25% selection most correlated to phenotype
# create random phenotype variable
pheno <- rep(1,ncol(bmat)); pheno[which(runif(ncol(bmat))<.5)] <- 2
lmat <- thin(bmat,.25,"assoc",phenotype=pheno,least=FALSE,verbose=TRUE, pref="th5")
prv.big.matrix(lmat)
# tidy up temporary files:
rm(lmat) 
unlink(c("thin.bck","thin.dsc","thin.RData",paste0("th",2:5)))
setwd(orig.dir)



cleanEx()
nameEx("uniform.select")
### * uniform.select

flush(stderr()); flush(stdout())

### Name: uniform.select
### Title: Derive a subset of a large dataset
### Aliases: uniform.select

### ** Examples

mat <- matrix(rnorm(200*100),ncol=200)  # standard matrix
bmat <- as.big.matrix(mat)              # big.matrix
ii1 <- uniform.select(bmat,.05,rows=TRUE) # thin down to 5% of the rows
ii2 <- uniform.select(bmat,45,rows=FALSE,random=TRUE) # thin down to 45 columns
prv(ii1,ii2)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
