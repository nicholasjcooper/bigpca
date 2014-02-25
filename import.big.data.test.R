#source("~/github/bigmisc/bigHelpers.R")
library(testthat)
library(bigmisc)


test.size <- 5 # try increasing this number for larger matrices
big.d <- 10^(test.size-2); bigger.d <- (10^(test.size-1))-1
M <- matrix(runif(10^test.size),ncol=big.d) # normal matrix
write.table(M,sep="\t",col.names=F,row.names=F,file="functest.txt",quote=F) # no dimnames
rown <- paste("rs",sample(10:99,nrow(M),replace=T),sample(big.d:bigger.d,nrow(M)),sep="")
coln <- paste("ID",sample(1:9,ncol(M),replace=T),sample(big.d:bigger.d,ncol(M)),sep="")
r.fn <- "rownames.txt"; c.fn <- "colnames.txt"
Mdn <- M; colnames(Mdn) <- coln; rownames(Mdn) <- rown
Mtest1 <- M; # expected result when no dimnames given
colnames(Mtest1) <- paste("col",1:ncol(Mtest1),sep="")
rownames(Mtest1) <- paste("row",1:nrow(Mtest1),sep="")
Mtest2 <- Mdn # expected result with correct dimnames
write.table(Mdn,sep="\t",col.names=T,row.names=T,file="functestdn.txt",quote=F) # with dimnames
prv.large(Mdn)
writeLines(paste(as.vector(M)),con="funclongcol.txt")
in.fn <- "functest.txt"
### IMPORTING SIMPLE 1 FILE MATRIX ##
writeLines(rown,r.fn); writeLines(coln,c.fn)
#1. import without specifying row/column names
ii <- import.big.data(in.fn); b1 <- get.big.matrix(ii) # SLOWER without dimnames!
#2. import using row/col names from file
ii <- import.big.data(in.fn,
 cols.fn="colnames.txt",rows.fn="rownames.txt"); b2 <- get.big.matrix(ii)
#3. import by passing colnames/rownames as objects
ii <- import.big.data(in.fn,
 col.names=coln,row.names=rown); b3 <- get.big.matrix(ii)
# check all 3 against expected result
t1 <- test_that("SimpleMatrix", {
          expect_equal(as.matrix(b1),Mtest1)
          expect_equal(as.matrix(b2),Mtest2)
          expect_equal(as.matrix(b3),Mtest2) })
### IMPORTING SIMPLE 1 FILE MATRIX ALREADY WITH DIMNAMES ##
#  # run tests 1-3 with: # 
in.fn <- "functestdn.txt"
#1. import without specifying row/column names
t2 <- test_that("WarningRnChange", expect_warning(import.big.data(in.fn)) )
ii <- import.big.data(in.fn); b1 <- get.big.matrix(ii) # SLOWER without dimnames!
#2. import using row/col names from file
ii <- import.big.data(in.fn,
                       cols.fn="colnames.txt",rows.fn="rownames.txt"); b2 <- get.big.matrix(ii)
#3. import by passing colnames/rownames as objects
ii <- import.big.data(in.fn,
                      col.names=coln,row.names=rown); b3 <- get.big.matrix(ii)
# check all 3 against expected result
t3 <- test_that("DimnamesMatrix", {
  expect_equal(as.matrix(b1),Mtest2)
  expect_equal(as.matrix(b2),Mtest2)
  expect_equal(as.matrix(b3),Mtest2) })
### IMPORTING SIMPLE 1 FILE MATRIX ALREADY WITH MISORDERED col DIMNAMES ##
coln2 <- coln; coln <- sample(coln)
# re-run test3 using in.fn with dimnames # should fail with ERROR
t4 <- test_that("ColNamesWrongMatrix", expect_error(import.big.data(in.fn,
                      col.names=coln,row.names=rown)) )
# restore using:  
coln <- coln2
### IMPORTING SIMPLE 1 FILE MATRIX ALREADY WITH MISORDERED row DIMNAMES ##
rown2 <- rown; rown <- sample(rown);
# re-run test3 using in.fn with dimnames
t5 <- test_that("RowNamesWrongWarn", expect_warning(import.big.data(in.fn,
                              col.names=coln,row.names=rown)) )
ii <- import.big.data(in.fn,col.names=coln,row.names=rown); b3 <- get.big.matrix(ii)
# should be equivalent except for row re-ordering
t6 <- test_that("RowNamesReorderMatrix", {
  expect_equal(as.matrix(b3),Mtest2[match(rownames(b3),rownames(Mtest2)),]) })
# restore using: 
rown <- rown2
### IMPORTING SIMPLE 1 FILE LONG by cols ##
in.fn <- "funclongcol.txt"; #rerun 1:3 # nb: 1 should fail!
#1. import without specifying row/column names
t7 <- test_that("LongNoNamePars", expect_error(import.big.data(in.fn)) )
#2. import using row/col names from file
ii <- import.big.data(in.fn,
          cols.fn="colnames.txt",rows.fn="rownames.txt"); b2 <- get.big.matrix(ii)
#3. import by passing colnames/rownames as objects
ii <- import.big.data(in.fn,
          col.names=coln,row.names=rown); b3 <- get.big.matrix(ii)
# check all last 2 against expected result
t8 <- test_that("DimnamesMatrix", {
  expect_equal(as.matrix(b2),Mtest2)
  expect_equal(as.matrix(b3),Mtest2) })

### IMPORTING SIMPLE 1 FILE LONG by rows ##
#  there is no such supported behaviour!

### IMPORTING multifile LONG by rows ##
splF <- factor(rep(c(1,2,3),nrow(M)*c(.1,.5,.4)))
rownL <- split(rown,splF)
Ms <- split(M,splF); Ms <- lapply(Ms,function(X) { dim(X) <- c(length(X)/ncol(M),ncol(M)); X } )
rowfs <- paste("rn",1:length(rownL),".txt",sep="")
infs <- paste("split",1:length(rownL),".txt",sep="")
for(cc in 1:length(rownL)) { writeLines(rownL[[cc]],con=rowfs[cc]) }
for(cc in 1:length(infs)) { writeLines(paste(as.vector((Ms[[cc]]))),con=infs[cc]) }
# [A] read in using three separate files split by rowname groups 
ii <- import.big.data(infs,
 cols.fn="colnames.txt",rows.fn=rowfs); b2 <- get.big.matrix(ii)
ii <- import.big.data(infs,
 col.names=coln,row.names=rownL); b3 <- get.big.matrix(ii)
# check all last 2 against expected result
t9 <- test_that("MultiFileLongRows", {
  expect_equal(as.matrix(b2),Mtest2)
  expect_equal(as.matrix(b3),Mtest2) })
### IMPORTING multifile LONG by cols ##
splF <- factor(rep(c(1,2,3),ncol(M)*c(.1,.5,.4)))
colnL <- split(coln,splF); MM <- as.data.frame(t(M))
Ms2 <- split(MM,splF); Ms2 <- lapply(Ms2,function(X) { X <- t(X); dim(X) <- c(nrow(M),length(X)/nrow(M)); X } )
#lapply(Ms2,prv.large)
colfs <- paste("cn",1:length(colnL),".txt",sep="")
infs <- paste("split",1:length(colnL),".txt",sep="")
for(cc in 1:length(colnL)) { writeLines(colnL[[cc]],con=colfs[cc]) }
for(cc in 1:length(infs)) { writeLines(paste(as.vector((Ms2[[cc]]))),con=infs[cc]) }
# [B] read in using three separate files split by rowname groups
ii <- import.big.data(infs,
 cols.fn=colfs,rows.fn="rownames.txt"); b2 <- get.big.matrix(ii)
ii <- import.big.data(infs,
 col.names=colnL,row.names=rown); b3 <- get.big.matrix(ii)
# check all last 2 against expected result
t10 <- test_that("MultiFileLongCols", {
  expect_equal(as.matrix(b2),Mtest2)
  expect_equal(as.matrix(b3),Mtest2) })
### IMPORTING multifile MATRIX by rows ##
infs <- paste("splitmatR",1:length(colnL),".txt",sep="")
for(cc in 1:length(infs)) { write.table(Ms[[cc]],sep="\t",col.names=F,row.names=F,file=infs[cc],quote=F) }
# test using A
ii <- import.big.data(infs,
                      cols.fn="colnames.txt",rows.fn=rowfs); b2 <- get.big.matrix(ii)
ii <- import.big.data(infs,
                      col.names=coln,row.names=rownL); b3 <- get.big.matrix(ii)
# check all last 2 against expected result
t11 <- test_that("MultiFileMatrixRows", {
  expect_equal(as.matrix(b2),Mtest2)
  expect_equal(as.matrix(b3),Mtest2) })
### IMPORTING multifile MATRIX by cols ##
infs <- paste("splitmatC",1:length(colnL),".txt",sep="")
for(cc in 1:length(infs)) { write.table(Ms2[[cc]],sep="\t",col.names=F,row.names=F,file=infs[cc],quote=F) }
# test using B
ii <- import.big.data(infs,
                      cols.fn=colfs,rows.fn="rownames.txt"); b2 <- get.big.matrix(ii)
ii <- import.big.data(infs,
                      col.names=colnL,row.names=rown); b3 <- get.big.matrix(ii)
# check all last 2 against expected result
t12 <- test_that("MultiFileMatrixCols", {
  expect_equal(as.matrix(b2),Mtest2)
  expect_equal(as.matrix(b3),Mtest2) })

### IMPORTING multifile MATRIX by rows with dimnames ##
dMs <- Ms; dMs2 <- Ms2; 
for (cc in 1:length(colnL)) {
 rownames(Ms[[cc]]) <- rownL[[cc]]; colnames(Ms[[cc]]) <- coln
 rownames(Ms2[[cc]]) <- rown; colnames(Ms2[[cc]]) <- colnL[[cc]]
}
infs <- paste("splitmatRd",1:length(colnL),".txt",sep="")
for(cc in 1:length(infs)) { write.table(Ms[[cc]],sep="\t",file=infs[cc],quote=F) }
# test using A
ii <- import.big.data(infs,
          cols.fn="colnames.txt",rows.fn=rowfs); b2 <- get.big.matrix(ii)
ii <- import.big.data(infs,
          col.names=coln,row.names=rownL); b3 <- get.big.matrix(ii)
# check all last 2 against expected result
t13 <- test_that("MultiFileMatrixDimRows", {
  expect_equal(as.matrix(b2),Mtest2)
  expect_equal(as.matrix(b3),Mtest2) })
### IMPORTING multifile MATRIX by cols with dimnames ##
infs <- paste("splitmatCd",1:length(colnL),".txt",sep="")
for(cc in 1:length(infs)) { write.table(Ms2[[cc]],sep="\t",file=infs[cc],quote=F) }
# test using B
ii <- import.big.data(infs,
                      cols.fn=colfs,rows.fn="rownames.txt"); b2 <- get.big.matrix(ii)
ii <- import.big.data(infs,
                      col.names=colnL,row.names=rown); b3 <- get.big.matrix(ii)
# check all last 2 against expected result
t14 <- test_that("MultiFileMatrixDimCols", {
  expect_equal(as.matrix(b2),Mtest2)
  expect_equal(as.matrix(b3),Mtest2) })
# restore matrices: 
Ms <- dMs; Ms2 <- dMs2
###################
all.passed <- is.null(test_that("AllTestsPassed",{ t1
                            t2
                            t3
                            t4
                            t5
                            t6
                            t7
                            t8
                            t9
                            t10
                            t11
                            t12
                            t13
                            t14}))
## ALLL TESTS PASSED!! ##

if(all.passed) { cat("\n\nAll tests passed successfully!\n\n\n") } else { cat("\n\nAt least 1 test failed\n\n\n") }

all.fn <- c("rownames.txt","colnames.txt","functestdn.txt","funclongcol.txt","functest.txt",
            paste("rn",1:length(rownL),".txt",sep=""),paste("cn",1:length(rownL),".txt",sep=""),
            paste("split",1:length(rownL),".txt",sep=""),
            paste("splitmatCd",1:length(colnL),".txt",sep=""),paste("splitmatRd",1:length(colnL),".txt",sep=""),
            paste("splitmatC",1:length(colnL),".txt",sep=""), paste("splitmatR",1:length(colnL),".txt",sep=""))

## DELETE ALL FILES ##
unlink(all.fn)