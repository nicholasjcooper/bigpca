###NAMESPACE ADDITIONS###
# Depends: R (>= 4.1), grDevices, graphics, stats, utils, reader (>= 1.0.10), NCmisc (>= 1.1.7), bigmemory (>= 4.6.1)
# Imports: parallel, methods, irlba
# Suggests: testthat (>= 3.0.0)
# importFrom(parallel, mclapply)
# importFrom(irlba, irlba)
# import(methods, grDevices, graphics, stats, utils, reader, NCmisc, bigmemory)
###END NAMESPACE###


.onLoad <- function(libname, pkgname) {
  invisible(TRUE)
}

# Internal helpers for modernised PCA workflows --------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}

.bigpca_logger <- function(verbose = FALSE) {
  force(verbose)
  function(level, fmt, ...) {
    level <- toupper(level)
    msg <- if (length(list(...))) sprintf(fmt, ...) else fmt
    switch(
      level,
      ERROR = stop(msg, call. = FALSE),
      WARN = { warning(msg, call. = FALSE); invisible() },
      INFO = { if (verbose) message(sprintf("[INFO] %s", msg)); invisible() },
      DEBUG = { if (verbose) message(sprintf("[DEBUG] %s", msg)); invisible() },
      { if (verbose) message(sprintf("[%s] %s", level, msg)); invisible() }
    )
  }
}

#' Build file paths with optional prefixes and extensions
#'
#' This helper centralises how the package constructs file paths for
#' file-backed big.matrix artefacts while providing optional prefix and
#' extension handling.
#'
#' @param dir Optional directory component.
#' @param fn Optional filename or vector of filenames.
#' @param ... Optional character suffix appended to the basename.
#' @param pref Optional prefix to prepend to the basename.
#' @param ext Optional extension; a leading dot is added if missing.
#' @param must.exist Logical indicating whether the resulting path must exist.
#' @return Character vector of constructed paths.
#' @keywords internal
cat.path <- function(dir = NULL, fn = NULL, ..., pref = NULL, ext = NULL, must.exist = FALSE) {
  pieces <- list(...)
  suffix <- if (length(pieces)) paste0(vapply(pieces, as.character, character(1)), collapse = "") else ""
  base <- if (!is.null(fn)) as.character(fn) else ""
  if (!is.null(pref) && length(pref)) {
    pref <- as.character(pref)
    if (!length(base)) {
      base <- pref
    } else if (length(pref) == 1L || length(pref) == length(base)) {
      base <- paste0(pref, base)
    } else {
      stop("'pref' must have length 1 or match 'fn'.", call. = FALSE)
    }
  }
  if (!length(base)) {
    base <- pref %||% ""
  }
  if (nzchar(suffix)) {
    if (length(base) == 1L) {
      base <- paste0(base, suffix)
    } else {
      base <- paste0(base, rep(suffix, length.out = length(base)))
    }
  }
  if (!is.null(ext) && length(ext)) {
    ext <- as.character(ext)
    ext <- ifelse(substr(ext, 1, 1) == ".", ext, paste0(".", ext))
    if (length(ext) == 1L && length(base) > 1L) {
      ext <- rep(ext, length(base))
    } else if (length(ext) != length(base)) {
      stop("'ext' must have length 1 or match 'fn'.", call. = FALSE)
    }
    base <- paste0(base, ext)
  }
  if (!is.null(dir) && length(dir)) {
    dir <- as.character(dir)
    if (length(dir) == 1L && length(base) > 1L) {
      dir <- rep(dir, length(base))
    } else if (length(dir) != 1L && length(dir) != length(base)) {
      stop("'dir' must have length 1 or match 'fn'.", call. = FALSE)
    }
    path <- file.path(dir, base)
  } else {
    path <- base
  }
  if (isTRUE(must.exist)) {
    missing <- !file.exists(path)
    if (any(missing)) {
      stop(sprintf("Path does not exist: %s", paste(path[missing], collapse = ", ")), call. = FALSE)
    }
  }
  path
}

.bigpca_validate_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("%s must be a non-missing logical scalar.", name), call. = FALSE)
  }
  x
}

.bigpca_validate_numeric_scalar <- function(x, name, lower = -Inf, upper = Inf, allow_na = FALSE, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || (!allow_na && is.na(x))) {
    stop(sprintf("%s must be a numeric scalar.", name), call. = FALSE)
  }
  if (!allow_na && (x < lower || x > upper)) {
    stop(sprintf("%s must be between %s and %s.", name, lower, upper), call. = FALSE)
  }
  if (integer && !is.na(x) && abs(x - round(x)) > .Machine$double.eps^0.5) {
    stop(sprintf("%s must be an integer value.", name), call. = FALSE)
  }
  x
}

.bigpca_resolve_dirs <- function(dir, elements = c("big", "pc")) {
  if (exists("validate.dir.for", mode = "function")) {
    return(do.call("validate.dir.for", list(dir = dir, elements = elements, warn = FALSE)))
  }
  if (is.list(dir) && all(elements %in% names(dir))) {
    return(dir[elements])
  }
  if (!is.character(dir) || length(dir) != 1L) {
    stop(
      sprintf("dir must be a character scalar or a list containing entries for: %s", paste(elements, collapse = ", ")),
      call. = FALSE
    )
  }
  resolved <- dir %||% getwd()
  dirs_list <- as.list(rep(resolved, length(elements)))
  names(dirs_list) <- elements
  dirs_list
}

.bigpca_resolve_input_matrix <- function(x, dirs, logger = .bigpca_logger(FALSE)) {
  if (inherits(x, "big.matrix")) {
    bm <- x
  } else if (inherits(x, "big.matrix.descriptor")) {
    bm <- bigmemory::attach.big.matrix(x)
  } else if (is.character(x) && length(x) == 1L) {
    bm <- get.big.matrix(x, dir = dirs)
  } else if (is.matrix(x)) {
    storage.mode(x) <- "numeric"
    return(list(
      type = "matrix",
      matrix = x,
      nrow = nrow(x),
      ncol = ncol(x),
      rownames = rownames(x),
      colnames = colnames(x)
    ))
  } else {
    stop("Unsupported matrix input. Provide a matrix, big.matrix, descriptor, or descriptor filepath.", call. = FALSE)
  }
  if (!inherits(bm, "big.matrix")) {
    stop("Input is not a valid big.matrix instance.", call. = FALSE)
  }
  list(
    type = "big.matrix",
    bigmatrix = bm,
    nrow = nrow(bm),
    ncol = ncol(bm),
    rownames = rownames(bm),
    colnames = colnames(bm)
  )
}

.bigpca_materialize_matrix <- function(input) {
  if (identical(input$type, "matrix")) {
    mat <- input$matrix
  } else {
    mat <- bigmemory::as.matrix(input$bigmatrix)
  }
  storage.mode(mat) <- "numeric"
  if (!is.null(input$rownames)) {
    rownames(mat) <- input$rownames
  }
  if (!is.null(input$colnames)) {
    colnames(mat) <- input$colnames
  }
  list(
    type = "matrix",
    matrix = mat,
    nrow = nrow(mat),
    ncol = ncol(mat),
    rownames = rownames(mat),
    colnames = colnames(mat)
  )
}

.bigpca_extract_scores <- function(pca_result, dirs, logger = .bigpca_logger(FALSE)) {
  if (inherits(pca_result, "bigpca_result")) {
    return(pca_result$scores)
  }
  if (is.list(pca_result)) {
    candidate <- pca_result[c("scores", "PCs")]
    candidate <- candidate[vapply(candidate, is.matrix, logical(1))]
    if (length(candidate) > 0) {
      return(candidate[[1]])
    }
    matrices <- Filter(is.matrix, pca_result)
    if (length(matrices) > 0) {
      return(matrices[[1]])
    }
  }
  if (is.matrix(pca_result)) {
    return(pca_result)
  }
  if (is.character(pca_result) && length(pca_result) == 1L) {
    candidate_files <- c(pca_result, cat.path(dirs$pc, pca_result))
    file_path <- candidate_files[file.exists(candidate_files)][1]
    if (is.na(file_path)) {
      stop(sprintf("PCA result file '%s' does not exist.", pca_result), call. = FALSE)
    }
    env <- new.env(parent = emptyenv())
    load(file_path, envir = env)
    if (exists("result", envir = env) && inherits(env$result, "bigpca_result")) {
      return(env$result$scores)
    }
    if (exists("PCs", envir = env)) {
      return(get("PCs", envir = env))
    }
    matrices <- Filter(is.matrix, as.list(env))
    if (length(matrices) > 0) {
      return(matrices[[1]])
    }
    stop(sprintf("No PCA score matrix found in '%s'.", file_path), call. = FALSE)
  }
  stop("Unable to interpret 'pca.result'. Provide a bigpca_result object, a matrix of scores, or a saved PCA file.", call. = FALSE)
}

.bigpca_preprocess_matrix <- function(mat, center) {
  if (!is.matrix(mat)) {
    stop("Internal error: expected a numeric matrix for preprocessing.", call. = FALSE)
  }
  storage.mode(mat) <- "numeric"
  n_features <- nrow(mat)
  center_vec <- NULL
  centered <- FALSE
  if (isTRUE(center)) {
    center_vec <- rowMeans(mat, na.rm = TRUE)
    mat <- mat - center_vec
    centered <- TRUE
  } else if (isFALSE(center)) {
    center_vec <- rowMeans(mat, na.rm = TRUE)
  } else if (is.numeric(center)) {
    if (length(center) != n_features) {
      stop("When supplying a numeric centre vector it must have length equal to the number of rows in the input matrix.", call. = FALSE)
    }
    center_vec <- center
    mat <- mat - center_vec
    centered <- TRUE
  } else {
    stop("center must be logical or a numeric vector matching the number of rows in the input matrix.", call. = FALSE)
  }
  if (anyNA(mat)) {
    idx <- which(is.na(mat), arr.ind = TRUE)
    if (centered) {
      mat[idx] <- 0
    } else {
      replacements <- center_vec[idx[, 1]]
      replacements[is.na(replacements)] <- 0
      mat[idx] <- replacements
    }
  }
  list(matrix = mat, center = if (centered) center_vec else NULL, center_values = center_vec, centered = centered)
}

.bigpca_compute_svd <- function(mat, pcs, return_loadings, method = c("irlba", "svd", "lapack", "eigen", "princomp"), use_bigalgebra = TRUE, logger = .bigpca_logger(FALSE)) {
  method <- match.arg(method)
  nU <- if (return_loadings) pcs else 0
  if (identical(method, "princomp")) {
    pr <- stats::princomp(t(mat))
    keep <- min(pcs, ncol(pr$scores))
    scores <- pr$scores[, seq_len(keep), drop = FALSE]
    loadings <- if (return_loadings) unclass(pr$loadings)[, seq_len(keep), drop = FALSE] else NULL
    eigenvalues <- pr$sdev^2
    return(list(scores = scores, loadings = loadings, eigenvalues = eigenvalues, method = "princomp"))
  }
  if (identical(method, "eigen")) {
    xtx <- crossprod(mat)
    eig <- eigen(xtx / nrow(mat), symmetric = TRUE)
    keep <- min(pcs, ncol(eig$vectors))
    scores <- eig$vectors[, seq_len(keep), drop = FALSE]
    eigenvalues <- eig$values
    return(list(scores = scores, loadings = NULL, eigenvalues = eigenvalues, method = "eigen"))
  }
  if (identical(method, "lapack")) {
    svd_res <- La.svd(mat, nu = nU, nv = pcs)
    keep <- min(pcs, ncol(svd_res$vt))
    scores <- t(svd_res$vt)[, seq_len(keep), drop = FALSE]
    loadings <- if (return_loadings && nU > 0) svd_res$u[, seq_len(min(pcs, ncol(svd_res$u))), drop = FALSE] else NULL
    eigenvalues <- svd_res$d^2
    return(list(scores = scores, loadings = loadings, eigenvalues = eigenvalues, method = "lapack"))
  }
  use_irlba <- identical(method, "irlba") && requireNamespace("irlba", quietly = TRUE)
  if (identical(method, "irlba") && !use_irlba) {
    logger("WARN", "irlba package not available; falling back to base::svd.")
  }
  if (use_irlba) {
    irlba_args <- list(A = mat, nv = pcs, nu = nU)
    irlba_formals <- names(formals(irlba::irlba))
    if (use_bigalgebra && "matmul" %in% irlba_formals) {
      irlba_args$matmul <- matmul
    } else if (use_bigalgebra && "mult" %in% irlba_formals) {
      irlba_args$mult <- matmul
    }
    res <- do.call(irlba::irlba, irlba_args)
    keep <- min(pcs, ncol(res$v))
    scores <- res$v[, seq_len(keep), drop = FALSE]
    loadings <- if (return_loadings && !is.null(res$u)) res$u[, seq_len(min(pcs, ncol(res$u))), drop = FALSE] else NULL
    eigenvalues <- res$d^2
    return(list(scores = scores, loadings = loadings, eigenvalues = eigenvalues, method = "irlba"))
  }
  svd_res <- svd(mat, nu = nU, nv = pcs)
  keep <- min(pcs, ncol(svd_res$v))
  scores <- svd_res$v[, seq_len(keep), drop = FALSE]
  loadings <- if (return_loadings && nU > 0) svd_res$u[, seq_len(min(pcs, ncol(svd_res$u))), drop = FALSE] else NULL
  eigenvalues <- svd_res$d^2
  list(scores = scores, loadings = loadings, eigenvalues = eigenvalues, method = "svd")
}

.bigpca_construct_result <- function(scores, eigenvalues, loadings, center = NULL, metadata = list()) {
  res <- list(
    scores = scores,
    eigenvalues = eigenvalues,
    loadings = loadings,
    center = center,
    metadata = metadata
  )
  class(res) <- "bigpca_result"
  res
}

.bigpca_result_alias <- function(name) {
  if (!is.character(name) || length(name) != 1L) {
    return(NULL)
  }
  switch(
    tolower(name),
    pcs = "scores",
    scores = "scores",
    loadings = "loadings",
    evalues = "eigenvalues",
    eigenvalues = "eigenvalues",
    eigenv = "eigenvalues",
    eigenvals = "eigenvalues",
    eigen = "eigenvalues",
    NULL
  )
}

#' @export
`$.bigpca_result` <- function(x, name) {
  alias <- .bigpca_result_alias(name)
  if (!is.null(alias) && alias %in% names(x)) {
    return(x[[alias]])
  }
  underlying <- unclass(x)
  underlying[[name]]
}

#' @export
`[[.bigpca_result` <- function(x, i, ...) {
  if (is.character(i)) {
    alias <- .bigpca_result_alias(i)
    if (!is.null(alias) && alias %in% names(x)) {
      return(x[[alias]])
    }
  }
  NextMethod()
}

#' @export
print.bigpca_result <- function(x, ...) {
  scores <- x$scores
  n_obs <- if (!is.null(dim(scores))) nrow(scores) else length(scores)
  n_comp <- if (!is.null(dim(scores))) ncol(scores) else 0L
  cat("<bigpca_result>\n")
  cat(sprintf(" - observations: %d\n", n_obs))
  cat(sprintf(" - components: %d\n", n_comp))
  if (!is.null(x$loadings)) {
    cat(sprintf(" - features: %d\n", nrow(x$loadings)))
  }
  if (!is.null(x$metadata$method)) {
    cat(sprintf(" - method: %s\n", x$metadata$method))
  }
  invisible(x)
}

# may want to use updated irlba2() function in order to allow bigger Matrices to be pca-ed #

#setMethod("print", signature=signature(x="big.matrix"), function(x,...) prv.big.matrix(x,...))

# internal function for testing: 
# use: keeper <- manage.test.files() at start of example
# then: unlink(bigpca:::manage.test.files(FALSE,keepers)) at end of examples
# then inspect the example output for warnings with unlink()
manage.test.files <- function(start = TRUE, keepers = NULL, path = getwd()) {
  start <- .bigpca_validate_logical_scalar(start, "start")
  if (!dir.exists(path)) {
    stop("The supplied 'path' does not exist.", call. = FALSE)
  }
  fixtures <- c(
    "testMyBig.bck", "t.bigMat.bck", "splitmatR1.bck", "split1.bck", "sel.bck", "functestdn.bck", "functest.bck", "funclongcol.bck", "t.bigMat.RData",
    "sel.RData", "functestdn.RData", "fn.RData", "testMyBig.dsc", "t.bigMat.dsc", "splitmatR1.dsc", "split1.dsc", "sel.dsc", "functestdn_file_rowname_list_check_this.txt",
    "functestdn.dsc", "functest.dsc", "funclongcol.dsc", "bigrowstemp.txt", "bigcolstemp.txt", "test.dsc", "test.bck"
  )
  files_present <- intersect(fixtures, list.files(path))
  if (start) {
    return(files_present)
  }
  keepers <- if (is.null(keepers)) character() else as.character(keepers)
  stale <- setdiff(files_present, keepers)
  if (length(stale)) {
    warning(
      sprintf(
        "Test fixture files remaining in '%s': %s",
        path,
        paste(stale, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  stale
}



#' Function to clear big.matrix objects in the calling environment
#' 
#' New big.memory behavious prevents overwriting of previous big.matrix
#' backing files, and in particular for windows you can only delete the 
#' backing file if the object is also deleted. This finds any big.matrix
#' objects in memory and deletes them (by default only with names in the
#' 'culprits' list)
#'
#' @param ignore.os delete big.matrix objects even if the OS is not windows
#' @param verbose report the names of deleted matrices
#' @param only.culprits delete only matrices named: "bM","bM2","bmat","bmat2","tbM","sel","lmat"
#' @param list.only just list active big.matrix objects rather than delete them
#' @return No return value, but effect is to delete the big.matrix objects in the calling env.
#' @seealso
#' @export
#' @examples
#' clear_active_bms(ignore.os=TRUE, only.culprits=FALSE, list.only=TRUE) # list those in memory
clear_active_bms <- function(ignore.os=FALSE, verbose=TRUE, only.culprits=TRUE, list.only=FALSE) {
    culprits <- c("bM","bM2","bmat","bmat2","tbM","sel","lmat")
    target.env <- parent.env(environment())
    list.out <- NULL
    if(.Platform$OS.type=="windows" | ignore.os) {
        ll <- ls(name= target.env)
        for (cc in 1:length(ll)) {
            if((do.call("is", args=list(get(ll[cc]))))[1]=="big.matrix") { 
                if(ll[cc] %in% culprits | !only.culprits) {
                    if(list.only) {
                        list.out <- c(list.out,ll[cc])
                    } else {
                        if(verbose) { message("removing big.matrix: ",ll[cc]) }
                        do.call("rm",args=list(ll[cc], envir= target.env)) 
                    }
                }
            } 
        }
    }
    if(list.only) { return(list.out) }
}


#' Tidier display function for big matrix objects
#' 
#' This function prints the first and last columns and rows of a big matrix, and
#' a few more than this if desired. Allows previewing of a big.matrix without 
#' overloading the console. 
#'
#' @param bigMat the description file, big.matrix object, or big.matrix.descriptor object, 
#'  anything that can be read by get.big.matrix()
#' @param dir the directory containing the big.matrix backing/description files
#' @param name logical, whether to print a name for the matrix
#' @param dat logical, whether to print any of the matrix contents (overrides row/col)
#' @param descr character, optional name of the description file, which if not null will be displayed
#' @param bck character, optional name of the backing file, which if not null will be displayed
#' @param mem logical, whether to display the amount of memory used by the object
#' @param rows integer, number of rows to display
#' @param cols integer, number of columns to display
#' @param rcap character, caption to display for the rows
#' @param ccap character, caption to display for the columns
#' @param ... additional arguments to prv.large (from NCmisc) which displays the end result
#' @return Prints to console a compact representation of the bigMat matrix, 
#'  with the first few rows and columns, and the last row and column. Note that sometimes
#'  the initial printing of a big.matrix can take a little while. But subsequently the printout
#'  should be almost instantaneous.
#' @seealso \code{\link{get.big.matrix}}
#' @export
#' @examples 
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' if(file.exists("test.bck")) { unlink(c("test.bck","test.dsc")) }
#' bM <- filebacked.big.matrix(20, 50,
#'        dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
#'        backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
#' bM[1:20,] <- replicate(50,rnorm(20))
#' prv.big.matrix(bM)
#' prv.big.matrix(bM,rows=10,cols=4)
#' rm(bM) 
#' unlink(c("test.dsc","test.bck"))  # clean up files
#' setwd(orig.dir)
prv.big.matrix <- function(bigMat,dir="",rows=3,cols=2,name=NULL,dat=TRUE,
                           descr=NULL,bck=NULL,mem=FALSE,rcap="",ccap="",...) {
  # print summary of big matrix contents
  #must.use.package("bigmemory")
  if(is.null(name) & is.character(bigMat)) { name <- basename(bigMat[1]) } # if it's a file name
  bigMat <- get.big.matrix(bigMat,dir=dir)
  if(!is.null(name)) { 
    cat("\nBig matrix; '",name,"', with: ",sep="") 
  } else { cat("Big matrix with: ") }
  nC <- ncol(bigMat); nR <- nrow(bigMat)
  bcap <- paste(" (",c(rcap[1],ccap[1]),")",sep="") 
  if(rcap!="") { rcap <- bcap[1] }; if(ccap!="") { ccap <- bcap[2] }
  cat(nR," rows",rcap,", ",nC," columns",ccap,"\n",sep="") 
  if(is.sub.big.matrix(bigMat)) { cat("[a sub.big.matrix object]\n")}
  cat(" - data type:",is(bigMat[1,1])[1],"\n")
  if(!is.null(descr)) { cat(" - descriptor file;",descr,"\n") }
  if(is.filebacked(bigMat)) { 
    if(!is.null(bck)) {
      cat(" - backing file;",bck,"\n") }
  } else {
    cat(" - not filebacked")
  }
  if(dat) {
    prv.large(bigMat,rows=rows,cols=cols,...)
  } else {
    if(!is.null(colnames(bigMat))) {
      cat(" - columns:",paste(colnames(bigMat)[1:max(rows,cols)],collapse=", "),
          "...",colnames(bigMat)[nC],"\n")
    } else { cat(" - no column names\n") }
    if(!is.null(rownames(bigMat))) {
      cat(" -    rows:",paste(rownames(bigMat)[1:max(rows,cols)],collapse=", "),
          "...",rownames(bigMat)[nR],"\n")  
    } else { cat(" - no row names\n") }
  }
  if(mem) {
    total.datapoints <- nR*nC
    disk.est <- round(estimate.memory(bigMat))
    cat("Total of",total.datapoints,"data-points, using",disk.est,"GB estimated disk space\n")
  }
  cat("\n")
}




#' Estimate the variance percentages for uncalculated eigenvalues
#'
#' If using a function like irlba() to calculate PCA, then you can choose (for speed) 
#' to only calculate a subset of the eigenvalues. So there is no exact percentage of variance explained 
#' by the PCA, or by each component as you will get as output from other routines.
#' This code uses a linear, or b*1/x model, to estimate the AUC for the unknown eigenvalues, providing
#' a reasonable estimate of the variances accounted for by each unknown eigenvalue, and
#' the predicted eigenvalue sum of the unknown eigenvalues.
#'
#' @param eigenv the vector of eigenvalues actually calculated
#' @param min.dim the size of the smaller dimension of the matrix submitted to singular
#'  value decomposition, e.g, number of samples - i.e, the max number of possible eigenvalues,
#'  alternatively use 'M'.
#' @param M optional enter the original dataset 'M'; simply used to derive the dimensions,
#'  alternatively use 'min.dim'.
#' @param elbow the number of components which you think explain the important portion
#'  of the variance of the dataset, so further components are assumed to be reflecting
#'  noise or very subtle effects, e.g, often the number of components used is decided
#'  by the 'elbow' in  a scree plot (see 'pca.scree.plot')
#' @param linear whether to use a linear model to model the 'noise' eigenvalues; alternative
#'  is a 1/x model with no intercept.
#' @param estimated logical, whether to return the estimated variance percentages for unobserved eigenvalues
#'  along with the real data; will also generate a factor describing which values in the returned
#'  vector are observed versus estimated.
#' @param print.est whether to output the estimate result to the console
#' @param print.coef whether to output the estimate regression coefficients to the console
#' @param add.fit.line logical, if there is an existing scree plot, adds the fit line from this estimate
#'  to the plot ('pca.scree.plot' can use this option using the parameter of the same name)
#' @param col colour for the fit line
#' @param ignore.warn ignore warnings when an estimate is not required (i.e, all eigenvalues present)
#' @return By default returns a list where the first element ''variance.pcs' are the known variance
#'  percentages for each eigenvalue based on the estimated divisor, the second element
#'  'tail.auc' is the area under the curve for the estimated eigenvalues. If estimate
#'  =TRUE then a third element is return with separate variance percentages for
#'  each of the estimated eigenvalues.
#' @seealso \code{\link{pca.scree.plot}}
#' @export
#' @examples
#' nsamp <- 100; nvar <- 300; subset.size <- 25; elbow <- 6
#' mat <- matrix(rnorm(nsamp*nvar),ncol=nsamp) 
#' # or use: # mat <- crimtab-rowMeans(crimtab) ; subset.size <- 10 # crimtab centred
#' prv.large(mat)
#' pca <- svd(mat,nv=subset.size,nu=0) # calculates subset of V, but all D
#' require(irlba)
#' pca2 <- irlba(mat,nv=subset.size,nu=0) # calculates subset of V & D
#' pca3 <- princomp(mat,cor=TRUE) # calculates all
#' # number of eigenvalues for svd is the smaller dimension of the matrix
#' eig.varpc <- estimate.eig.vpcs(pca$d^2,M=mat)$variance.pcs
#' cat("sum of all eigenvalue-variances=",sum(eig.varpc),"\n")
#' print(eig.varpc[1:elbow])
#' # number of eigenvalues for irlba is the size of the subset if < min(dim(M))
#' eig.varpc <- estimate.eig.vpcs((pca2$d^2)[1:subset.size],M=mat)$variance.pcs
#' print(eig.varpc[1:elbow])  ## using 1/x model, underestimates total variance
#' eig.varpc <- estimate.eig.vpcs((pca2$d^2)[1:subset.size],M=mat,linear=TRUE)$variance.pcs
#' print(eig.varpc[1:elbow])  ## using linear model, closer to exact answer
#' eig.varpc <- estimate.eig.vpcs((pca3$sdev^2),M=mat)$variance.pcs
#' print(eig.varpc[1:elbow])  ## different analysis, but fairly similar var.pcs
estimate.eig.vpcs <- function(eigenv=NULL,min.dim=length(eigenv),M=NULL,elbow=NA,linear=TRUE,
                              estimated=FALSE,print.est=TRUE,print.coef=FALSE,
                              add.fit.line=FALSE,col="blue",ignore.warn=FALSE) {
  ## if matrix is optionally inputted, calculate the minimum dim automatically
  if(length(eigenv)<2) { warning("was passed insufficient eigenvalues (",length(eigenv),")") ; return(eigenv) }
  if(!is.null(M)) { if(!is.null(dim(M))) { min.dim <- min(dim(M),na.rm=T) } }
  if(all(is.na(min.dim))) { min.dim <- length(eigenv) }
  n.comp <- length(eigenv) # max(c(min.dim,length(eigenv)),na.rm=T)
  if(all(is.na(elbow))) { 
    if(n.comp==min.dim) {
      elbow <- quick.elbow(eigenv)
    } else {
      elbow <- 3 
    }
  }
  elbow <- round(min(n.comp,elbow,na.rm=T)) # make sure not > n.comp
  if(!is.numeric(eigenv)) { warning("eigenv not numeric"); return(NULL) }
  #preview(c("elbow","min.dim","n.comp","eigenv"))
  if(is.na(min.dim) | ((min.dim-n.comp)<2) | ((n.comp-elbow)<(min(20,min.dim/20,na.rm=T))) ) {
    # if most/all eigenvalues already present this is not needed, or if parameters insufficient
    # then don't try to calculate the AUC of the remaining eigenvalues
    if(n.comp==min.dim) {
      if(!ignore.warn) { cat("All eigenvalues present, estimate not required\n") }
      estimated <- F
    } else {
      warning("didn't attempt to estimate eigenvalues as there were",
        " very few unknowns compared to the number of samples,",
        " or not enough eigenvalues between the elbow and 'min.dim'")
    }
    var.pcs <- eigenv[1:n.comp]/(sum(eigenv)); tail.var <- 0
  } else {
    # estimate combined variance of eigenvalues not calculated by irlba using 1/x model
    if(!linear) {
      xx <- 1/(1:length(eigenv))
      ab <- lm(eigenv[elbow:n.comp]~0+xx[elbow:n.comp])$coef
      tail.var <- ((log(min.dim)-log(n.comp))*ab[1]) # integral evaluated
      mod.txt <- "[b/x, no intercept]"
      predy <- ab[1]*(1/c((elbow+1):min.dim))
    } else {
      xx <- 1:length(eigenv)
      ab <- lm(eigenv[elbow:n.comp]~xx[elbow:n.comp])$coef
      zeropoint <- round(ab[1]/abs(ab[2])); zeropoint <- min(c(min.dim,zeropoint),na.rm=T)
      tail.var <- (zeropoint-n.comp)*(ab[1]+((n.comp)*ab[2]))*.5
     # tail.var <- ((ab[1]*(zeropoint-n.comp))+((((n.comp-elbow)^2)-((zeropoint-elbow)^2))*ab[2]*.5)) # integral evaluated
      mod.txt <- "[a + bx]"
      predy <- ab[1]+(ab[2]*c((elbow+1):min.dim))
      predy[(zeropoint-elbow):(min.dim-elbow)] <- 0
    }
    # intercept ignored as the asymptote should theoretically be zero so assumption
    # values > this reflect noise variance that might dissipate as x--> n.samp
    if(print.est) {
      not.calc <- min.dim-length(eigenv)
      cat(" estimate of eigenvalue sum of",not.calc,"uncalculated eigenvalues:",(as.numeric(tail.var)),"\n")
    }
    if(print.coef) {
      cat(" slope",mod.txt,":",as.numeric(tail(ab,1)),"\n")
    }
    if(add.fit.line) {
      # add fitted line to scree plot if one exists
      predx <- c((elbow+1):min.dim); print(predy)
      #prv(c("predx","predy"))
      print(length(predx)); print(length(predy))
      try(lines(predx,predy,col=col),T)
    }
    var.pcs <- eigenv[1:n.comp]/(sum(eigenv)+tail.var)
  }
  if(estimated) {
    var.pcs.comb <- numeric(min.dim); realornot <- rep("estimated",times=min.dim)
    var.pcs.comb[(1+min.dim-length(predy)):min.dim] <- predy/(sum(eigenv)+tail.var)
    var.pcs.comb[1:length(var.pcs)] <- var.pcs
    realornot[1:length(var.pcs)] <- "observed"
    out <- list(var.pcs.comb,tail.var,as.factor(realornot))
    names(out) <- c("variance.pcs","tail.auc","estimated")
  } else {
    out <- list(var.pcs,tail.var)
    names(out) <- c("variance.pcs","tail.auc")
  }
  return(out)
}


#' Make scree plots for any PCA
#'
#' Make a scree plot using eigenvalues from princomp(), prcomp(), svd(), irlba(), big.PCA(), etc.
#' Note that most these return values which need to be squared to be proper eigenvalues.
#' There is also an option to use the estimate.eig.vpcs() function to estimate any missing
#' eigenvalues (e.g, if using a function like irlba' to calculate PCA) and then to visualise
#' the fitline of the estimate on the scree plot.
#'
#' @param eigenv the vector of eigenvalues actually calculated
#' @param elbow the number of components which you think explain the important chunk
#'  of the variance of the dataset, so further components are modelled as reflecting
#'  noise or very subtle effects, e.g, often the number of components used is decided
#'  by the 'elbow' in  a scree plot (see 'pca.scree.plot')
#' @param min.dim the size of the smaller dimension of the matrix submitted to singular
#'  value decomposition, e.g, number of samples - i.e, the max number of possible eigenvalues,
#'  alternatively use 'M'.
#' @param M optional enter the original dataset 'M'; simply used to derive the dimensions,
#'  alternatively use 'min.dim'.
#' @param linear whether to use a linear model to model the 'noise' eigenvalues; alternative
#'  is a 1/x model with no intercept.
#' @param printvar logical, whether to print summary of variance calculations
#' @param add.fit.line logical, if there is an existing scree plot, adds the fit line from this estimate
#'  to the plot ('pca.scree.plot' can use this option using the parameter of the same name)
#' @param n.xax number of components to include on the x-axis
#' @param verbose logical, whether to display additional output
#' @param return.data logical, whether to return the percentages of variance explained for each component, or nothing (just plot)
#' @param ... further arguments to the plot function
#' @return Either a vector of variance percentages explained, or nothing (just a plot), depending on value of 'return.data'
#' @seealso \code{\link{pca.scree.plot}}
#' @export
#' @examples
#' require(irlba)
#' nsamp <- 100; nvar <- 300; subset.size <- 25; elbow <- 6
#' mat <- matrix(rnorm(nsamp*nvar),ncol=nsamp) 
#' #this gives the full solution
#' pca <- svd(mat,nv=subset.size,nu=0)
#  # test with larger and smaller subset, larger gives 1/x better fit, smaller, x
#' pca2 <- irlba(mat,nv=subset.size,nu=0)
#' # show alternate fits for linear versus 1/x fit
#' pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=100,add.fit.line=TRUE,
#'                min.dim=min(dim(mat)),linear=TRUE, elbow=6, ylim=c(0,1400))
#' pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=100,add.fit.line=TRUE,
#'               min.dim=min(dim(mat)),linear=FALSE, elbow=40, ylim=c(0,1400))
#' subset.size <- 75
#' pca2 <- irlba(mat,nv=subset.size,nu=0)
#' pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=100,add.fit.line=TRUE,
#'               min.dim=min(dim(mat)),linear=TRUE, elbow=6, ylim=c(0,1400))
#' pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=100,add.fit.line=TRUE,
#'               min.dim=min(dim(mat)),linear=FALSE, elbow=40, ylim=c(0,1400))
pca.scree.plot <- function(eigenv,elbow=NA,printvar=TRUE,min.dim=NA,M=NULL,add.fit.line=FALSE,
                           n.xax=max(30,length(eigenv)),linear=TRUE,verbose=FALSE,return.data=FALSE,...) 
{
  # do SCREE PLOTS AND calculate EIGENVALUE VARIANCE after a PCA
  if(!is.null(M)) { if(!is.null(dim(M))) { min.dim <- min(dim(M),na.rm=T) } }
  n.comp <- length(eigenv)
  if(all(is.na(elbow))) { 
    if(n.comp==min.dim) {
      elbow <- quick.elbow(eigenv)
    } else {
      elbow <- 3 
    }
  }
  elbow <- round(min(n.comp,elbow,na.rm=T))
  n.xax <- min(n.xax,n.comp) # constrain n.xax to not be greater than number of components
  plot(eigenv[1:n.xax],bty="l",xlab="number of principle components",
       ylab="eigenvalues",bg="green",pch=21,...)
  abline(v=(elbow+.5),lty="dashed")
  legend("topright",legend=c("Principle components","scree plot 'elbow' cutoff"),
         pt.bg=c("green",NA),pch=c(21,NA),lty=c(NA,"dashed"),bty="n")
  scree.calc <- estimate.eig.vpcs(eigenv=eigenv,min.dim=min.dim,elbow=elbow,ignore.warn=!verbose,
                  print.est=T,print.coef=T,add.fit.line=add.fit.line,col="blue",linear=linear)
  if(printvar & verbose) {
    cat(" sum of eigen-variance:",round(sum(eigenv)+scree.calc$tail.auc,2),"\n")
    cat(" variance % estimates: \n ",round(scree.calc$variance.pcs,2),"\n")
  }
  if(return.data) {
    return(scree.calc$variance.pcs)
  } else {
    return(invisible())
  }
}


# quickly choose an elbow for a PC. 
# at variance below 5% per component, choose the largest % drop
# designed for variance percentages, but will also work given a full set of Evalues
#' Quickly estimate the 'elbow' of a scree plot (PCA)
#' 
#' This function uses a rough algorithm to estimate a sensible 'elbow' to
#' choose for a PCA scree plot of eigenvalues. The function looks at an initial arbitrarily 'low'
#' level of variance and looks for the first eigenvalue lower than this. If the very first eigenvalue
#' is actually lower than this (i.e, when the PCs are not very explanatory) then this 'low' value is
#' iteratively halved until this is no longer the case. After starting below this arbitrary threshold
#' the drop in variance explained by each pair of consecutive PCs is standardized by dividing over the 
#' larger of the pair. The largest percentage drop in the series below 'low' % is selected as the 'elbow'.
#' @param varpc numeric, vector of eigenvalues, or 'percentage of variance' explained datapoints for
#'  each principle component. If only using a partial set of components, should first pass to 
#'  estimate.eig.vpcs() to estimate any missing eigenvalues.
#' @param low numeric, between zero and one, the threshold to define that a principle component
#'  does not explain much 'of the variance'.
#' @param max.pc maximum percentage of the variance to capture before the elbow (cumulative sum to PC 'n')
#' @return The number of last principle component to keep, prior to the determined elbow cutoff
#' @export
#' @seealso \code{\link{estimate.eig.vpcs}}
#' @author Nicholas Cooper 
#' @examples
#' # correlated data
#' mat <- sim.cor(100,50)
#' result <- princomp(mat)
#' eig <- result$sdev^2
#' elb.a <- quick.elbow(eig)
#' pca.scree.plot(eig,elbow=elb.a,M=mat) 
#' elb.b <- quick.elbow(eig,low=.05) # decrease 'low' to select more components
#' pca.scree.plot(eig,elbow=elb.b,M=mat) 
#' # random (largely independent) data, usually higher elbow #
#' mat2 <- generate.test.matrix(5,3)
#' result2 <- princomp(mat2)
#' eig2 <- result2$sdev^2
#' elb2 <- quick.elbow(result2$sdev^2)
#' pca.scree.plot(eig2,elbow=elb2,M=mat2)
quick.elbow <- function(varpc,low=.08,max.pc=.9) {
  ee <- varpc/sum(varpc) # ensure sums to 1
  #print(round(log(ee),3))
  while(low>=max(ee)) { low <- low/2 } # when no big components, then adjust 'low'
  lowie <- (ee<low) ; highie <- ee>low/8
  low.ones <- which(lowie & highie)
  others <- length(which(!lowie))
  if(length(low.ones)>0) {
    if(length(low.ones)==1) {
      elbow <- low.ones 
    } else {
      set <- ee[low.ones]
      pc.drops <- abs(diff(set))/(set[1:(length(set)-1)])
      infz <- is.infinite(pc.drops)
      #print(pc.drops)
      elbow <- which(pc.drops==max(pc.drops[!infz],na.rm=T))[1]+others
    }
  } else { 
    # if somehow there are no small eigenvalues, just choose the elbow as the second last
    cat("no eigenvalues were significantly smaller than the previous\n")
    elbow <- length(ee) 
  }
  if(tail(cumsum(ee[1:elbow]),1)>max.pc) {
      elbow <- which(cumsum(ee)>max.pc)[1]-1
  }
  if(elbow<1) {
    warning("elbow calculation failed, return zero")
    return(0)
  }
  names(elbow) <- NULL
  return(elbow)
}




#' A multicore 'apply' function for big.matrix objects
#'
#' # to put into NCmisc
#' Multicore method to run a function for a big.matrix that could be run using 'apply'
#' on a regular matrix (when parameter use.apply=T [default]). Otherwise for a
#' function that might be more efficient done in done in chunks (e.g, utilising vectorised 
#'  functions) use.apply=F can be set so that processing is done on larger submatrices, rather
#' than 1 row/column at a time. Input to specify whether to perform the function
#' row or columnwise is equivalent to 'apply' syntax, 1=by-rows, 2=by-columns.
#' This function is useful for big.matrix processing even without multiple cores, particulary
#' when MARGIN=1 (row-wise). While native colmean, colmin and colsd functions for 
#' big.matrix objects are very fast (and will probably outperform bmcapply even with 1
#'  core versus many), these are only natively implemented for column-wise operations and 
#' the equivalent operations if needing to be row-wise should be faster with bmcapply for
#' matrices larger than available RAM.
#' Can also be used for regular matrices although there is unlikely to be a speed advantage.
#' @seealso \code{\link{get.big.matrix}}
#' @param bigMat the big.matrix object to apply the function upon, can enter as a filename,
#'  description object or any other valid parameter to get.big.matrix(). Can also use with a standard matrix
#' @param MARGIN 1=row-wise, 2=column-wise, see same argument for base:::apply()
#' @param FUN the function to apply, should return a result with 1 dimension that has the
#'  same length as dim(bigMat)[MARGIN]=L; i.e, a vector length L, matrix (L,x) or (x,L) or list[[L]].
#'  Note that using a custom 'combine.fn' parameter might allow exceptions to this.
#' @param dir directory argument for get.big.matrix(), ie. the location of the bigMat backing file if 
#'  not in the current working directory.
#' @param by integer, the number of rows/columns to process at once. The default should work in most
#'  situations however, if the dimension not specified by MARGIN is very large, this might need
#'  to be smaller, or if the function being applied is much more efficient performed 
#'  on a large matrix than several smaller ones then this 'by' parameter should be increased
#'  within memory contraints. You should make sure 'estimate.memory(c(by,dim(bigMat)[-MARGIN]))'
#'  doesn't exceed available RAM.
#' @param n.cores integer, the number of parallel cores to utilise; note that sometimes if a machine
#'  has only a few cores this can result in slower performance by tying up resources
#'  which should be available to perform background and system operations.
#' @param use.apply logical, if TRUE then use the 'apply' function to apply FUN to
#'  each submatrix, or if FALSE, then directly apply FUN to submatrices, which
#'  means that FUN must return results with at least 1 dimension the same as the input, 
#'  or you can use a custom 'combine.fn' parameter to recombine results from submatrices.
#' @param convert logical, only need to change this parameter when use.apply=FALSE. If use are using a 
#'  function that can natively run on big.matrix objectsthen you can increase speed 
#'  by setting convert=FALSE. Most functions will expect a regular matrix
#'   and may fail with a big.matrix, so default convert=TRUE behaviour
#'  will convert submatrices to a regular matrix just before processing.
#' @param combine.fn a custom function to recombine input from sub.matrix processing. Default
#'  combine functions are list(), cbind() and rbind(); so a custom function should
#'  expect the same input as these; ie., a list of unspecified length, which will be
#'  the list of results from parallel calls on submatrices of bigMat, usually of size by*X.
#' @param ... if use.apply=TRUE, then additional arguments for apply(); else additional arguments
#'  for FUN.
#' @return Result depends on the function 'FUN' called, and the parameter 'combine.fn', but if MARGIN=1 usually is a vector 
#'  of length nrow(bigMat), or if MARGIN=2 a vector of length ncol(bigMat).
#' @export
#' @examples
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' if(file.exists("test.bck")) { unlink(c("test.bck","test.dsc")) }
#' # set up a toy example of a big.matrix (functions most relevant when matrix is huge)
#' bM <- filebacked.big.matrix(20, 50,
#'        dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
#'        backingfile = "test9.bck",  backingpath = getwd(), descriptorfile = "test9.dsc")
#' bM[1:20,] <- replicate(50,rnorm(20))
#' prv.big.matrix(bM)
#' # compare native bigmemory column-wise function to multicore [native probably faster]
#' v1 <- colsd(bM) # native bigmemory function
#' v2 <- bmcapply(bM,2,sd,n.cores=2) # use up to 2 cores if available
#' print(all.equal(v1,v2))
#' # compare row-means approaches
#' v1 <- rowMeans(as.matrix(bM))
#' v2 <- bmcapply(bM,1,mean,n.cores=2) # use up to 2 cores if available
#' v3 <- bmcapply(bM,1,rowMeans,use.apply=FALSE)
#' print(all.equal(v1,v2)); print(all.equal(v2,v3))
#' # example using a custom combine function; taking the mean of column means
#' weight.means.to.scalar <- function(...) { X <- list(...); mean(unlist(X)) }
#' v1 <- bmcapply(bM, 2, sd, combine.fn=weight.means.to.scalar)
#' v2 <- mean(colsd(bM))
#' print(all.equal(v1,v2))
#' ## note that this function works with normal matrices, however, multicore
#' # operation is only likely to benefit speed when operations take more than 10 seconds
#' # so this function will mainly help using large matrices or intensive functions
#' test.size <- 5 # try increasing this number, or use more intensive function than sd()
#' # to test relative speed for larger matrices
#' M <- matrix(runif(10^test.size),ncol=10^(test.size-2)) # normal matrix
#' system.time(bmcapply(M,2,sd,n.cores=2)) # use up to 2 cores if available
#' system.time(apply(M,2,sd)) # 
#' rm(bM) 
#' unlink(c("test9.bck","test9.dsc"))
#' setwd(orig.dir)
bmcapply <- function(bigMat,MARGIN,FUN,dir=NULL,by=200,n.cores=1,
                     use.apply=TRUE,convert=!use.apply,combine.fn=NULL,...) {
  # multicore way of calculating a function (e.g, dlrs) for a big.matrix,
  # when use.apply=T, a function that could be done with apply(); when use.apply=F, one that
  # is best done in chunks rather than row by row or col by col
  # can do it column wise (bycol=T, eg for all samples), or row-wise, eg for all snps
  # 'by' is number of rows/cols to process in each chunk
  if(is.null(dir)) { dir <- getwd() } else { if(!file.exists(dir)) { dir <- getwd() } }
  if(!as.numeric(MARGIN)[1] %in% c(1,2)) { stop("MARGIN must be 1=rows, or 2=columns") }
  if(!is.function(FUN)) { stop("FUN must be a function") }
  if(!is.numeric(by)) { by <- 200 } else { by <- round(by) }
  if(n.cores>1 & .Platform$OS.type=="windows") { n.cores <- 1; warning("setting n.cores to 1 because mc.cores is not supported by windows") }
  bycol <- as.logical(as.numeric(MARGIN[1])-1)
  #must.use.package("multicore")
  if(is.null(dim(bigMat))) { stop("this function only works on matrix objects") }
  if(length(dim(bigMat))!=2) { stop("this function only works on matrix objects") }
  if(bycol) { tot.main <- ncol(bigMat) } else { tot.main <- nrow(bigMat)}
  stepz <- round(seq(from=1,to=(tot.main+1),by=by))
  # if the last step is too small, merge with previous
  sc.lst <- head(tail(stepz,2),1); lst <- tail(stepz,1)
  if((tail(stepz,1)) != (tot.main+1)) { stepz <- c(stepz,(tot.main+1)) }
  if(lst-sc.lst <=2) { ll <- length(stepz); if(ll>2) { stepz <- stepz[-(ll-1)] } else { warning("number of rows/columns quite small, may cause issues") } }
  #
  split.to <- length(stepz)-1
  result <- numeric(tot.main)
  ## define the function
  big.fnc <- function(dd,func,stepz,bigMat,dir,bycol=T,...)
  {
    big.type <- is.big.matrix(bigMat)
    x1 <- stepz[dd]; x2 <- stepz[dd+1]-1 #subset row selection
    if(bycol) {
      if(big.type) {
        next.block <- sub.big.matrix(bigMat, firstCol=x1, lastCol=x2, backingpath=dir )
      } else {
        next.block <- bigMat[,x1:x2]
      }
      dm <- 2
    } else {
      if(big.type) {
        next.block <- sub.big.matrix(bigMat, firstRow=x1, lastRow=x2, backingpath=dir )
      } else {
        next.block <- bigMat[x1:x2,]
      }
      dm <- 1
    }
    if(convert || is.null(dim(next.block))) {
      next.block <- bigmemory::as.matrix(next.block)
#      next.block <- next.block[1:nrow(next.block),1:ncol(next.block)]  # conv to standard matrix if req'd
    } else if (use.apply && inherits(next.block, "big.matrix")) {
      next.block <- bigmemory::as.matrix(next.block)
    }
    if(!use.apply) {
      out <- func(next.block,...)
    } else {
      out <- base::apply(next.block, dm, func, ...)
    }
    rm(next.block) ; gc() # remove the sub-matrix pointer each iteration  
    return(out)
  }
  ## run function as mclapply()
  if(split.to>=1) {
    #print("z1")
    result.list <- parallel::mclapply(1:split.to, FUN=big.fnc, func=FUN, stepz=stepz, 
                                       bigMat=bigMat, dir=dir, bycol=bycol, mc.cores=n.cores,...)
    #print("z2")
    if(is.function(combine.fn)) {
      result <- do.call(combine.fn,args=result.list)
    } else {
      comb.fn <- choose.comb.fn(result.list,stepz) # automatically decide best combining function based on largest and most common result
      result <- do.call(comb.fn,args=result.list)
      #result <- unlist(result.list,recursive=unlist.results) 
    }
  } else {
    result <- NULL; warning("matrix had insufficient columns returning NULL")
  }
  return(result)  
}


## internal function
choose.comb.fn <- function(result.list,stepz) {
  ### NB: when NCmisc is a package this should require(NCmisc)
  # choose the best function to combine multicore results from bigmcapply
  ## DEFINE function to choose best based on 1 example result:
  sub.comb.fn <- function(dm,ls,ll) {
    # evaluate output based on dimensions of an example result
    if(ls==ll) { comb.fn <- "c" } else {
      if(!is.null(dm)){
        if(dm[1]==dm[2] & dm[1]==ls) {
          warning("looks like results have dim[1]==dim[2], suggest using 'combine.fn' parameter to obtain desired result")
          comb.fn <- c("list","cbind","rbind")
        } else {
          if(dm[1]==ls | dm[2]==ls) {
            if(dm[2]==ls) {
              comb.fn <- "cbind"
            } else {
              comb.fn <- "rbind"
            }
          } else {
            warning("mapping from function results was not obvious, suggest using 'combine.fn' parameter")
            comb.fn <- "list"
          }
        }
      } else {
        comb.fn <- "list"
      }
    }
    return(comb.fn)
  }
  ## process max and mode results using function defined above
  rawlens <- sapply(result.list,function(x) { length(unlist(x)) })
  #print(is(rawlens)); print(dim(rawlens)); print(head(rawlens))
  lL <- length(rawlens); if(length(rawlens)>1) { rawlens <- rawlens[-lL] }
  max.res <- which(rawlens==max(rawlens,na.rm=T))[1] # choose the largest result to define structure
  mode.res <- which(rawlens==Mode(rawlens,multi=T))[1] # choose the largest result to define structure
  dm <- dim(result.list[[max.res]]); ll <- length(result.list[[max.res]]); ls <- length(stepz[max.res]:stepz[max.res+1])-1
  fn.max <- sub.comb.fn(dm,ls,ll)
  dm <- dim(result.list[[mode.res]]); ll <- length(result.list[[mode.res]]); ls <- length(stepz[mode.res]:stepz[max.res+1])-1
 # print(dm); print(ls); print(ll); print(mode.res); print(max.res)
  fn.mode <- sub.comb.fn(dm,ls,ll)
  if(fn.max[1]!=fn.mode[1]) {
    # resolve any conflict between results based on max or mode
    allf <- c(fn.max,fn.mode); 
    if(length(allf)==4) { 
      fn.max <- Mode(allf) # by chance one had equal dimensions, but this should pick the best by convergence
    } else {
      warning("differential testing of result output from multiple cores gave different structures, suggest using 'combine.fn' to ensure correct output")
    }
  }
  return(fn.max[1])
}


#' Retired bigalgebra installation helpers
#'
#' Legacy helpers for installing \pkg{bigalgebra} from SVN have been removed.
#' The Big Memory stack is now available directly from CRAN, and manual
#' installation instructions are documented in the package README alongside
#' guidance for enabling optimised BLAS libraries when needed.
#'
#' @param ... ignored.
#' @return These helpers are defunct and will raise an error explaining how to
#'   proceed.
#' @export
svn.bigalgebra.install <- function(...) {
  stop(
    "svn.bigalgebra.install() has been retired. See README.md for current bigmemory/BLAS guidance.",
    call. = FALSE
  )
}

#' @rdname svn.bigalgebra.install
#' @export
big.algebra.install.help <- function(...) {
  stop(
    "big.algebra.install.help() has been retired. See README.md for current bigmemory/BLAS guidance.",
    call. = FALSE
  )
}


#' Retrieve a big.matrix object
#'
#' This function can load a big.matrix object using a big.matrix.descriptor object, the
#' name of a description file, the name of a binary file containing a big.matrix.descriptor
#' or if passed a big.matrix object, it will just return that object. Only the object or
#' file name plus the directory containing the backing file are required.
#' @param fn the name of a description file, the name of a binary file containing a
#'  big.matrix.descriptor, a big.matrix object or a big.matrix.descriptor object.
#' @param dir directory containing the backing file (if not the working directory)
#' @param verbose whether to display information on method being used, or minor warnings
#' @return Returns a big.matrix object, regardless of what method was used as reference/input
#' @export
#' @examples 
#' # set up a toy example of a big.matrix 
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' if(file.exists("test.bck")) { unlink(c("test.bck","test.dsc")) }
#' bM <- filebacked.big.matrix(20, 50,
#'        dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
#'        backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
#' bM[1:20,] <- replicate(50,rnorm(20))
#' # Now have a big matrix which can be retrieved using this function in 4 ways:
#' d.bM <- describe(bM)
#' save(d.bM,file="fn.RData")
#' bM1 <- get.big.matrix("test.dsc")
#' bM2 <- get.big.matrix(d.bM)
#' bM3 <- get.big.matrix("fn.RData")
#' bM4 <- get.big.matrix(bM)
#' prv.big.matrix(bM)
#' prv.big.matrix(bM1)
#' prv.big.matrix(bM2)
#' prv.big.matrix(bM3)
#' prv.big.matrix(bM4)
#' rm(bM) 
#' unlink(c("fn.RData","test.bck","test.dsc"))
#' setwd(orig.dir)
get.big.matrix <- function(fn,dir="",verbose=FALSE)
{
  # loads a big.matrix either using an big.matrix description object
  # , or this object in a binary file or text file, or points to a bigmatrix or matrix
  if(all(dir=="")) { dir <- getwd() }
  if(exists("validate.dir.for",mode="function")) {
    ## plumbCNV specific code ##
    dir <- do.call("validate.dir.for",list(dir=dir,elements="big",warn=F))  
    dir.big <- dir$big
  } else {
    # otherwise
    dir.big <- dir
    if(is.list(dir)) { if(!is.null(dir[["big"]])) { dir.big <- dir$big } }
  }
  if(is(fn)[1]=="big.matrix.descriptor")
  {
    bigMat2 <- attach.big.matrix(fn,path=dir.big)
  } else {
    if(is(fn)[1]=="big.matrix" | is(fn)[1]=="matrix")
    {
      if(is(fn)[1]=="matrix") {
        bigMat2 <- as.big.matrix(fn,descriptorfile="TEMPBIG",backingpath=dir.big)
      } else { bigMat2 <- fn }
    } else {
      lastchar <- substr(dir.big,nchar(dir.big),nchar(dir.big))
      if (length(grep(".RData",fn))==0) {
        fn <- basename(fn)
        if(!fn %in% list.files(dir.big)) { 
          stop(paste("Error: big.matrix file '",fn,"' not in 'dir.big'",sep=""))
        }
        if(verbose) { cat(" loading big matrix using text description\n") }
        if(lastchar=="/") { dir.big <- substr(dir.big,1,nchar(dir.big)-1) }
        bigMat2 <- attach.big.matrix(fn,path=dir.big)
      } else {
        if(verbose) { cat(" loading big matrix using RData description\n") }
        if(lastchar!="/") { dir.big <- paste(dir.big,"/",sep="") }
        filenm <- cat.path(dir.big,fn,must.exist=T)
        dscnm <- paste(load(filenm))
        big.fn <- NULL
        for (ii in 1:length(dscnm)) {
          if("big.matrix.descriptor" %in% is(get(dscnm[ii])))
          { big.fn <- dscnm[ii] } 
        }
        if(!is.null(big.fn)) {
          descr <- get(big.fn) 
        } else {
          stop(paste("Error: didn't find bigmatrix descriptor in file",fn))
        }
        bigMat2 <- attach.big.matrix(descr,path=dir.big) 
      }
    }
  }
  return(bigMat2)
}





#' by disk space and not by RAM.
#' @param size 10^size is the total number of datapoints simulated. 6 or less are fairly quick to generate,
#'  while 7 takes a few seconds. 8 will take under a minute, 9 around ten minutes, 10, perhaps over an hour.
#'  Values are coerced to the range of integers c(2:10).
#' @param row.exp similar to 'nrow' when creating a matrix, except this is exponential, giving 10^row.exp rows.
#' @param rand a function, must return 'n' values, when rand(n) is called, eg., rnorm(), runif(), numeric()
#' @param dimnames logical, whether to generate some row and column names
#' @param data.frame logical, whether to return as a data.frame (FALSE means return a matrix)
#' @param big.matrix logical, whether to return as a big.matrix (overrides data.frame). If a file.name
#'  is used then the big.matrix will be filebacked and this function returns a list with a 
#'  a big.matrix, and the description and backing filenames.
#' @param file.name if a character, then will write the result to tab file instead of returning
#'  the object, will return the filename; overrides data.frame. Alternatively, if big.matrix=TRUE,
#'  then this provides the basename for a filebacked big.matrix.
#' @param tracker logical, whether to display a progress bar for large matrices (size>7) where progress will be slow
#' @return Returns a random matrix of data for testing/simulation, can be a data.frame or big.matrix if those options are selected
#' @export
#' @author Nicholas Cooper 
#' @examples 
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' mat <- (generate.test.matrix(5)); prv(mat)
#' lst <- (generate.test.matrix(5,3,big.matrix=TRUE,file.name="bigtest"))
#' mat <- lst[[1]]; prv(mat); headl(lst[2:3]); 
#' unlink(unlist(lst[2:3]))
#' setwd(orig.dir) # reset working dir to original
generate.test.matrix <- function(size=5,row.exp=2,rand=rnorm,dimnames=TRUE,
                                 data.frame=FALSE,big.matrix=FALSE,file.name=NULL, 
                                 tracker=TRUE) {
  test.size <- min(max(2,round(size)),10) # try increasing this number for larger matrices
  dim.dif <- max(min(row.exp,size-2),1)
  big.d <- 10^(test.size-dim.dif); bigger.d <- (10^(test.size-(dim.dif-1)))-1
  other.d <- 10^(dim.dif) ; another.d <- (10^(dim.dif+1))-1
  if(!is.function(rand)) { stop("rand must be a function that generates 'n' simulated values") }
  nr <- (10^test.size)/big.d; nc <- big.d
  if(dimnames) {
    rown <- paste("rs",sample(10:99,nr,replace=T),sample(other.d:another.d,nr),sep="")
    coln <- paste("ID",sample(1:9,nc,replace=T),sample(big.d:bigger.d,nc),sep="")
    dnn <- list(rown,coln)
  } else { dnn <- NULL }
  M <- NULL
  if(big.matrix) {
    if(test.size>7) { if(is.null(file.name)) { file.name="big_test" } }
    # automatically extract the path if one is included in file.name
    if(!is.null(file.name)) {
      if(dirname(file.name)!=".") { pth <- dirname(file.name); file.name <- basename(file.name) } else { pth <- NULL}
      bck <- cat.path(fn=file.name,ext="bck")
      descr <- cat.path(fn=file.name,ext="dsc")
    } else { bck <- descr <- NULL }
    if(test.size>7) {
     M <- big.matrix(nrow=nr,ncol=nc,backingfile=bck,descriptorfile=descr,dimnames=dnn,backingpath=pth)
     if(nr>nc) {
       for (cc in 1:nr) { M[cc,] <- rand(nc); if(tracker) { loop.tracker(cc,nr) }  }
     } else {
       for (cc in 1:nc) { M[,cc] <- rand(nr);  if(tracker) { loop.tracker(cc,nc) } }
     }
    }
  }
  if(is.null(M)) {
    M <- matrix(rand(10^test.size),ncol=nc) # normal matrix
    colnames(M) <- coln; rownames(M) <- rown
  }
  #prv(big.d,bigger.d,other.d,another.d,M)
  if(is.character(file.name) & !big.matrix) {
    write.table(M,sep="\t",col.names=dimnames,row.names=dimnames,file=file.name,quote=F) # no dimnames
    return(file.name)
  } 
  if(data.frame & !big.matrix) {
    return(as.data.frame(M))
  }
  if(big.matrix) {
    if(is.character(file.name)) {
      if(dirname(file.name)!=".") { pth <- dirname(file.name); file.name <- basename(file.name) } else { pth <- getwd() }
      if(!is.big.matrix(M)) { M <- as.big.matrix(M,descriptorfile=descr,backingfile=bck,  backingpath = pth) }
      return(list(M=M,descr=descr,bck=bck))
    } else {
      return(as.big.matrix(M))
    }
  }
  return(M)
}



#' Internal function used by import.big.data
.bigpca_normalise_dirs <- function(dir, elements = c("big", "ano", "col")) {
  if (exists("validate.dir.for", mode = "function")) {
    validated <- do.call("validate.dir.for", list(dir = dir, elements = elements, warn = FALSE))
    if (!is.list(validated)) {
      stop("validate.dir.for must return a list of directories.", call. = FALSE)
    }
    missing <- setdiff(elements, names(validated))
    if (length(missing)) {
      stop(sprintf("validate.dir.for did not provide entries for: %s", paste(missing, collapse = ", ")), call. = FALSE)
    }
    return(validated[elements])
  }
  if (is.null(dir)) {
    dir <- getwd()
  }
  if (is.list(dir)) {
    out <- vapply(elements, function(el) {
      candidate <- dir[[el]]
      if (is.null(candidate) || !nzchar(candidate)) {
        fallback <- dir[[1]] %||% getwd()
        return(fallback)
      }
      candidate
    }, character(1))
  } else if (is.character(dir) && length(dir) >= 1L) {
    base <- dir[1] %||% getwd()
    out <- rep(base, length(elements))
  } else {
    stop("dir must be a character scalar or a named list.", call. = FALSE)
  }
  names(out) <- elements
  as.list(out)
}

.bigpca_import_resolve_data_type <- function(dat.type) {
  dat.type <- tolower(dat.type %||% "double")
  switch(
    dat.type,
    double = list(big = "double", storage = "double", bytes = 8L, readr = readr::col_double()),
    numeric = list(big = "double", storage = "double", bytes = 8L, readr = readr::col_double()),
    integer = list(big = "integer", storage = "integer", bytes = 4L, readr = readr::col_integer()),
    short = list(big = "short", storage = "double", bytes = 8L, readr = readr::col_double()),
    char = list(big = "char", storage = "character", bytes = 1L, readr = readr::col_character()),
    character = list(big = "char", storage = "character", bytes = 1L, readr = readr::col_character()),
    stop(sprintf("Unsupported dat.type '%s'.", dat.type), call. = FALSE)
  )
}

.bigpca_import_count_lines <- function(path) {
  con <- file(path, open = "r")
  on.exit(close(con), add = TRUE)
  total <- 0L
  repeat {
    lines <- readLines(con, n = 50000)
    if (!length(lines)) {
      break
    }
    total <- total + length(lines)
  }
  total
}

.bigpca_import_is_numeric_vector <- function(x) {
  suppressWarnings(!is.na(as.numeric(x)))
}

.bigpca_import_probe_file <- function(path, delim = "\t") {
  con <- file(path, open = "r")
  on.exit(close(con), add = TRUE)
  first_line <- readLines(con, n = 1)
  if (!length(first_line)) {
    stop(sprintf("File '%s' is empty.", path), call. = FALSE)
  }
  second_line <- readLines(con, n = 1)
  if (!length(second_line)) {
    stop(sprintf("File '%s' has only a single line; unable to infer structure.", path), call. = FALSE)
  }
  first_fields <- strsplit(first_line, delim, fixed = TRUE)[[1]]
  second_fields <- strsplit(second_line, delim, fixed = TRUE)[[1]]
  header <- any(!.bigpca_import_is_numeric_vector(first_fields))
  row_names_in_file <- length(second_fields) > 0L && !.bigpca_import_is_numeric_vector(second_fields[1])
  header_fields <- if (header) {
    if (row_names_in_file && length(first_fields) == length(second_fields)) {
      first_fields[-1]
    } else {
      first_fields
    }
  } else {
    character()
  }
  ncol <- length(second_fields) - if (row_names_in_file) 1L else 0L
  if (ncol <= 0L) {
    stop(sprintf("Unable to infer number of data columns in '%s'.", path), call. = FALSE)
  }
  list(
    header = header,
    row_names_in_file = row_names_in_file,
    header_fields = header_fields,
    ncol = ncol
  )
}

.bigpca_import_default_names <- function(prefix, n) {
  sprintf("%s%d", prefix, seq_len(n))
}

.bigpca_import_compute_chunk_size <- function(ncol, bytes_per_cell, ram_gb) {
  if (!is.numeric(ram_gb) || !length(ram_gb) || is.na(ram_gb)) {
    ram_gb <- 1
  }
  ram_bytes <- ram_gb * (1024^3)
  if (!is.finite(ram_bytes) || ram_bytes <= 0) {
    ram_bytes <- 128 * 1024^2
  }
  denom <- max(bytes_per_cell * max(1L, ncol), 1)
  chunk <- floor(ram_bytes / denom)
  max(1L, chunk)
}

.bigpca_import_read_vector_file <- function(files, base_dir, label) {
  files <- unlist(files)
  if (!length(files)) {
    return(NULL)
  }
  lapply(files, function(f) {
    path <- if (!is.null(base_dir) && nzchar(base_dir)) cat.path(base_dir, f, must.exist = TRUE) else f
    if (!file.exists(path)) {
      stop(sprintf("%s name file '%s' was not found.", tools::toTitleCase(label), f), call. = FALSE)
    }
    readr::read_lines(path, progress = FALSE)
  })
}

.bigpca_import_resolve_name_inputs <- function(explicit, files_spec, dir, label) {
  per_file <- FALSE
  if (!is.null(explicit)) {
    if (is.character(explicit) && length(explicit) == 1L) {
      candidate <- tryCatch(cat.path(dir, explicit), error = function(...) explicit)
      if (file.exists(candidate)) {
        files_spec <- explicit
        explicit <- NULL
      }
    }
  }
  if (!is.null(explicit)) {
    if (is.list(explicit)) {
      per_file <- length(explicit) > 1L
      values <- lapply(explicit, function(x) {
        if (!is.character(x)) {
          stop(sprintf("%s names must be character vectors.", tools::toTitleCase(label)), call. = FALSE)
        }
        x
      })
    } else if (is.character(explicit)) {
      values <- list(explicit)
    } else {
      stop(sprintf("%s names must be a character vector or list of character vectors.", label), call. = FALSE)
    }
    return(list(values = values, per_file = per_file, source = "explicit"))
  }
  if (!is.null(files_spec)) {
    file_list <- as.list(files_spec)
    values <- .bigpca_import_read_vector_file(file_list, dir, label)
    per_file <- length(unlist(file_list)) > 1L
    return(list(values = values, per_file = per_file, source = "file"))
  }
  list(values = vector("list", 0), per_file = FALSE, source = "none")
}

.bigpca_import_expand_list <- function(values, n_files, label) {
  if (!length(values)) {
    stop(sprintf("No %s names supplied for partitioned import.", label), call. = FALSE)
  }
  if (length(values) == 1L && n_files > 1L) {
    return(rep(values, n_files))
  }
  if (length(values) != n_files) {
    stop(sprintf("Number of %s name sets must equal the number of input files or be length one.", label), call. = FALSE)
  }
  values
}

.bigpca_import_build_spec <- function(files, dirs, long, rows.fn, cols.fn, row.names, col.names, ram.gb, cell_bytes, logger) {
  n_files <- length(files)
  row_input <- .bigpca_import_resolve_name_inputs(row.names, rows.fn, dirs$ano, "row")
  col_input <- .bigpca_import_resolve_name_inputs(col.names, cols.fn, dirs$ano, "column")
  if (n_files > 1L) {
    if (row_input$per_file && !col_input$per_file) {
      mode <- "rows"
      row_lists <- .bigpca_import_expand_list(row_input$values, n_files, "row")
      col_lists <- col_input$values
    } else if (col_input$per_file && !row_input$per_file) {
      mode <- "cols"
      col_lists <- .bigpca_import_expand_list(col_input$values, n_files, "column")
      row_lists <- row_input$values
    } else if (row_input$per_file && col_input$per_file) {
      stop("Provide either per-file row names or per-file column names, not both.", call. = FALSE)
    } else {
      stop("Multiple input files require per-file row or column names.", call. = FALSE)
    }
  } else if (isTRUE(long)) {
    mode <- "long"
    row_lists <- row_input$values
    col_lists <- col_input$values
  } else {
    mode <- "single"
    row_lists <- row_input$values
    col_lists <- col_input$values
  }
  spec <- list(mode = mode, files = files, delim = "\t")
  if (identical(mode, "single") || identical(mode, "long")) {
    probe <- .bigpca_import_probe_file(files[1], delim = spec$delim)
    total_lines <- .bigpca_import_count_lines(files[1])
    nrow <- total_lines - as.integer(probe$header)
    if (nrow <= 0L) {
      stop("Input file contains no data rows.", call. = FALSE)
    }
    ncol <- probe$ncol
    if (length(col_lists)) {
      col_names <- col_lists[[1]]
      if (length(col_names) != ncol) {
        stop("Number of column names does not match detected column count.", call. = FALSE)
      }
    } else if (probe$header) {
      col_names <- probe$header_fields
      if (length(col_names) != ncol) {
        col_names <- col_names[seq_len(ncol)]
      }
    } else {
      col_names <- .bigpca_import_default_names("col", ncol)
    }
    capture_row_names <- FALSE
    if (length(row_lists)) {
      row_names <- row_lists[[1]]
      if (length(row_names) != nrow) {
        stop("Number of row names does not match detected row count.", call. = FALSE)
      }
    } else if (probe$row_names_in_file) {
      row_names <- character(nrow)
      capture_row_names <- TRUE
    } else {
      row_names <- .bigpca_import_default_names("row", nrow)
    }
    chunk_size <- min(nrow, .bigpca_import_compute_chunk_size(ncol, cell_bytes, ram.gb))
    spec$nrow <- nrow
    spec$ncol <- ncol
    spec$col_names <- col_names
    spec$row_names <- if (capture_row_names) NULL else row_names
    spec$row_names_buffer <- if (capture_row_names) row_names else NULL
    spec$file_specs <- list(list(
      path = files[1],
      header = probe$header,
      row_names_in_file = probe$row_names_in_file,
      chunk_size = chunk_size
    ))
  } else if (identical(mode, "rows")) {
    col_template <- if (length(col_lists)) col_lists[[1]] else NULL
    if (is.null(col_template) || !length(col_template)) {
      probe <- .bigpca_import_probe_file(files[1], delim = spec$delim)
      if (probe$row_names_in_file) {
        stop("Row-partitioned files must not contain row names; supply them separately.", call. = FALSE)
      }
      col_template <- if (probe$header) probe$header_fields else .bigpca_import_default_names("col", probe$ncol)
    }
    ncol <- length(col_template)
    row_lists <- .bigpca_import_expand_list(row_lists, n_files, "row")
    row_lengths <- vapply(row_lists, length, integer(1))
    nrow <- sum(row_lengths)
    chunk_size <- min(max(row_lengths), .bigpca_import_compute_chunk_size(ncol, cell_bytes, ram.gb))
    offsets <- cumsum(c(0L, row_lengths))
    file_specs <- vector("list", n_files)
    for (i in seq_len(n_files)) {
      probe <- .bigpca_import_probe_file(files[i], delim = spec$delim)
      if (probe$row_names_in_file) {
        stop("Row-partitioned files must not contain row names.", call. = FALSE)
      }
      if (probe$ncol != ncol) {
        stop(sprintf("File '%s' had %d columns but %d were expected.", basename(files[i]), probe$ncol, ncol), call. = FALSE)
      }
      file_specs[[i]] <- list(
        path = files[i],
        header = probe$header,
        row_names_in_file = FALSE,
        row_start = offsets[i] + 1L,
        row_end = offsets[i + 1L],
        chunk_size = min(row_lengths[i], chunk_size)
      )
    }
    spec$nrow <- nrow
    spec$ncol <- ncol
    spec$col_names <- col_template
    spec$row_names <- unlist(row_lists, use.names = FALSE)
    spec$row_names_buffer <- NULL
    spec$file_specs <- file_specs
  } else if (identical(mode, "cols")) {
    col_lists <- .bigpca_import_expand_list(col_lists, n_files, "column")
    col_lengths <- vapply(col_lists, length, integer(1))
    ncol <- sum(col_lengths)
    if (!length(row_lists)) {
      probe <- .bigpca_import_probe_file(files[1], delim = spec$delim)
      if (!probe$row_names_in_file) {
        stop("Column-partitioned imports require row names via 'row.names' or within the files.", call. = FALSE)
      }
      total_lines <- .bigpca_import_count_lines(files[1])
      nrow <- total_lines - as.integer(probe$header)
      row_template <- character(nrow)
      needs_capture <- TRUE
    } else {
      row_template <- row_lists[[1]]
      nrow <- length(row_template)
      needs_capture <- FALSE
    }
    chunk_size <- min(nrow, .bigpca_import_compute_chunk_size(ncol, cell_bytes, ram.gb))
    offsets <- cumsum(c(0L, col_lengths))
    file_specs <- vector("list", n_files)
    for (i in seq_len(n_files)) {
      probe <- .bigpca_import_probe_file(files[i], delim = spec$delim)
      if (probe$ncol != col_lengths[i]) {
        stop(sprintf("File '%s' had %d columns but %d were expected.", basename(files[i]), probe$ncol, col_lengths[i]), call. = FALSE)
      }
      file_specs[[i]] <- list(
        path = files[i],
        header = probe$header,
        row_names_in_file = probe$row_names_in_file,
        col_start = offsets[i] + 1L,
        col_end = offsets[i + 1L],
        chunk_size = chunk_size
      )
    }
    spec$nrow <- nrow
    spec$ncol <- ncol
    spec$col_names <- unlist(col_lists, use.names = FALSE)
    spec$row_names <- if (needs_capture) NULL else row_template
    spec$row_names_buffer <- if (needs_capture) row_template else NULL
    spec$file_specs <- file_specs
  } else if (identical(mode, "long")) {
    if (!length(row_lists) || !length(col_lists)) {
      stop("Long format import requires explicit 'row.names' and 'col.names'.", call. = FALSE)
    }
    row_vec <- row_lists[[1]]
    col_vec <- col_lists[[1]]
    spec$nrow <- length(row_vec)
    spec$ncol <- length(col_vec)
    spec$col_names <- col_vec
    spec$row_names <- row_vec
    spec$row_names_buffer <- NULL
    spec$file_specs <- list(list(path = files[1]))
  }
  spec
}

.bigpca_import_initialise_target <- function(spec, dirs, pref, type_info, delete.existing) {
  if (is.null(pref) || !nzchar(pref)) {
    pref <- tools::file_path_sans_ext(basename(spec$files[1]))
  }
  backingfile <- sprintf("%s.bck", pref)
  descriptorfile <- sprintf("%s.dsc", pref)
  rdatafile <- sprintf("%s.RData", pref)
  if (delete.existing) {
    unlink(file.path(dirs$big, backingfile))
    unlink(file.path(dirs$big, descriptorfile))
    unlink(file.path(dirs$big, rdatafile))
  }
  matrix <- bigmemory::filebacked.big.matrix(
    nrow = spec$nrow,
    ncol = spec$ncol,
    backingfile = backingfile,
    descriptorfile = descriptorfile,
    backingpath = dirs$big,
    type = type_info$big,
    dimnames = list(NULL, spec$col_names)
  )
  list(
    matrix = matrix,
    pref = pref,
    backingfile = backingfile,
    descriptorfile = descriptorfile,
    rdatafile = rdatafile
  )
}

.bigpca_import_coerce_chunk <- function(chunk, storage_mode) {
  if (!nrow(chunk)) {
    return(NULL)
  }
  mat <- as.matrix(chunk)
  storage.mode(mat) <- storage_mode
  mat
}

.bigpca_import_apply_row_names <- function(target, names_vec) {
  options(bigmemory.allow.dimnames = TRUE)
  rownames(target) <- names_vec
}

.bigpca_import_apply_col_names <- function(target, names_vec) {
  options(bigmemory.allow.dimnames = TRUE)
  colnames(target) <- names_vec
}

.bigpca_import_report_progress <- function(completed, total, logger) {
  if (total <= 0) {
    return(invisible())
  }
  percent <- (completed / total) * 100
  logger("INFO", "Imported %.1f%% of rows", percent)
}

.bigpca_import_stream_single <- function(spec, target, type_info, tracker, logger) {
  fs <- spec$file_specs[[1]]
  row_buffer <- spec$row_names_buffer
  row_pointer <- 0L
  callback <- readr::SideEffectChunkCallback$new(function(chunk, pos) {
    if (!nrow(chunk)) {
      return()
    }
    if (fs$row_names_in_file) {
      row_values <- chunk[[1]]
      chunk <- chunk[-1]
      indices <- seq_len(length(row_values)) + row_pointer
      if (is.null(spec$row_names)) {
        row_buffer[indices] <<- row_values
      } else if (!identical(row_values, spec$row_names[indices])) {
        stop("Row names in file do not match provided 'row.names'.", call. = FALSE)
      }
    }
    mat <- .bigpca_import_coerce_chunk(chunk, type_info$storage)
    indices <- seq_len(nrow(mat)) + row_pointer
    target$matrix[indices, ] <- mat
    row_pointer <<- row_pointer + nrow(mat)
    if (tracker) {
      .bigpca_import_report_progress(row_pointer, spec$nrow, logger)
    }
  })
  readr::read_delim_chunked(
    fs$path,
    callback = callback,
    chunk_size = fs$chunk_size,
    delim = spec$delim,
    col_names = fs$header,
    show_col_types = FALSE,
    progress = FALSE,
    skip_empty_rows = TRUE,
    col_types = readr::cols(.default = type_info$readr)
  )
  if (row_pointer != spec$nrow) {
    stop("Number of imported rows did not match expectation.", call. = FALSE)
  }
  if (is.null(spec$row_names)) {
    .bigpca_import_apply_row_names(target$matrix, row_buffer)
  } else {
    .bigpca_import_apply_row_names(target$matrix, spec$row_names)
  }
  invisible(target)
}

.bigpca_import_stream_rows <- function(spec, target, type_info, tracker, logger) {
  for (fs in spec$file_specs) {
    expected_rows <- fs$row_end - fs$row_start + 1L
    row_pointer <- fs$row_start - 1L
    rows_written <- 0L
    callback <- readr::SideEffectChunkCallback$new(function(chunk, pos) {
      if (!nrow(chunk)) {
        return()
      }
      mat <- .bigpca_import_coerce_chunk(chunk, type_info$storage)
      indices <- seq_len(nrow(mat)) + row_pointer
      target$matrix[indices, ] <- mat
      row_pointer <<- row_pointer + nrow(mat)
      rows_written <<- rows_written + nrow(mat)
      if (tracker) {
        .bigpca_import_report_progress(rows_written, expected_rows, logger)
      }
    })
    readr::read_delim_chunked(
      fs$path,
      callback = callback,
      chunk_size = fs$chunk_size,
      delim = spec$delim,
      col_names = fs$header,
      show_col_types = FALSE,
      progress = FALSE,
      skip_empty_rows = TRUE,
      col_types = readr::cols(.default = type_info$readr)
    )
    if (rows_written != expected_rows) {
      stop(sprintf("File '%s' did not supply the expected number of rows.", basename(fs$path)), call. = FALSE)
    }
  }
  .bigpca_import_apply_row_names(target$matrix, spec$row_names)
  invisible(target)
}

.bigpca_import_stream_cols <- function(spec, target, type_info, tracker, logger) {
  row_buffer <- spec$row_names_buffer
  for (fs in spec$file_specs) {
    col_range <- seq(fs$col_start, fs$col_end)
    row_pointer <- 0L
    callback <- readr::SideEffectChunkCallback$new(function(chunk, pos) {
      if (!nrow(chunk)) {
        return()
      }
      if (fs$row_names_in_file) {
        row_values <- chunk[[1]]
        chunk <- chunk[-1]
        indices <- seq_len(length(row_values)) + row_pointer
        if (is.null(row_buffer)) {
          if (!identical(row_values, spec$row_names[indices])) {
            stop("Row names in column partitions do not match provided 'row.names'.", call. = FALSE)
          }
        } else {
          row_buffer[indices] <<- row_values
        }
      }
      mat <- .bigpca_import_coerce_chunk(chunk, type_info$storage)
      indices <- seq_len(nrow(mat)) + row_pointer
      target$matrix[indices, col_range] <- mat
      row_pointer <<- row_pointer + nrow(mat)
      if (tracker) {
        .bigpca_import_report_progress(row_pointer, spec$nrow, logger)
      }
    })
    readr::read_delim_chunked(
      fs$path,
      callback = callback,
      chunk_size = fs$chunk_size,
      delim = spec$delim,
      col_names = fs$header,
      show_col_types = FALSE,
      progress = FALSE,
      skip_empty_rows = TRUE,
      col_types = readr::cols(.default = type_info$readr)
    )
    if (row_pointer != spec$nrow) {
      stop(sprintf("File '%s' did not supply the expected number of rows.", basename(fs$path)), call. = FALSE)
    }
  }
  if (!is.null(row_buffer)) {
    .bigpca_import_apply_row_names(target$matrix, row_buffer)
  } else {
    .bigpca_import_apply_row_names(target$matrix, spec$row_names)
  }
  invisible(target)
}

.bigpca_import_stream_long <- function(spec, target, type_info, tracker, logger) {
  values <- readr::read_lines(spec$files[1], progress = FALSE)
  total_expected <- spec$nrow * spec$ncol
  if (length(values) != total_expected) {
    stop(sprintf("Long format file provided %d values but %d were expected.", length(values), total_expected), call. = FALSE)
  }
  if (type_info$storage %in% c("double", "integer")) {
    values <- as.numeric(values)
    if (anyNA(values)) {
      stop("Failed to parse numeric values from long format input.", call. = FALSE)
    }
    if (identical(type_info$storage, "integer")) {
      values <- as.integer(values)
    }
  } else if (!identical(type_info$storage, "character")) {
    values <- as.numeric(values)
  }
  mat <- matrix(values, nrow = spec$nrow, ncol = spec$ncol, byrow = FALSE)
  storage.mode(mat) <- type_info$storage
  target$matrix[,] <- mat
  .bigpca_import_apply_row_names(target$matrix, spec$row_names)
  invisible(target)
}

#' Load delimited text into a file-backed big.matrix
#'
#' @param input.fn Character vector or list of input file paths. When multiple
#'   files are supplied, they must represent either row-wise or column-wise
#'   partitions of the data and the corresponding row/column names must be
#'   provided via `row.names`/`rows.fn` or `col.names`/`cols.fn`.
#' @param dir Directory (or list of directories) containing the data files and
#'   output artefacts. A named list with entries `big`, `ano`, and `col` is
#'   supported for backward compatibility.
#' @param long Logical flag indicating whether the input is a single long-format
#'   vector of values. When `TRUE`, both `row.names` and `col.names` must be
#'   supplied.
#' @param rows.fn,cols.fn Optional character vectors of filenames containing row
#'   or column names. These are primarily used when reading partitioned inputs.
#' @param pref Prefix for the generated backing and descriptor files.
#' @param delete.existing Logical; overwrite existing outputs sharing the same
#'   prefix.
#' @param ret.obj Logical; return the big.matrix descriptor object instead of
#'   the saved descriptor path.
#' @param verbose Logical; emit progress messages.
#' @param row.names,col.names Optional character vectors or lists providing row
#'   or column names directly.
#' @param dat.type Storage mode requested for the big.matrix (e.g. "double",
#'   "integer").
#' @param ram.gb Approximate gigabytes of RAM that can be used for chunked
#'   reading.
#' @param hd.gb Estimated free disk space (in gigabytes) required to accept the
#'   import; used for guard-rail checks.
#' @param tracker Logical; emit progress updates during streaming.
#'
#' @return When `ret.obj = FALSE` (default) the path to the saved descriptor
#'   (`.RData`) file is returned. When `ret.obj = TRUE` the in-memory descriptor
#'   object is returned.
#' @export
import.big.data <- function(input.fn = NULL, dir = getwd(), long = FALSE, rows.fn = NULL, cols.fn = NULL,
                              pref = "", delete.existing = TRUE, ret.obj = FALSE, verbose = TRUE,
                              row.names = NULL, col.names = NULL, dat.type = "double", ram.gb = 2,
                              hd.gb = 1000, tracker = TRUE) {
  logger <- .bigpca_logger(verbose)
  if (is.null(input.fn)) {
    stop("input.fn must be supplied.", call. = FALSE)
  }
  delete.existing <- .bigpca_validate_logical_scalar(delete.existing, "delete.existing")
  ret.obj <- .bigpca_validate_logical_scalar(ret.obj, "ret.obj")
  long <- .bigpca_validate_logical_scalar(long, "long")
  tracker <- .bigpca_validate_logical_scalar(tracker, "tracker")
  dirs <- .bigpca_normalise_dirs(dir)
  files <- unlist(input.fn)
  if (!length(files)) {
    stop("No input files provided in 'input.fn'.", call. = FALSE)
  }
  files <- vapply(files, function(x) {
    if (file.exists(x)) {
      x
    } else {
      cat.path(dirs$col, x, must.exist = TRUE)
    }
  }, character(1))
  type_info <- .bigpca_import_resolve_data_type(dat.type)
  spec <- .bigpca_import_build_spec(files, dirs, long, rows.fn, cols.fn, row.names, col.names, ram.gb, type_info$bytes, logger)
  estimated_usage <- (spec$nrow * spec$ncol * type_info$bytes) / (1024^3)
  if (estimated_usage > hd.gb) {
    stop(sprintf("Estimated disk usage %.2f GB exceeds hd.gb (%.2f GB).", estimated_usage, hd.gb), call. = FALSE)
  }
  previous_dim_option <- getOption("bigmemory.allow.dimnames")
  on.exit(options(bigmemory.allow.dimnames = previous_dim_option), add = TRUE)
  options(bigmemory.allow.dimnames = TRUE)
  target <- .bigpca_import_initialise_target(spec, dirs, pref, type_info, delete.existing)
  logger("INFO", "Importing %d x %d matrix to backing '%s'.", spec$nrow, spec$ncol, target$backingfile)
  if (identical(spec$mode, "single")) {
    .bigpca_import_stream_single(spec, target, type_info, tracker, logger)
  } else if (identical(spec$mode, "rows")) {
    .bigpca_import_stream_rows(spec, target, type_info, tracker, logger)
  } else if (identical(spec$mode, "cols")) {
    .bigpca_import_stream_cols(spec, target, type_info, tracker, logger)
  } else if (identical(spec$mode, "long")) {
    .bigpca_import_stream_long(spec, target, type_info, tracker, logger)
  } else {
    stop(sprintf("Unsupported import mode '%s'.", spec$mode), call. = FALSE)
  }
  bigmemory::flush(target$matrix)
  descriptor <- bigmemory::describe(target$matrix)
  if (ret.obj) {
    return(descriptor)
  }
  rdata_path <- cat.path(dirs$big, target$rdatafile)
  save(descriptor, file = rdata_path)
  logger("INFO", "Saved big.matrix descriptor to %s", rdata_path)
  rdata_path
}

# mean replacement code internal
row.rep <- function(X) { X[is.na(X)] <- mean(X,na.rm=T); X }


#internal, analog of 'select.samp.snp.custom' from older versions of plumbCNV
select.col.row.custom <- function(bigMat,row,col,verbose=T)
{
  # based on files/vectors of row-ids and column-ids create selection
  # vectors to select only the ids in these lists for a matrix
  if(verbose) { cat(" calculating selections for rows\n") }
  # try to detect whether a vector of IDs, or file names
  row.ref <- rownames(bigMat)  ; col.ref <- colnames(bigMat) 
  row.sel <- col.sel <- NULL
  byname <- T
  if (length(row)==1 & length(col)==1 & is.character(row) & is.character(col))
  {
    if(verbose) { cat(" [assuming 'col' and 'row' are file names containing column and row ids]") }
    if(file.exists(row)) {
      row.sel <- readLines(row)
    } else {
      if(row=="") {
        if(verbose) { cat(c(" row subset file was empty, selecting all\n")) }
        row.sel <- row.ref
      } else {
        stop("Error: argument 'row' should be a vector of rows names length>1 or a filename with a list of rows (no header)")
      }
    }
    if(file.exists(col)) {
      col.sel <- readLines(col)
    } else {
      if(col=="") {
        if(verbose) { cat(c(" column subset file was empty, selecting all\n")) }
        col.sel <- col.ref
      } else {
        stop("Error: argument 'col' should be a vector of column names length>1 or a filename with a list of rows (no header)")
      }
    }
  } else { 
    if(is.character(row) | is.character(col)) {
      #cat("[assuming 'col' and 'row' are vectors of column and row ids]")
      # if blank then assign all ids
      if(all(row=="")) {
        row.sel <- row.ref
      } else {
        row.sel <- row
      }
      if(all(col=="")) {
        col.sel <- col.ref
      } else { 
        col.sel <- col  
      }
    } else {
      byname <- F
      # assume numeric or logical selection
      if(is.logical(row)) { row <- which(row) }
      if(is.logical(col)) { col <- which(col) }
      to.order.r <- row[row<=nrow(bigMat)]
      to.order.c <- col[col<=ncol(bigMat)]
    }
  }
  # use sort/exclusion lists to get reordering vectors
  #row.sel <- row.sel ; col.sel <- col.sel
  
  #print(head(row.sel));print(head(col.sel))
  if(byname) {
    # selection is based on row/col names
    to.order.r <- narm(match(row.sel,rownames(bigMat)))
    to.order.c <- narm(match(col.sel,colnames(bigMat)))
  } else {
    # selection is based on row/col numbers or logical
  }
  if (!(length(to.order.r[!is.na(to.order.r)])>0 & length(to.order.c[!is.na(to.order.c)])>0))
  { warning("selection of rows and/or columns has resulted in an empty dataset",
            "\ncheck rownames, column names and selection lists for errors") }
  
  out.list <- list(to.order.r,to.order.c,row.sel,col.sel)
  names(out.list) <- c("to.order.r","to.order.c","row.list","column.list")
  return(out.list)
}


#' Reduce one dimension of a large matrix in a strategic way
#' 
#' Thin the rows (or columns) of a large matrix or big.matrix in order to reduce the size of the
#' dataset while retaining important information. Percentage of the original size or a new number 
#' of rows/columns is selectable, and then there are four methods to choose the data subset.
#' Simple uniform and random selection can be specified. Other methods look at the correlation
#' structure of a subset of the data to derive non-arbitrary selections, using correlation, PCA,
#' or association with a phenotype or some other categorical variable. Each of the four methods
#' has a separate function in this package, which you can see for more information, this function
#' is merely a wrapper to select one of the four.
#' @param bigMat a big.matrix object, or any argument accepted by get.big.matrix(), which includes
#'  paths to description files or even a standard matrix object.
#' @param keep numeric, by default a proportion (decimal) of the original number of rows/columns to choose
#'  for the subset. Otherwise if an integer>2 then will assume this is the size of the desired subset,
#'  e.g, for a dataset with 10,000 rows where you want a subset size of 1,000 you could set 'keep' as
#'  either 0.1 or 1000.
#' @param how character, only the first two characters are required and they are not case sensitive,
#'  select what method to use to perform subset selection, options are:
#' 'uniform': evenly spaced selection when random=FALSE, or random selection otherwise;
#'  see uniform.select().
#' 'correlation': most correlated subset when hi.cor=TRUE, least correlated otherwise;
#'  see subcor.select().
#' 'pca': most representative variables of the principle components of a subset;
#'  see subpc.select().
#' 'association': most correlated subset with phenotype if least=FALSE, or least correlated otherwise;
#'  see select.least.assoc().
#' @param dir directory containing the filebacked.big.matrix, same as 'dir' for get.big.matrix.
#' @param rows logical, whether to choose a subset of rows (TRUE), or columns (FALSE). rows is always
#'  TRUE when using 'association' methods.
#' @param random logical, whether to use random selections and subsets (TRUE), or whether to use uniform
#'  selections that should give the same result each time for the same dataset (FALSE)
#' @param hi.cor logical, if using 'correlation' methods, then whether to choose the most correlated (TRUE)
#'  or least correlated (FALSE).
#' @param least logical, if using 'association' methods, whether to choose the least associated (TRUE) or 
#'  most associated variables with phenotype
#' @param pref character, a prefix for big.matrix backing files generated by this selection
#' @param verbose logical, whether to display more information about processing
#' @param delete.existing logical, whether to overwrite existing file-backed matrices generated with the same prefix
#' @param ret.obj logical, whether to return the result as a big.matrix object (TRUE), or as a reference
#'  to the binary file containing the big.matrix.descriptor object [either can be read with get.big.matrix() or
#'  prv.big.matrix()]
#' @param ... other arguments to be passed to uniform.select, subpc.select, subcor.select, or select.least.assoc
#' @return A smaller big.matrix with fewer rows and/or columns than the original matrix
#' @export
#' @seealso \code{\link{uniform.select}}, \code{\link{subpc.select}}, \code{\link{subcor.select}}, 
#' \code{\link{select.least.assoc}}, \code{\link{big.select}}, \code{\link{get.big.matrix}}
#' @author Nicholas Cooper 
#' @examples 
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' if(file.exists("thin.bck")) { unlink(c("thin.bck","thin.dsc")) }
#' bmat <- generate.test.matrix(5,big.matrix=TRUE)
#' prv.big.matrix(bmat)
#' # make 5% random selection:
#' lmat <- thin(bmat, pref="th2")
#' prv.big.matrix(lmat)
#' # make 10% most orthogonal selection (lowest correlations):
#' lmat <- thin(bmat,.10,"cor",hi.cor=FALSE, pref="th3")
#' prv.big.matrix(lmat)
#' # make 10% most representative selection:
#' lmat <- thin(bmat,.10,"PCA",ret.obj=FALSE, pref="th4") # return file name instead of object
#' print(lmat)
#' prv.big.matrix(lmat)
#' # make 25% selection most correlated to phenotype
#' # create random phenotype variable
#' pheno <- rep(1,ncol(bmat)); pheno[which(runif(ncol(bmat))<.5)] <- 2
#' lmat <- thin(bmat,.25,"assoc",phenotype=pheno,least=FALSE,verbose=TRUE, pref="th5")
#' prv.big.matrix(lmat)
#' # tidy up temporary files:
#' rm(lmat) 
#' unlink(c("thin.bck","thin.dsc","thin.RData",paste0("th",2:5)))
#' setwd(orig.dir)
thin <- function(
  bigMat,
  keep = 0.05,
  how = c("uniform", "correlation", "pca", "association"),
  dir = "",
  rows = TRUE,
  random = TRUE,
  hi.cor = TRUE,
  least = TRUE,
  pref = "thin",
  verbose = FALSE,
  ret.obj = TRUE,
  delete.existing = TRUE,
  ...
) {
  logger <- .bigpca_logger(verbose)
  dirs <- .bigpca_resolve_dirs(dir, c("big", "pc"))
  rows <- .bigpca_validate_logical_scalar(rows, "rows")
  random <- .bigpca_validate_logical_scalar(random, "random")
  hi.cor <- .bigpca_validate_logical_scalar(hi.cor, "hi.cor")
  least <- .bigpca_validate_logical_scalar(least, "least")
  ret.obj <- .bigpca_validate_logical_scalar(ret.obj, "ret.obj")
  delete.existing <- .bigpca_validate_logical_scalar(delete.existing, "delete.existing")
  how_choice <- match.arg(tolower(how), c("uniform", "correlation", "pca", "association"))
  method_key <- toupper(substr(how_choice, 1, 2))
  input <- .bigpca_resolve_input_matrix(bigMat, dirs, logger)
  if (identical(method_key, "AS") && !rows) {
    logger("WARN", "Association thinning only operates on rows; switching to rows = TRUE.")
    rows <- TRUE
  }
  total_dim <- if (rows) input$nrow else input$ncol
  if (total_dim < 1L) {
    stop("Input matrix must have positive dimensions for thinning.", call. = FALSE)
  }
  keep_value <- .bigpca_validate_numeric_scalar(keep, "keep", lower = 0)
  subset_size <- if (keep_value > 1) min(total_dim, round(keep_value)) else max(1L, round(total_dim * keep_value))
  if (subset_size >= total_dim) {
    logger("INFO", "Requested keep value retains all %s; proceeding without reduction.", if (rows) "rows" else "columns")
  } else {
    logger("INFO", "Reducing %s from %d to %d entries using %s thinning.", if (rows) "rows" else "columns", total_dim, subset_size, method_key)
  }
  work_mat <- if (identical(input$type, "big.matrix")) input$bigmatrix else input$matrix
  selection <- switch(
    method_key,
    UN = {
      rc <- uniform.select(work_mat, keep = keep, dir = dirs$big, rows = rows, random = random, ...)
      if (rows) rc$order.r else rc$order.c
    },
    CO = subcor.select(work_mat, keep = keep, dir = dirs$big, rows = rows, random = random, hi.cor = hi.cor, ...),
    PC = subpc.select(work_mat, keep = keep, dir = dirs$big, rows = rows, random = random, ...),
    AS = {
      dots <- list(...)
      if (!"phenotype" %in% names(dots)) {
        stop("must include argument 'phenotype' when how = 'association'", call. = FALSE)
      }
      select.least.assoc(work_mat, keep = keep, dir = dirs$big, verbose = verbose, least = least, ...)
    },
    stop(sprintf("invalid subsetting method (%s) specified", how_choice), call. = FALSE)
  )
  if (is.null(selection) || length(selection) == 0L) {
    stop("Thinning selection produced no indices; check input parameters.", call. = FALSE)
  }
  if (rows) {
    selected_rows <- selection
    selected_cols <- seq_len(input$ncol)
  } else {
    selected_rows <- seq_len(input$nrow)
    selected_cols <- selection
  }
  big_result <- big.select(
    bigMat,
    select.rows = selected_rows,
    select.cols = selected_cols,
    dir = dirs$big,
    deepC = TRUE,
    pref = pref,
    verbose = verbose,
    delete.existing = delete.existing
  )
  if (ret.obj) {
    return(get.big.matrix(big_result, dir = dirs))
  }
  big_result
}




#' Select a subset of a big.matrix
#' 
#' Select a subset of big.matrix using indexes for a subset of rows and columns.
#' Essentially a wrapper for bigmemory::deepcopy, but with slightly more flexible
#' parameters. bigMat can be entered in any form accepted by get.big.matrix(), row and
#' column selections can be vectors of indexes, names or file.names containing indexes.
#' Default is to process using deepcopy, but processing without using bigmemory native
#' methods is a faster option when matrices are small versus available RAM. File names
#' for backing files are managed only requiring you to enter a prefix, or optionally
#' use the default and gain filebacked functionality without having to bother choosing
#' filename parameters.
#' @param bigMat a big.matrix, matrix or any object accepted by get.big.matrix()
#' @param select.rows selection of rows of bigMat, can be numbers, logical, rownames, or a file with names. 
#'  If using a filename argument, must also use a filename argument for select.cols (cannot mix)
#' @param select.cols selection of columns of bigMat, can be numbers, logical, colnames, or a file with names
#' @param dir the directory containing the bigMat backing file (e.g, parameter for get.big.matrix()).
#' @param deepC logical, whether to use bigmemory::deepcopy, which is slowish, but scalable, or 
#'  alternatively to use standard indexing which converts the result to a regular matrix object,
#'  and is fast, but only feasible for matrices small enough to fit in memory.
#' @param pref character, prefix for the big.matrix backingfile and descriptorfile, and optionally
#'  an R binary file containing a big.matrix.descriptor object pointing to the big.matrix result.
#' @param verbose whether to display extra information about processing and progress
#' @param delete.existing logical, if a big.matrix already exists with the same name as implied
#'  by the current 'pref' and 'dir' arguments, then default behaviour (FALSE) is to return an error.
#'  to overwrite any existing big.matrix file(s) of the same name(s), set this parameter to TRUE.
#' @return A big.matrix with the selected (in order) rows and columns specified
#' @export
#' @author Nicholas Cooper 
#' @examples 
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' if(file.exists("sel.bck")) { unlink(c("sel.bck","sel.dsc")) }
#' bmat <- generate.test.matrix(5,big.matrix=TRUE)
#' # take a subset of the big.matrix without using deepcopy
#' sel <- big.select(bmat,c(1,2,8),c(2:10),
#'  deepC=FALSE,verbose=TRUE, delete.existing=TRUE)
#' prv.big.matrix(sel)
#' # now select the same subset using row/column names from text files
#' writeLines(rownames(bmat)[c(1,2,8)],con="bigrowstemp.txt")
#' writeLines(colnames(bmat)[c(2:10)],con="bigcolstemp.txt")
#' sel <- big.select(bmat, "bigrowstemp.txt","bigcolstemp.txt", delete.existing=TRUE, pref="sel2")
#' prv.big.matrix(sel)
#' rm(bmat)
#' rm(sel)  
#' unlink(c("bigcolstemp.txt","bigrowstemp.txt","sel.RData","sel2.bck","sel2.dsc"))
#' setwd(orig.dir) # reset working dir to original
big.select <- function(bigMat, select.rows=NULL, select.cols=NULL, dir=getwd(), 
                       deepC=TRUE, pref="sel", delete.existing=FALSE, verbose=FALSE)
{
  # sort and exclude snps/samples from a big.matrix
  if(exists("validate.dir.for",mode="function")) {
    ## plumbCNV specific code ##
    dir <- do.call("validate.dir.for",list(dir=dir,elements=c("big","pc"),warn=F))  
  } else {
    # otherwise
    dir <- list(big=dir,pc=dir)
  }
  # bigmatrix file names for re-ordered filtered matrix (which is the final output of this script)
  if(is.character(bigMat) & length(bigMat)==1) { Fn <- gsub("descrFile","",bigMat) } else { Fn <- "" }
  bck.fn.o <- cat.path(fn=Fn,pref=pref,ext="bck")
  des.fn.o <- cat.path(fn=Fn,pref=pref,ext="dsc")
  R.descr <- cat.path(dir$big,rmv.ext(des.fn.o),ext=".RData"); print(rmv.ext(des.fn.o))
  
  bigMat <- get.big.matrix(bigMat,dir)
  if(verbose) { cat(paste(" attached matrix with dims:",paste(dim(bigMat),collapse=","),"\n")) }
  # get list of deleting/reordering vectors using annotation files
  trans.list <- select.col.row.custom(bigMat,row=select.rows,col=select.cols,verbose=verbose)
  if(verbose) { cat(paste(" selected",length(trans.list[[2]]),"listed samples and",length(trans.list[[1]]),"variables\n")) }
  wrn <- "trans.list was already attached, detaching now..\n"
  #while("trans.list" %in% search()) { detach(trans.list); warning(wrn) }
  #attach(trans.list)
  to.order.r <- trans.list$to.order.r; to.order.c <- trans.list$to.order.c
  if(verbose) {
    cat("\nReordering Variables and Samples...\n")
    cat("\nINDEXES SUMMARY\n")
    cat(paste(length(to.order.r),"row indexes range is from",min(to.order.r),"to",max(to.order.r),"\n"))
    cat("-->",head(to.order.r),sep=", "); cat ("\n")
    cat(paste(length(to.order.c),"col indexes range is from",min(to.order.c),"to",max(to.order.c),"\n"))
    cat("-->",head(to.order.c),sep=", ")
    cat("\n\n raw big.matrix summary before selection/ordering:\n\n")
    prv.big.matrix(bigMat,"bigMat")
  }
  if(!deepC)
  {
    # this is fast with available RAM (like 20 secs)
    if(verbose) { cat(" running reorder in system memory\n") }
    bigMat1 <- bigMat[to.order.r,to.order.c]
    if(verbose) {
      cat(" adding colnames\n") ; colnames(bigMat1) <- colnames(bigMat)[to.order.c] 
      cat(" adding rownames\n") ; rownames(bigMat1) <- rownames(bigMat)[to.order.r] 
      cat(" converting matrix to big.matrix\n") 
    }
    if(delete.existing & file.exists(cat.path(dir$big,basename(bck.fn.o)))) { unlink(cat.path(dir$big,basename(bck.fn.o))) }
    bigMat2 <- as.big.matrix(bigMat1, backingfile=basename(bck.fn.o),
                             descriptorfile=basename(des.fn.o),backingpath=dir$big)
    if(verbose) { cat(paste(" matrix descr saved as standard description file:",des.fn.o,"\n")) }
    descr <- bigmemory::describe(bigMat2)
  } else {
    #this is slow but creates backing file and will speed up ops later
      ### DELETE EXISTING FILE IF HAS SAME NAME ###
      if ((!basename(des.fn.o) %in% list.files(dir$big)) | delete.existing )
      {
        if(delete.existing & (basename(des.fn.o) %in% list.files(dir$big)))
        {
          dfn <- cat.path(dir$big,basename(des.fn.o))
          cat("\n deleting",dfn,"\n")
          unlink(dfn)
        } else {
          #all clear, no files already exist with same name
        }
      } else {
        cat(paste("\nWarning: Big matrix description file",basename(des.fn.o),"already exists in",dir$big,"\n"))
        cat("You may wish to delete, rename or move this file, or use option 'delete.existing'=T, before re-running this script\n")
        #stop()
      }
    if(verbose) { cat(" starting deep copy...") }
    if(delete.existing & file.exists(cat.path(dir$big,basename(bck.fn.o)))) { unlink(cat.path(dir$big,basename(bck.fn.o))) }
    bigMat2 <- deepcopy(bigMat, cols = to.order.c, rows = to.order.r,
                        backingfile=basename(bck.fn.o),backingpath=dir$big,
                        descriptorfile=basename(des.fn.o) )
    if(verbose) { cat("done\n") }
    if(verbose) { cat("\nAdding names\n") }
    options(bigmemory.allow.dimnames=TRUE)
    colnames(bigMat2) <- colnames(bigMat)[to.order.c]
    if(verbose) { cat(" added colnames\n") }
    rownames(bigMat2) <- rownames(bigMat)[to.order.r]  
    if(verbose) { cat(" added rownames\n") }
    descr <- bigmemory::describe(bigMat2)
    bigmemory::flush(bigMat2) # hopefully this will ensure the row/colnames are added to the file backing
    if(verbose) { cat(paste(" due to use of deep copy option, recommend only to use descr saved as rbinary description file\n")) }
  }
  save(descr,file=R.descr)
  if(verbose) {
    cat(paste(" created big.matrix description file:",des.fn.o,"\n"))
    cat(paste(" created big.matrix backing file:",bck.fn.o,"\n"))
    cat(paste(" created big.matrix binary description file:",basename(R.descr),"\n"))
  } 
  #while("trans.list" %in% search()) { detach(trans.list) }
  #return(descr) 
  return(R.descr)
}



#' Selection of a representative variable subset
#' 
#' Returns a subset (size='keep') of row or column numbers that are most representative of a dataset.
#' This function performs PCA on a small subset of columns and all rows (when rows=TRUE, or vice
#'  -versa when rows=FALSE), and selects rows (rows=TRUE) most correlated to the first 'n' principle
#' components, where 'n' is chosen by the function quick.elbow(). The number of variables selected
#' corresponding to each component is weighted according to how much of the variance is explained
#' by each component.
#' @param bigMat a big.matrix, matrix or any object accepted by get.big.matrix()
#' @param keep numeric, by default a proportion (decimal) of the original number of rows/columns to choose
#'  for the subset. Otherwise if an integer>2 then will assume this is the size of the desired subset,
#'  e.g, for a dataset with 10,000 rows where you want a subset size of 1,000 you could set 'keep' as
#'  either 0.1 or 1000.
#' @param rows logical, whether the subset should be of the rows of bigMat. If rows=FALSE, then 
#'  the subset is chosen from columns, would be equivalent to calling subpc.select(t(bigMat)),
#'  but avoids actually performing the transpose which can save time for large matrices.
#' @param dir the directory containing the bigMat backing file (e.g, parameter for get.big.matrix()).
#' @param random logical, passed to uniform.select(), whether to take a random or uniform selection
#'  of columns (or rows if rows=F) to run the subset PCA.
#' @param ram.gb maximum size of the matrix in gigabytes for the subset PCA, 0.1GB is the default 
#'  which should result in minimal processing time on a typical system. Increasing this
#'  increases the processing time, but also the representativeness of the subset chosen. Note
#'  that some very large matrices will not be able to be processed by this function unless 
#'  this parameter is increased; basically if the dimension being thinned is more than 5% of
#'  this memory limit (see estimate.memory() from NCmisc).
#' @param ... further parameters to pass to big.PCA() which performs the subset PCA used to 
#'  determine the most representative rows (or columns).
#' @return A set of row or column indexes (depents on 'rows' parameter) of the most representative
#'  variables in the matrix, as defined by most correlated to principle components
#' @export
#' @seealso \code{\link{thin}}, \code{\link{uniform.select}}, \code{\link{big.PCA}}, \code{\link{get.big.matrix}}
#' @author Nicholas Cooper 
#' @examples 
#' mat <- matrix(rnorm(200*2000),ncol=200) # normal matrix
#' bmat <- as.big.matrix(mat)              # big matrix
#' ii <- subpc.select(bmat,.05,rows=TRUE) # thin down to 5% of the rows
#' ii <- subpc.select(bmat,45,rows=FALSE) # thin down to 45 columns
#' # show that rows=T is equivalent to rows=F of the transpose (random must be FALSE)
#' ii1 <- subpc.select(mat,.4,rows=TRUE,random=FALSE)
#' ii2 <- subpc.select(t(mat),.4,rows=FALSE,random=FALSE)
#' print(all.equal(ii1,ii2))
subpc.select <- function(bigMat,keep=.05,rows=TRUE,dir=getwd(),random=TRUE,ram.gb=0.1,...) {  
  # select a subset of variables based on the most representative variables in the PCs of a subset
  bigMat <- get.big.matrix(bigMat,dir=dir)
  if(rows) { N <- nrow(bigMat) } else { N <- ncol(bigMat) }
  if(keep>2) {
    new.n <- round(keep)
  } else {
    new.n <- round(max(0,min(1,keep))*N)
  }
  min.other <- min(c(dim(bigMat)/2,20)) # e.g, minimum of 20 sample subset to test with very large variable set
  max.other <- min(c(dim(bigMat)/2,200)) # a good amount to test on
  if(estimate.memory(c(N,min.other))>ram.gb) {
    ## too big to do this
    warning("The matrix selected has too many variables to do a mini-PCA on a meaningful subset. Suggest choosing an alternative subset method")
    return(NULL)
  } 
  if(estimate.memory(c(N,max.other))<ram.gb) {
    # go with max
    keeper <- max.other
  } else {
    # go with best we can up to that memory point
    mult <- estimate.memory(c(N,max.other))/ram.gb
    keeper <- (max.other/mult)
  }
  #print(keeper)
  rc <- uniform.select(bigMat,keep=keeper,rows=!rows,dir=dir,random=random)
  ## do it here
  sub.mat <- bigMat[rc[[1]],rc[[2]]]
  #prv.large(sub.mat)
  irlba_available <- requireNamespace("irlba", quietly = TRUE)
  uba <- irlba_available
  if(rows) {
    quick.pc <- big.PCA(sub.mat,pcs.to.keep=NA,use.bigalgebra=uba,...)
  } else {
    quick.pc <- big.PCA(t(sub.mat),pcs.to.keep=NA,use.bigalgebra=uba,...)
  }
  el <- quick.elbow(quick.pc$Evalues)
  varpcs <- estimate.eig.vpcs(eigenv=quick.pc$Evalues,
        M=sub.mat,elbow=el,print.est=F,estimated=T,ignore.warn=T)$variance.pcs
  el <- quick.elbow(varpcs)
  pc <- quick.pc$PCs[,1:el]; 
  #print(dim(pc))
  if(ncol(sub.mat)==nrow(pc)) {
    cormat <- abs(cor(pc,t(sub.mat)))
  } else {
    cormat <- abs(cor(pc,sub.mat))
  }
  #return(cormat)
  #print(dim(cormat))
  selected <- logical(ncol(cormat)) # number of variables long logical
  new.set <- NULL
  vpc <- varpcs[1:el]/sum(varpcs[1:el]) # to prevent vagaries of roundoff error
  # make sure total number to choose will be exactly right
  if(random) {
    per.pc <- round(new.n*vpc)
    while(sum(per.pc)>new.n) { rr <- sample(1:length(per.pc),1); per.pc[rr] <- max(0,per.pc[rr]-1) }
    while(sum(per.pc)<new.n) { rr <- sample(1:length(per.pc),1); per.pc[rr] <- max(0,per.pc[rr]+1) }
  } else {
    brks <- quantile(1:new.n,cumsum(rev(sort(vpc))))
    per.pc <- table(findInterval(1:new.n,brks,rightmost.closed=T)+1)
  }
  for (cc in 1:nrow(cormat)) {
    nxt.row <- cormat[cc,]; indc <- 1:ncol(cormat)
    nxt.row <- nxt.row[!selected]; indc <- indc[!selected]
    if(per.pc[cc]>0) {
      chc <- (indc[rev(order(nxt.row))])[1:per.pc[cc]]
      #print((nxt.row[rev(order(nxt.row))])[1:per.pc[cc]])
      new.set <- c(new.set,chc)
      selected[new.set] <- T
    }
  }  
  return(sort(new.set))
}  


#' Selection of the most correlated variable subset
#' 
#' Returns a subset (size='keep') of row or column numbers that are most correlated to other
#' variables in the dataset (or if hi.cor=F), then those that are least correlated.
#' This function performs cor() on a small subset of columns and all rows (when rows=TRUE, or vice
#'  -versa when rows=FALSE), and selects rows (rows=TRUE) with greatest/least absolute sum of correlations.
#' @param bigMat a big.matrix, matrix or any object accepted by get.big.matrix()
#' @param keep numeric, by default a proportion (decimal) of the original number of rows/columns to choose
#'  for the subset. Otherwise if an integer>2 then will assume this is the size of the desired subset,
#'  e.g, for a dataset with 10,000 rows where you want a subset size of 1,000 you could set 'keep' as
#'  either 0.1 or 1000.
#' @param rows logical, whether the subset should be of the rows of bigMat. If rows=FALSE, then 
#'  the subset is chosen from columns, would be equivalent to calling subpc.select(t(bigMat)),
#'  but avoids actually performing the transpose which can save time for large matrices.
#' @param hi.cor logical, whether to choose the most correlated (TRUE) or least correlated subset (FALSE).
#' @param dir the directory containing the bigMat backing file (e.g, parameter for get.big.matrix()).
#' @param random logical, passed to uniform.select(), whether to take a random or uniform selection
#'  of columns (or rows if rows=FALSE) to run the subset PCA.
#' @param ram.gb maximum size of the matrix in gigabytes for the subset PCA, 0.1GB is the default 
#'  which should result in minimal processing time on a typical system. Increasing this
#'  increases the processing time, but also the representativeness of the subset chosen. Note
#'  that some very large matrices will not be able to be processed by this function unless 
#'  this parameter is increased; basically if the dimension being thinned is more than 5% of
#'  this memory limit (see estimate.memory() from NCmisc).
#' @return A set of row or column indexes (depents on 'rows' parameter) of the most inter-correlated
#'  (or least) variables in the matrix.
#' @export
#' @seealso \code{\link{thin}}, \code{\link{uniform.select}}, \code{\link{get.big.matrix}}
#' @author Nicholas Cooper 
#' @examples 
#' mat <- matrix(rnorm(200*2000),ncol=200)
#' bmat <- as.big.matrix(mat)
#' ii1 <- subcor.select(bmat,.05,rows=TRUE) # thin down to 5% of the rows
#' ii2 <- subcor.select(bmat,45,rows=FALSE) # thin down to 45 columns
#' prv(ii1,ii2)
#' # show that rows=T is equivalent to rows=F of the transpose (random must be FALSE)
#' ii1 <- subcor.select(mat,.4,rows=TRUE,random=FALSE)
#' ii2 <- subcor.select(t(mat),.4,rows=FALSE,random=FALSE)
#' print(all.equal(ii1,ii2))
subcor.select <- function(bigMat,keep=.05,rows=TRUE,hi.cor=TRUE,dir=getwd(),random=TRUE,ram.gb=0.1) {  
  # select a subset of variables based on the most representative variables in the PCs of a subset
  if(rows) { N <- nrow(bigMat) } else { N <- ncol(bigMat) }
  if(keep>2) {
    new.n <- round(keep)
  } else {
    new.n <- round(max(0,min(1,keep))*N)
  }
  min.other <- min(c(dim(bigMat)/2,20)) # e.g, minimum of 20 sample subset to test with very large variable set
  max.other <- min(c(dim(bigMat)/2,200)) # a good amount to test on
  if(estimate.memory(c(N,min.other))>ram.gb) {
    ## too big to do this
    warning("The matrix selected has too many variables to do a mini-PCA on a meaningful subset. Suggest choosing an alternative subset method")
    return(NULL)
  } 
  if(estimate.memory(c(N,max.other))<ram.gb) {
    # go with max
    keeper <- max.other
  } else {
    # go with best we can up to that memory point
    mult <- estimate.memory(c(N,max.other))/ram.gb
    keeper <- (max.other/mult)
  }
  #print(keeper)
  rc <- uniform.select(bigMat,keep=keeper,rows=!rows,dir=dir,random=random)
  ## do it here
  sub.mat <- bigMat[rc[[1]],rc[[2]]]
  #prv.large(sub.mat)
  #print(dim(pc))
  if(!rows) {
    cormat <- abs(cor(sub.mat))
  } else {
    cormat <- abs(cor(t(sub.mat)))
  }
  sorter <- order(rowSums(cormat))
  if(hi.cor) {
    rank <- rev(sorter)[1:new.n] # select top new.n correlations
  } else {
    rank <- sorter[1:new.n] # select new.n least correlated
  }
  return(sort(rank))
}


#' Derive a subset of a large dataset
#' 
#' Either randomly or uniformly select rows or columns from a large dataset
#' to form a new smaller dataset.
#' @param bigMat a big.matrix object, or any argument accepted by get.big.matrix(), which includes
#'  paths to description files or even a standard matrix object.
#' @param keep numeric, by default a proportion (decimal) of the original number of rows/columns to choose
#'  for the subset. Otherwise if an integer>2 then will assume this is the size of the desired subset,
#'  e.g, for a dataset with 10,000 rows where you want a subset size of 1,000 you could set 'keep' as
#'  either 0.1 or 1000.
#' @param dir directory containing the filebacked.big.matrix, same as dir for get.big.matrix.
#' @param rows logical, whether the subset should be of the rows of bigMat. If rows=FALSE, then 
#'  the subset is chosen from columns, would be equivalent to calling subpc.select(t(bigMat)),
#'  but avoids actually performing the transpose which can save time for large matrices.
#' @param random logical, passed to uniform.select(), whether to take a random or uniform selection
#'  of columns (or rows if rows=FALSE) to run the subset PCA.
#' @param ram.gb maximum size of the matrix in gigabytes for the subset PCA, 0.1GB is the default 
#'  which should result in minimal processing time on a typical system. Increasing this
#'  increases the processing time, but also the representativeness of the subset chosen. Note
#'  that some very large matrices will not be able to be processed by this function unless 
#'  this parameter is increased; basically if the dimension being thinned is more than 5% of
#'  this memory limit (see estimate.memory() from NCmisc).
#' @return A set of row or column indexes (depents on 'rows' parameter) of uniformly distributed
#'  (optionally reproduceable) or randomly selected variables in the matrix.
#' @export
#' @seealso \code{\link{subpc.select}}
#' @author Nicholas Cooper 
#' @examples 
#' mat <- matrix(rnorm(200*100),ncol=200)  # standard matrix
#' bmat <- as.big.matrix(mat)              # big.matrix
#' ii1 <- uniform.select(bmat,.05,rows=TRUE) # thin down to 5% of the rows
#' ii2 <- uniform.select(bmat,45,rows=FALSE,random=TRUE) # thin down to 45 columns
#' prv(ii1,ii2)
## tailor this to make a random uniform selection
uniform.select <- function(bigMat,keep=.05,rows=TRUE,dir="",random=TRUE,ram.gb=0.1) {  
  # select an exactly evenly spaced subset (reproduceable)
  if(rows) { N <- nrow(bigMat) } else { N <- ncol(bigMat) }
  if(keep>2) {
    new.n <- round(keep)
  } else {
    new.n <- round(max(0,min(1,keep))*N)
  }
  if(!random) {
    X <- cut_fac(N,new.n); 
    indx <- tapply(1:N,X,function(x) { round(median(x)) })
    indx <- as.integer(indx)
    # select a randomly uniform subset
  } else {
    indx <- sort(sample(N,new.n))
  }
  if(rows) { 
    outlist <- list(order.r=indx, order.c=1:ncol(bigMat)) 
  } else {
    outlist <- list(order.r=1:nrow(bigMat), order.c=indx)
  }
  return(outlist)
}


#' Quick association tests for phenotype
#' 
#' Simplistic association tests, only meant for purposes of preliminary variable
#' selection or creation of priors, etc. Quickly obtain association p-values for
#' a big.matrix against a list of phenotypes for each row, where columns are 
#' samples and column labels correspond to the rownames of the sample.info dataframe
#' which contains the phenotype information, in a column labelled 'use.col'.
#' @param bigMat a big.matrix object, or any argument accepted by get.big.matrix(), which includes
#'  paths to description files or even a standard matrix object.
#' @param dir directory containing the filebacked.big.matrix, same as dir for get.big.matrix.
#' @param sample.info a data.frame with rownames corresponding to colnames of the bigMat. Must
#'  also contain a column named 'use.col' (default 'phenotype') which contains the categorical
#'  variable to perform the association test for phenotype, etc. This file may contain extra
#'  ids not in colnames(bigMat), although if any column names of bigMat are missing from
#'  sample.info a warning will be given, and the call is likely to give incorrect results.
#' @param use.col the name of the phenotype column in the data.frame 'sample.info'
#' @param p.values logical, whether to return p.values from the associations 
#' @param F.values logical, whether to return F.values from the associations
#' @param n.cores integer, if wanting to process the analysis using multiple cores, specify the number
#' @return Depending on options selected returns either a list of F values and p values, or just F, or just p-values
#'  for association with each variable in the big.matrix.
#' @param verbose logical, whether to display additional output on progress
#' @return If both F.values and p.values are TRUE, returns dataframe of both statistics for each variable, else a vector.
#' If the phenotype has 20 more or more unique categories, it will be assumed to be continuous and the association
#' test applied will be correlation. If there are two categories a t-test will be used, and 3 to 19 categories, an ANOVA#
#' will be used. Regardless of the analysis function, output will be converted to an F statistic and/or associated p-values.
#' Except if p.values and F.values are both set to FALSE and the phenotype is continuous, then pearsons correlation values
#' will be returned
#' @export
#' @seealso \code{\link{get.big.matrix}}
#' @author Nicholas Cooper 
#' @examples 
#' bmat <- generate.test.matrix(5,big.matrix=TRUE)
#' pheno <- rep(1,ncol(bmat)); pheno[which(runif(ncol(bmat))<.5)] <- 2
#' ids <- colnames(bmat); samp.inf <- data.frame(phenotype=pheno); rownames(samp.inf) <- ids
#' both <- quick.pheno.assocs(bmat,samp.inf); prv(both)
#' Fs <- quick.pheno.assocs(bmat,samp.inf,verbose=TRUE,p.values=FALSE); prv(Fs)
#' Ps <- quick.pheno.assocs(bmat,samp.inf,F.values=FALSE); prv(Ps)
quick.pheno.assocs <- function(bigMat,sample.info=NULL,use.col="phenotype",dir="",
                               p.values=TRUE,F.values=TRUE,n.cores=1,verbose=FALSE)
{
  ## use sample.info
  # create list of bigMatrix locations
  # go 'N' snps at time, concatenate into 1 file, run regression assocs
  # for 2 phenotypes/grps gives t values - ordinally equivalent to logistic regression with 2 groups
  if(!all(c(use.col) %in% colnames(sample.info))) { stop(paste("sample.info was invalid for association tests, need ",use.col," column")) }
  if(verbose) {
    cat(" running row-wise tests against",use.col,"\n")
  }
  bigMat <- get.big.matrix(bigMat,dir)
  samp.list <- colnames(bigMat); 
  tot.samps <- length(samp.list)
  # check that sample.info is valid, if not attempt to fix it
  if(!is.data.frame(sample.info)) { stop("sample.info must be a data.frame (containing phenotype information for each row id)") }
  ## ENSURE ONLY USING SAMPLES IN THE BIGMATRIX ##
  cutt <- which(!rownames(sample.info) %in% samp.list)
  if(length(cutt)>0) {  sample.info <- sample.info[-cutt,,drop=F]  }
  if(ncol(bigMat)!=nrow(sample.info)) { 
    n.mis <- length(which(colnames(bigMat) %in% rownames(sample.info)))
    warning(n.mis," samples in BigMat were found in sample.info [failure likely]")
  }
  ## determine test to use based on number of phenotypes ##
  #print(head(sample.info)); iii34 <- sample.info[[paste(use.col)]]; prv(iii34)
  #print(table(sample.info[[paste(use.col)]],useNA=NULL))
  n.phenos <- length(table(sample.info[[paste(use.col)]],useNA=NULL))
  #si <- table(sample.info[[paste(use.col)]],useNA=NULL)
  #prv(si,use.col,sample.info)
  t.type <- "single"
  if(n.phenos==2) { t.type <- "t.test"}
  if(n.phenos>2) { t.type <- "anova"}
  if(n.phenos>19) { t.type <- "cor"; if(!p.values & !F.values) { t.type <- "cor2"; F.values <- T } }
  if(verbose) {
    cat(" found ",n.phenos," ",use.col,"s, ",t.type," will be used to summarise rows most associated with ",use.col,"\n\n",sep="")
  }
  three.test <- function(col,pheno) { return(summary(aov(col~pheno))[[1]][["F value"]][1]) }
  two.test <- function(col,pheno) { return((cor.test(col,pheno)$statistic)^2)  }
  cnt.test <- function(col,pheno) { return(cor(col,pheno,use="pairwise.complete"))  }
  ph.test <- switch(t.type,anova=three.test,t.test=two.test,cor=two.test,cor2=cnt.test,single=NULL)
  deg.fr <- switch(t.type,anova=n.phenos-1,t.test=1,cor=1,single=NULL)
  if(is.null(ph.test)) { stop("Error: used option for association test by ",use.col," but there is only 1 phenotype in file")}
  row.labels <- rownames(bigMat)
  full.size <- length(row.labels)
  good.mem.lim <- 10^7 # allows fast processing at this limit
  if(ncol(bigMat)>10^5) { good.mem.lim <- 10*(ncol(bigMat)) }
  #print(estimate.memory(bigMat))
  if(estimate.memory(bigMat) < .3) { tracker <- F } else { tracker <- T }
  opt.size <- round(good.mem.lim/tot.samps)
  n.segs <- ceiling(full.size/opt.size)
  #prv(n.segs,bigMat)
  seg.starts <- 1+c(0:(n.segs+2))*opt.size
  seg.starts[seg.starts>full.size] <- full.size+1 # +1 ensures final snp included
  seg.starts <- seg.starts[!duplicated(seg.starts)]
  results <- vector("list", n.segs)
  pheno <- sample.info[[use.col]]
  if(!is.numeric(pheno)) { 
    pp <- pheno; pheno <- as.numeric(factor(paste(pp)))
    warning("pheno was not numeric, so recoded:\n")
    ii <- table(pp,pheno); cat(paste(rownames(ii),colnames(ii),sep=" ==> "),"\n")  
  }
  test.seg <- matrix(numeric(),nrow=opt.size,ncol=tot.samps)
  # break analysis into chunks that fit in memory
  # NB: avoided parallel computing - it does not seem to add any speed here?
  kk <- proc.time()[3]
  if(n.cores>1) {
    Fvalues <- bmcapply(bigMat,1,FUN=ph.test,dir=dir,by=200,n.cores=n.cores,pheno=pheno)
    #prv(Fvalues)
  } else {  
    for (dd in 1:n.segs)
    {
      row.subset <- (seg.starts[dd]):(seg.starts[dd+1]-1)
      nr <- length(row.subset)
      # test subset of snps for each segment in turn
      test.seg[1:nr,] <- bigMat[row.subset,] 
      results[[dd]] <- apply(test.seg[1:nr,],1,ph.test,pheno=pheno)
      if(tracker) { loop.tracker(dd,n.segs) }
    }
    Fvalues <- (do.call(c,results))
  }
  #Fvalues <- round(Fvalues,4) # remember t^2 = F
  if(verbose) {
    cat(" took",round((proc.time()[3]-kk)/60,1),"minutes\n")
  }
  ### option to return p values? could also have option for CHI squared.
  ## also should be able to the bigmatrix apply for a speedup
  # p.from.t <- function(t,df) { pt(t, df, lower=FALSE) }
  p.from.f <- function(FF,k,n) {  pf(FF, k, n,lower.tail = FALSE) }
  # pvalues <- sapply(Fvalues^.5,p.from.t,df=tot.samps-1)
  if(is.numeric(Fvalues)) {
    if(p.values) { 
      pvalues <- sapply(Fvalues,p.from.f,k=deg.fr,n=(tot.samps-1))
      out <- pvalues 
    } else { out <- Fvalues }
    if(verbose) {
      cat("\nSummary of",if(p.values & !F.values) { "p-value" } else { "F" },"statistics returned:\n"); print(summary(out)); cat("\n")
    }
    if(length(row.labels)==length(out))
    { names(out) <- row.labels } else {
      stop("Error: analysis failure as list of row labels did not match length of test statistics")
    }
  } else { out <- rep(1,length(Fvalues)) }
  if(!(p.values & F.values)) {
    return(out)
  } else {
    out <- data.frame(F=Fvalues,p=pvalues)
    rownames(out) <- row.labels
    return(out)
  }
}


#' Select subset of rows least associated with a categorical variable
#' 
#' Runs a quick association analysis on the dataset against a phenotype/categorical
#' variable stored in a dataframe, and uses the results as a way to select
#' a subset of the original matrix, so you may wish to select the 'N' least associated
#' variables, or the 'N' most associated.
#' @param bigMat a big.matrix object, or any argument accepted by get.big.matrix(), which includes
#'  paths to description files or even a standard matrix object.
#' @param keep numeric, by default a proportion (decimal) of the original number of rows/columns to choose
#'  for the subset. Otherwise if an integer>2 then will assume this is the size of the desired subset,
#'  e.g, for a dataset with 10,000 rows where you want a subset size of 1,000 you could set 'keep' as
#'  either 0.1 or 1000.
#' @param dir directory containing the filebacked.big.matrix, same as dir for get.big.matrix.
#' @param phenotype a vector which contains the categorical variable to perform an association 
#'  test for phenotype, etc. This should be the same length as the number of columns (e.g, samples)
#'  in bigMat.
#' @param least logical, whether to select TRUE, the top least associated variables, or FALSE, the most 
#'  associated.
#' @param n.cores integer, if wanting to process the analysis using multiple cores, specify the number
#' @param verbose logical, whether to display additional output
#' @return A set of row or column indexes (depents on 'rows' parameter) of the variables most dependent
#'  (or indepent) variables measured by association with a [continuous/categorical] phenotype.
#' @export
#' @seealso \code{\link{quick.pheno.assocs}}
#' @author Nicholas Cooper 
#' @examples 
#' bmat <- generate.test.matrix(5,big.matrix=TRUE)
#' pheno <- rep(1,ncol(bmat)); pheno[which(runif(ncol(bmat))<.5)] <- 2
#' most.correl <- select.least.assoc(bmat,phenotype=pheno,least=FALSE)
#' least.correl <- select.least.assoc(bmat,phenotype=pheno,least=TRUE)
#' cor(bmat[least.correl,][1,],pheno)  # least correlated
#' cor(bmat[most.correl,][1,],pheno)  # most correlated
select.least.assoc <- function(bigMat,keep=.05,phenotype=NULL,least=TRUE,dir="",n.cores=1,verbose=TRUE)
{
  # select a subset of variables based on the most representative variables in the PCs of a subset
  bigMat <- get.big.matrix(bigMat,dir=dir)
  N <- nrow(bigMat) 
  if(keep>2) {
    new.n <- round(keep)
  } else {
    new.n <- round(max(0,min(1,keep))*N)
  }
  if(is.null(colnames(bigMat)) | is.null(rownames(bigMat)) ) {
    options(bigmemory.allow.dimnames=TRUE)
    if(is.null(colnames(bigMat))) { colnames(bigMat) <- paste("c",1:ncol(bigMat),sep="") }
    if(is.null(rownames(bigMat))) { rownames(bigMat) <- paste("r",1:nrow(bigMat),sep="") }
  }
  if(length(phenotype)!=ncol(bigMat)) { stop("phenotype must be the same length as ncol(bigMat)") }
  sample.info <- data.frame(phenotype=phenotype); rownames(sample.info) <- colnames(bigMat)
  ##association version of selecting subset of snps for PCA
  # takes the 'pc.to.keep'% least associated with #sample.info$phenotype'
  myPs <- quick.pheno.assocs(bigMat=bigMat,sample.info=sample.info,
                             dir=dir,F.values=F,n.cores=n.cores,verbose=verbose)
  rank <- order(myPs)
  if(least) { rank <- rev(rank) }
  if(verbose) {
    #cat("\nsummary of p-values for association tests\n"); print(summary(myPs))
  }
  kept.snps <- rank[1:new.n]
  return(sort(kept.snps))
}


# internal function
cut_fac <- function(N,n.grps,start.zero=FALSE,factor=TRUE) {
  X <- findInterval(((1:N)/(N/n.grps)), 1:n.grps, all.inside=F, rightmost.closed=T)+as.numeric(!start.zero)
  #X <- cut(1:N,n.grps,labels=F)
  if(factor) { X <- as.factor(X) }
  return(X)
}





#' PCA/Singular Value Decomposition for big.matrix
#' 
#' At the time of submission there was no existing native method to conduct principle components
#' analysis (PCA) on big.matrix objects. This function allows singular value decomposition (SVD) of
#' very large matrices, very efficiently, versus the default method. The major speed advantages
#' occur when the 'bigalgebra' package is installed, and when the argument for this function
#' 'SVD'=TRUE. Regular PCA can be conducted using SVD=FALSE but it will be slower and the maximum
#' matrix size able to produce a result, given memory limits, will be smaller. SVD is not exactly
#' the same as PCA, but from my testing the components produced will correlate R>.9 with components
#' of PCA on the same matrix. This function is not completely native to big.matrix objects so 
#' there is one step where the matrix submitted needs to be loaded into memory, so if your 
#' big.matrix object is larger than the allowed size of a standard R-matrix [[which is roughly 3GB; 
#' you can check using NCmisc::estimate.memory()], then this function will fail unless you set the 
#' option 'thin' to a percentage that, multiplied by the original matrix memory-load, is under 3GB.
#' For large matrices in my applications, components produced with thinning are still highly 
#' correlated with components produced using the full dataset. For a breakdown of thinning methods,
#' see the description for the function thin() for more  information. Even with medium sized
#' matrices, for instance 15,000 x 50,000 in size, this function is orders of magnitude faster
#' than the standard R PCA functions, usually running in a matter of minutes, rather than hours
#' or days in examples that I have tested, due to much better handling of memory for internal
#' transpose and eigen operations by using the 'bigmemory' architecture.
#'
#' @param bigMat a big.matrix object, or any argument accepted by get.big.matrix(), which includes
#'  paths to description files or even a standard matrix object.
#' @param dir directory containing the filebacked.big.matrix, and also where the output
#'  file will be saved by default if the save.pcs option is set TRUE. 
#' @param pcs.to.keep integer, number of principle components to keep. Singular Value Decomposition
#'  methods are able to run faster if they don't need to calculate every single PC for a large
#'  matrix. Default is to calculate only the first 50; in practice even fewer than this are generally
#'  used directly. Apart from reducing processing time, this can also reduce storage/RAM burden for 
#'  the resulting matrix. Set to NA, or a number >= min(dim(bigMat)) in order to keep all PCs.
#' @param thin logical, whether to apply \code{thin()} before running PCA. When TRUE, any additional
#'  arguments supplied via \code{...} are forwarded to \code{thin()} (for example \code{keep}, \code{how}, or \code{pref}).
#'  Thinning can be used to reduce very wide matrices to a manageable subset prior to SVD.
#' @param SVD logical, whether to use a Singular Value Decomposition method or a PCA method. The 
#'  eigenvalues and eigenvectors of each alternative will be highly correlated so for most applications,
#'  such as PC-correction, this shouldn't make much difference to the result. However, using SVD=TRUE
#'  can provide significant advantages in speed, or even allow a solution on a matrix that would be
#'  to large to practically compute full PCA. Note that only in SVD mode, and with the bigalgebra
#'  package installed will the full speed advantage of this function be utilised.
#' @param LAP logical, whether to use La.svd() instead of svd() when SVD=TRUE, see base:svd for more info.
#' @param center whether to 'centre' the matrix rows by subtracting rowMeans() before conducting the PCA. This is usually
#'  advisable, although you may wish to skip this if the matrix is already centred to save extra processing.
#'  unlike prcomp there is no option to standardize or use the correlation matrix, if this is desired, please
#'  standardize the bigMat object before running this function. Alternatively, 'center' can be a vector of length
#'  nrow(bigMat) which gives an offset to correct each row by.
#' @param save.pcs whether to save the principle component matrix and eigenvalues to a binary file with name pcs.fn
#' @param pcs.fn name of the binary when save.pcs=TRUE
#' @param return.loadings logical, whether to return the 'loadings' (or the other singular vector when SVD=T); could result in a speed decrease
#' @param verbose whether to display detailed progress of the PCA
#' @param use.bigalgebra logical, whether to use the optional \pkg{bigalgebra}
#'  package for algebraic operations. When available alongside an optimised BLAS
#'  implementation it can accelerate work on very large matrices. Installation
#'  tips for optional acceleration are documented in the README. Set this to
#'  FALSE to skip bigalgebra checks when you know it is unavailable.
#' @param ... additional arguments forwarded to \code{thin()} when \code{thin = TRUE}.
#' @param delete.existing logical, whether to overwrite intermediate file-backed matrices created during thinning.
#' @return A \code{bigpca_result} S3 object containing principal component scores, optional loadings, eigenvalues,
#'  and metadata about the run. Legacy element names such as \code{$PCs} and \code{$Evalues} remain available for
#'  backwards compatibility via list-style access.
#' @export
#' @seealso \code{\link{get.big.matrix}}, \code{\link{PC.correct}}
#' @author Nicholas Cooper
#' @examples 
## PRELIMINARY EXAMPLES: demonstration of PCA versus SVD ##
#' # create an example matrix and its transpose
#' min.dim <- 200; nvar <- 500; subset.size <- 50
#' mat <- matrix(rnorm(min.dim*nvar),ncol=min.dim) 
#' prv.large(mat)
#' t.mat <- t(mat)
#' # create two alternative covariance matrices
#' MMs <- t.mat %*% mat
#' MsM <- mat %*% t.mat
#' # run singular value decomposition
#' pca <- svd(mat)   
#' D <- pca$d # singular values (=sqrt(eigenvalues))
#' V <- pca$v # right singular vector
#' U <- pca$u # left singular vector
#' sig <- mat-mat; diag(sig) <- D; 
#' MMs2 <- V %*% (t(sig) %*% sig) %*% t(V)
#' sig <- t.mat-t.mat; diag(sig) <- D; 
#' MsM2 <- U %*% (sig %*% t(sig)) %*% t(U)
#' # show that the covariance matrices are equal to the functions of 
#' # the left and right singular vectors
#' prv(MMs,MsM); prv(MMs2,MsM2)
#' pr <- princomp(mat) # PCA using eigendecomposition of cov matrix
#' L <- matrix(rep(0,40000),ncol=200); diag(L) <- pr[[1]]^2 # eigenvalues as diag
#' mat2 <- (pr[[2]]) %*% L %*%  solve(pr[[2]]) # = eigenvectors * eigenvalues * inv(eigenvectors)
#' prv.large(cov(mat)); prv.large(mat2) #  == COVmat (may be slight tolerance differences)
#' ## Now demonstrate the correlation between SVD and PCA ##
#' # the right singular vector is highly correlated with the pca loadings:
#' median(abs(diag(cor(V,pr[["loadings"]]))))
#' # the left singular vector is highly correlated with the pca scores (eigenvectors):
#' median(abs(diag(cor(U,pr[["scores"]]))))
#' cor(pr$sdev,D) # the singular values are equivalent to the eigenvalues
#' 
#' ## MAIN EXAMPLES ##
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' if(file.exists("testMyBig.bck")) { unlink(c("testMyBig.bck","testMyBig.dsc")) }
#' bmat <- as.big.matrix(mat,backingfile="testMyBig.bck",
#'  descriptorfile="testMyBig.dsc",  backingpath = getwd())
#' result <- big.PCA(bmat) #,verbose=TRUE)
#' headl(result)
#' # plot the eigenvalues with a linear fit line and elbow placed at 13
#' Eigv <- pca.scree.plot(result$Evalues,M=bmat,elbow=6,printvar=FALSE)
#' rm(bmat) 
#' unlink(c("testMyBig.bck","testMyBig.dsc"))
#' ##  generate some data with reasonable intercorrelations ##
#' mat2 <- sim.cor(500,200,genr=function(n){ (runif(n)/2+.5) })
#' bmat2 <- as.big.matrix(mat2,backingfile="testMyBig2.bck",
#'  descriptorfile="testMyBig2.dsc",  backingpath = getwd())
#' # calculate PCA on decreasing subset size 
#' result2 <- big.PCA(bmat2,thin=FALSE)
#' normal <- result2$PCs; rm(result2)
#' result3 <- big.PCA(bmat2,thin=TRUE,keep=.5, pref="t1")
#' thinned <- result3$PCs; rm(result3)
#' result4 <- big.PCA(bmat2,thin=TRUE,keep=.5, pref="t2", how="cor")
#' corred <- result4$PCs; rm(result4)
#' result5 <- big.PCA(bmat2,thin=TRUE,keep=.5, pref="t3", how="pca")
#' pced <- result5$PCs; rm(result5)
#' result6 <- big.PCA(bmat2,thin=TRUE,keep=.2, pref="t4")
#' thinner <- result6$PCs; rm(result6)
#' ## correlate the resulting PCs with the un-thinned PCs
#' cors.thin.with.orig <- apply(cor(normal,thinned),1,max)
#' cors.corred.with.orig <- apply(cor(normal,corred),1,max)
#' cors.pced.with.orig <- apply(cor(normal,pced),1,max)
#' cors.thinner.with.orig <-apply(cor(normal,thinner),1,max)
#' plot(cors.thin.with.orig,type="l",col="red",ylim=c(0,1))
#' lines(cors.thinner.with.orig,col="orange")
#' lines(cors.corred.with.orig,col="lightblue")
#' lines(cors.pced.with.orig,col="lightgreen")
#' # can see that the first component is highly preserved,
#' # and next components, somewhat preserved; try using different thinning methods
#' rm(bmat2) 
#' unlink(c("testMyBig2.bck","testMyBig2.dsc"))
#' setwd(orig.dir)
big.PCA <- function(
  bigMat,
  dir = getwd(),
  pcs.to.keep = 50,
  thin = FALSE,
  SVD = TRUE,
  LAP = FALSE,
  center = TRUE,
  save.pcs = FALSE,
  use.bigalgebra = TRUE,
  pcs.fn = "PCsEVsFromPCA.RData",
  return.loadings = FALSE,
  verbose = FALSE,
  delete.existing = TRUE,
  ...
) {
  logger <- .bigpca_logger(verbose)
  dirs <- .bigpca_resolve_dirs(dir, c("big", "pc"))
  SVD <- .bigpca_validate_logical_scalar(SVD, "SVD")
  LAP <- .bigpca_validate_logical_scalar(LAP, "LAP")
  use.bigalgebra <- .bigpca_validate_logical_scalar(use.bigalgebra, "use.bigalgebra")
  save.pcs <- .bigpca_validate_logical_scalar(save.pcs, "save.pcs")
  return.loadings <- .bigpca_validate_logical_scalar(return.loadings, "return.loadings")
  delete.existing <- .bigpca_validate_logical_scalar(delete.existing, "delete.existing")
  thin_enabled <- .bigpca_validate_logical_scalar(thin, "thin")
  extra_args <- list(...)
  if (!is.logical(center) && !is.numeric(center)) {
    stop("center must be logical or a numeric vector matching the number of rows.", call. = FALSE)
  }
  pcs_keep <- NA_integer_
  if (!is.na(pcs.to.keep)) {
    pcs_keep <- .bigpca_validate_numeric_scalar(pcs.to.keep, "pcs.to.keep", lower = 1, integer = TRUE)
  }
  thin_arg_names <- character()
  if (thin_enabled) {
    thin_args <- extra_args
    thin_args$bigMat <- bigMat
    thin_args$dir <- dirs
    thin_args$verbose <- verbose
    thin_args$delete.existing <- delete.existing
    thin_args$ret.obj <- TRUE
    thin_args$thin <- NULL
    thin_arg_names <- setdiff(names(thin_args), c("bigMat", "dir", "verbose", "delete.existing", "ret.obj"))
    bigMat <- do.call(thin, thin_args)
    if (inherits(bigMat, "big.matrix") || is.matrix(bigMat)) {
      dims <- dim(bigMat)
      logger("INFO", "Thinned matrix to %d rows x %d columns.", dims[1], dims[2])
    } else {
      logger("INFO", "Thinning complete.")
    }
    extra_args <- list()
  } else if (length(extra_args) > 0L) {
    logger(
      "WARN",
      "Ignoring %d additional argument%s because thin = FALSE.",
      length(extra_args),
      if (length(extra_args) == 1L) "" else "s"
    )
  }
  input <- .bigpca_resolve_input_matrix(bigMat, dirs, logger)
  materialized <- .bigpca_materialize_matrix(input)
  mat <- materialized$matrix
  dims <- dim(mat)
  if (any(dims < 2L, na.rm = TRUE)) {
    stop("Input matrix must have at least two rows and two columns for PCA.", call. = FALSE)
  }
  max_components <- min(dims)
  if (is.na(pcs_keep)) {
    pcs_keep <- max_components
  } else if (pcs_keep > max_components) {
    logger("WARN", "pcs.to.keep (%d) exceeds available components (%d); truncating.", pcs_keep, max_components)
    pcs_keep <- max_components
  }
  preprocess <- .bigpca_preprocess_matrix(mat, center)
  method <- if (!SVD) {
    if (dims[2] > dims[1]) "princomp" else "eigen"
  } else if (LAP) {
    "lapack"
  } else {
    "irlba"
  }
  svd_result <- .bigpca_compute_svd(
    preprocess$matrix,
    pcs_keep,
    return.loadings,
    method = method,
    use_bigalgebra = use.bigalgebra,
    logger = logger
  )
  scores <- svd_result$scores
  components_available <- ncol(scores)
  comp_labels <- paste0("PC", seq_len(components_available))
  if (!is.null(materialized$colnames)) {
    rownames(scores) <- materialized$colnames
  }
  colnames(scores) <- comp_labels
  loadings <- svd_result$loadings
  if (!is.null(loadings)) {
    if (!is.null(materialized$rownames)) {
      rownames(loadings) <- materialized$rownames
    }
    colnames(loadings) <- comp_labels
  }
  eigenvalues <- svd_result$eigenvalues
  eigenvalues <- eigenvalues[seq_len(components_available)]
  metadata <- list(
    method = svd_result$method,
    call = match.call(),
    centered = preprocess$centered,
    source_dim = c(rows = dims[1], cols = dims[2]),
    thin = thin_enabled,
    thin_args = thin_arg_names,
    components_requested = pcs_keep
  )
  result <- .bigpca_construct_result(
    scores = scores,
    eigenvalues = eigenvalues,
    loadings = loadings,
    center = preprocess$center,
    metadata = metadata
  )
  if (save.pcs) {
    output_path <- cat.path(dirs$pc, pcs.fn)
    logger("INFO", "Saving PCA outputs to %s", output_path)
    PCs <- result$scores
    Evalues <- result$eigenvalues
    loadings_obj <- result$loadings
    save(result, PCs, Evalues, loadings_obj, file = output_path)
  }
  result
}






#' Correct a big.matrix by principle components
#' 
#' Principle components (PC) can be used as a way of capturing bias (when common variance represents bias)
#' and so PC correction is a way to remove such bias from a dataset. Using the first 'n' PCs from an 
#' an analysis performed using big.PCA(), this function will transform the original matrix by regressing
#' onto the 'n' principle components (and optionally gender) and returing the residuals. The result
#' is returned as a big.matrix object, so that objects larger than available RAM can be processed, and
#' multiple processors can be utilised for greater speed for large datasets.
#' 
#' @param pca.result result returned by 'big.PCA()' (a \code{bigpca_result}), a list containing
#'  principal component scores, or the path to an R binary file containing such an object.
#' @param bigMat a big.matrix with exactly corresponding samples (columns) to those submitted to PCA prior to correction
#' @param dir directory containing the big.matrix backing file
#' @param num.pcs number of principle components (or SVD components) to correct for
#' @param n.cores number of cores to use in parallel for processing
#' @param pref prefix to add to the file name of the resulting corrected matrix backing file
#' @param big.cor.fn instead of using 'pref' directly specify the desired file name
#' @param write whether to write the result to a file.backed big.matrix or to simply
#'  return a pointer to the resulting corrected big.matrix
#' @param sample.info if using 'correct.sex=TRUE' then this object should be
#'  a dataframe containing the sex of each sample, with sample names as rownames
#' @param correct.sex if sample.info is a dataframe containing a column named 'gender' or 'sex'
#'  (case insensitive), then add a sex covariate to the PC correction linear model
#' @param add.int logical, whether to maintain the pre-corrected means of each variable, i.e, post-correction
#'  add the mean back onto the residuals which will otherwise have mean zero for each variable.
#' @param preserve.median logical, if add.int=TRUE, then setting this parameter to TRUE will preserve
#'  the median of the original data, instead of the mean. This is because after PC-correction the 
#'  skew may change.
#' @param tracker logical, whether to display a progress bar
#' @param verbose logical, whether to display preview of pre- and post- corrected matrix
#' @param delete.existing logical, whether to remove existing file-backed artefacts with the same prefix before
#'  writing the corrected output. Set to FALSE to retain previous results.
#' @return A big.matrix of the same dimensions as original, corrected for n PCs and an optional covariate (sex)
#' @export
#' @seealso \code{\link{big.PCA}}
#' @author Nicholas Cooper 
#' @examples 
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' if(file.exists("testMyBig.bck")) { unlink(c("testMyBig.bck","testMyBig.dsc")) }
#' mat2 <- sim.cor(500,200,genr=function(n){ (runif(n)/2+.5) })
#' bmat2 <- as.big.matrix(mat2,backingfile="testMyBig.bck",
#'  descriptorfile="testMyBig.dsc",  backingpath = getwd())
#' ## calculate PCA ##
#' \donttest{ result2 <- big.PCA(bmat2,thin=FALSE)
#' corrected <- PC.correct(result2,bmat2)
#' corrected2 <- PC.correct(result2,bmat2,n.cores=2)
#' c1 <- get.big.matrix(corrected) ; c2 <- get.big.matrix(corrected2)
#' all.equal(as.matrix(c1),as.matrix(c2)) }
#' rm(bmat2) 
#' unlink(c("testMyBig.bck","testMyBig.dsc"))
#' setwd(orig.dir) # reset working dir to original
PC.correct <- function(
  pca.result,
  bigMat,
  dir = getwd(),
  num.pcs = 9,
  n.cores = 1,
  pref = "corrected",
  big.cor.fn = NULL,
  write = FALSE,
  sample.info = NULL,
  correct.sex = FALSE,
  add.int = FALSE,
  preserve.median = FALSE,
  tracker = TRUE,
  verbose = TRUE,
  delete.existing = TRUE
) {
  logger <- .bigpca_logger(verbose)
  dirs <- .bigpca_resolve_dirs(dir, c("big", "pc"))
  num.pcs <- .bigpca_validate_numeric_scalar(num.pcs, "num.pcs", lower = 1, integer = TRUE)
  n.cores <- .bigpca_validate_numeric_scalar(n.cores, "n.cores", lower = 1, integer = TRUE)
  write <- .bigpca_validate_logical_scalar(write, "write")
  correct.sex <- .bigpca_validate_logical_scalar(correct.sex, "correct.sex")
  add.int <- .bigpca_validate_logical_scalar(add.int, "add.int")
  preserve.median <- .bigpca_validate_logical_scalar(preserve.median, "preserve.median")
  tracker <- .bigpca_validate_logical_scalar(tracker, "tracker")
  delete.existing <- .bigpca_validate_logical_scalar(delete.existing, "delete.existing")

  origMat <- get.big.matrix(bigMat, dirs$big)
  nR <- nrow(origMat)
  nC <- ncol(origMat)
  if (nR < 2L || nC < 2L) {
    stop("'bigMat' must have at least two rows and two columns for PC correction.", call. = FALSE)
  }
  rN <- rownames(origMat)
  cN <- colnames(origMat)
  scores <- .bigpca_extract_scores(pca.result, dirs, logger)
  scores <- as.matrix(scores)
  if (!is.null(rownames(scores)) && !is.null(cN)) {
    idx <- match(cN, rownames(scores))
    if (anyNA(idx)) {
      stop("PCA scores did not contain all samples present in 'bigMat'.", call. = FALSE)
    }
    scores <- scores[idx, , drop = FALSE]
  } else if (nrow(scores) != nC) {
    stop("Number of PCA scores rows does not match the number of columns in 'bigMat'.", call. = FALSE)
  }
  if (num.pcs > ncol(scores)) {
    logger("WARN", "Requested %d PCs but only %d available; truncating.", num.pcs, ncol(scores))
    num.pcs <- ncol(scores)
  }
  design <- scores[, seq_len(num.pcs), drop = FALSE]
  design <- as.matrix(design)
  if (!is.numeric(design)) {
    storage.mode(design) <- "numeric"
  }
  if (correct.sex) {
    if (is.null(sample.info) || !is.data.frame(sample.info)) {
      stop("sample.info must be a data.frame when correct.sex = TRUE.", call. = FALSE)
    }
    if (is.null(rownames(sample.info))) {
      stop("sample.info must have rownames matching the sample identifiers.", call. = FALSE)
    }
    coln <- which(tolower(colnames(sample.info)) %in% c("gender", "sex"))
    if (!length(coln)) {
      stop("sample.info must contain a 'sex' or 'gender' column when correct.sex = TRUE.", call. = FALSE)
    }
    idx <- match(cN, rownames(sample.info))
    if (anyNA(idx)) {
      stop("sample.info is missing entries for some samples in 'bigMat'.", call. = FALSE)
    }
    sex.vec <- sample.info[idx, coln[1]]
    if (!is.numeric(sex.vec)) {
      sex.vec <- as.numeric(factor(sex.vec))
      logger("INFO", "Converted categorical sex labels to numeric codes for regression.")
    }
    sex.vec[is.na(sex.vec)] <- mean(sex.vec, na.rm = TRUE)
    design <- cbind(sex = sex.vec, design)
  }
  design <- cbind(intercept = 1, design)
  logger(
    "INFO",
    "Performing PC correction on %d variables across %d samples using %d component%s%s.",
    nR,
    nC,
    num.pcs,
    if (num.pcs == 1L) "" else "s",
    if (correct.sex) " with sex covariate" else ""
  )

  backingfile <- sprintf("%s.bck", pref)
  descriptorfile <- sprintf("%s.dsc", pref)
  if (delete.existing) {
    unlink(cat.path(dirs$big, backingfile))
    unlink(cat.path(dirs$big, descriptorfile))
  }
  options(bigmemory.allow.dimnames = TRUE)
  pcCorMat <- bigmemory::filebacked.big.matrix(
    nrow = nR,
    ncol = nC,
    backingfile = backingfile,
    descriptorfile = descriptorfile,
    backingpath = dirs$big,
    dimnames = list(rN, cN)
  )
  if (!bigmemory::is.big.matrix(pcCorMat)) {
    stop("Failed to initialise file-backed matrix for corrected data.", call. = FALSE)
  }

  multi <- n.cores > 1L && .Platform$OS.type != "windows"
  if (n.cores > 1L && !multi) {
    logger("WARN", "Parallel correction is not available on this platform; using a single core.")
  } else if (multi) {
    logger("INFO", "Using %d cores for row-wise correction.", n.cores)
  }

  nsamples <- nC
  snps.per.proc <- 400L
  flush.freq <- 20L
  if (nsamples > 10000L) snps.per.proc <- 300L
  if (nsamples > 20000L) snps.per.proc <- 150L
  if (nsamples > 40000L) snps.per.proc <- 50L
  if (nsamples > 80000L) snps.per.proc <- 10L
  if (n.cores > 5L) {
    snps.per.proc <- snps.per.proc * 2L
    flush.freq <- flush.freq * 2L
  }
  if (n.cores > 15L) {
    snps.per.proc <- snps.per.proc * 2L
  }
  if (snps.per.proc < n.cores) {
    n.cores <- max(1L, snps.per.proc %/% 5L)
  }
  stepz <- round(seq(from = 1, to = nR + 1, by = snps.per.proc))
  if (tail(stepz, 1) != nR + 1L) {
    stepz <- c(stepz, nR + 1L)
  }
  sc.lst <- head(tail(stepz, 2), 1)
  lst <- tail(stepz, 1)
  if (lst - sc.lst <= 4L) {
    ll <- length(stepz)
    if (ll > 2L) {
      stepz <- stepz[-(ll - 1L)]
    } else {
      logger("WARN", "Very small number of rows detected; chunking may be inefficient.")
    }
  }
  split.to <- length(stepz) - 1L
  start_time <- proc.time()[3]
  for (dd in seq_len(split.to)) {
    x1 <- stepz[dd]
    x2 <- stepz[dd + 1L] - 1L
    next.rows <- sub.big.matrix(origMat, firstRow = x1, lastRow = x2, backingpath = dirs$big)
    chunk <- bigmemory::as.matrix(next.rows)
    if (multi) {
      corrected <- PC.fn.mat.multi(chunk, design, mc.cores = n.cores, add.int = add.int, pm = preserve.median)
    } else {
      corrected <- PC.fn.mat.apply(chunk, design, add.int = add.int, pm = preserve.median)
    }
    pcCorMat[x1:x2, ] <- corrected
    if (tracker) {
      loop.tracker(dd, split.to)
    }
    if (dd %% flush.freq == 0L) {
      bigmemory::flush(pcCorMat)
      gc()
      descriptor <- bigmemory::describe(pcCorMat)
      rm(pcCorMat)
      pcCorMat <- bigmemory::attach.big.matrix(descriptor, path = dirs$big)
    }
    rm(next.rows)
  }
  bigmemory::flush(pcCorMat)
  descriptor <- bigmemory::describe(pcCorMat)
  elapsed_minutes <- (proc.time()[3] - start_time) / 60
  logger("INFO", "PC correction completed in %.2f minutes.", elapsed_minutes)
  if (verbose) {
    prv.big.matrix(pcCorMat, name = "pcCorMat")
  }
  if (write) {
    if (is.null(big.cor.fn) || !nzchar(big.cor.fn)) {
      big.cor.fn <- sprintf("describePCcorrect%d.RData", num.pcs)
    }
    output_path <- cat.path(dirs$big, big.cor.fn)
    save(descriptor, file = output_path)
    logger("INFO", "Saved PC-corrected matrix descriptor to %s", output_path)
    return(big.cor.fn)
  }
  descriptor
}



#' Transpose function for big.matrix objects
#'
#' At the time of writing, there is no transpose method for big.matrix()
#' This function returns a new filebacked big.matrix which is the transpose of the input
#' big.matrix. max.gb allows periodic manual flushing of the memory to be conducted in case
#' the built-in memory management of R/bigmemory is not working as desired.
#' This method is a non-native (not using the raw C objects from the package but merely
#' standard R accessors and operations) algorithm to transpose a big matrix efficiently
#' for memory usage and speed. A blank matrix is created on disk and the data is
#' block-wise transposed and buffered into the new matrix.
#'
#' @param bigMat default, a big.matrix(), although if 'file.ok' is set TRUE, then
#'  this can be a big.matrix descriptor, or a file location
#' @param dir the directory for the matrix backing file (preferably for both the original
#'  and the proposed transposed matrix). If this is left NULL and bigMat contains a path,
#'  this path (via dirname(bigMat)) will be used; if it doesn't contain a path the current
#'  working directory will be used
#' @param name the basename of the new transposed matrix
#' @param R.descr the name of a binary file that will store the big.matrix.descriptor
#'  for the transposed matrix. If "" then the descriptor won't be saved. If NULL, then
#'  it will be <name>.RData
#' @param max.gb the maximum number of GB of data to process before flushing the big.matrix
#' @param verbose whether to print messages about each stage of the process
#' @param tracker whether to use a progress bar. NA means it will only be used if the matrix
#'  in question is larger than 1GB.
#' @param file.ok whether to accept big.matrix.descriptors or filenames as input for 
#'  'bigMat'; if T, then anything that works with get.big.matrix(bigMat,dir) is acceptable
#' @param delete.existing logical, whether to overwrite any existing transposed matrices that share the
#'  same prefix before writing the result.
#' @return A big.matrix that is the transpose (rows and columns switched) of the original matrix
#' @export
#' @examples 
#' orig.dir <- getwd(); setwd(tempdir()); # move to temporary dir
#' if(file.exists("test.bck")) { unlink(c("test.bck","test.dsc")) }
#' bM <- filebacked.big.matrix(200, 500,
#'        dimnames = list(paste("r",1:200,sep=""), paste("c",1:500,sep="")),
#'        backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
#' bM[1:200,] <- replicate(500,rnorm(200))
#' prv.big.matrix(bM)
#' tbM <- big.t(bM,verbose=TRUE)
#' prv.big.matrix(tbM)
#' rm(tbM)
#' rm(bM)  
#' unlink(c("t.bigMat.RData","t.bigMat.bck","t.bigMat.dsc","test.bck","test.dsc"))
#' setwd(orig.dir)
big.t <- function(bigMat,dir=NULL,name="t.bigMat",R.descr=NULL,max.gb=NA,
                  verbose=F,tracker=NA,file.ok=T,delete.existing=TRUE) {
  #this can be slow!
  if(is.null(R.descr)) { R.descr <- cat.path(dirname(name),name,ext="RData") }
  if(!is.big.matrix(bigMat)) {
    if(is.matrix(bigMat) | is.data.frame(bigMat)) {
      warning("just a regular matrix, used t()") ; return(t(bigMat)) 
    } else {
      if(file.ok) { 
        # bigMat can be a file path
        if(is.null(dir) & file.exists(bigMat)) {
          if(dirname(bigMat)!=".") { dir <- dirname(bigMat) }
        }
        try(bigMat <- get.big.matrix(bigMat,dir)) 
      }
      if(!is.big.matrix(bigMat)) {
        stop("invalid object for big.matrix transposition")    
      }
    }
  } else {
    if(is.null(dir)) { dir <- getwd() }
  }
  slow.is <- 1 #GB  [ above this if tracker = NA, then do a progress bar ]
  nR <- nrow(bigMat); nC <- ncol(bigMat)
  if(is.na(tracker)) { if(estimate.memory(c(nR,nC))>slow.is) { tracker <- T } else { tracker <- F } }
  cN <- colnames(bigMat); rN <- rownames(bigMat)
  if(verbose) { cat(" creating",nC,"x",nR,"target matrix,",name,"...") }
  des <- paste(name,"dsc",sep=".")
  bck <- paste(name,"bck",sep=".")
  if(file.exists(cat.path(dir,bck)) & delete.existing) { unlink(cat.path(dir,bck)) }
  bigTrans <- big.matrix(nrow=nC,ncol=nR, backingfile=bck,
                         backingpath=dir, descriptorfile=des)
  if(verbose) { cat("done\n"); cat("\nAdding names\n") }
  options(bigmemory.allow.dimnames=TRUE)
  colnames(bigTrans) <- rN
  if(verbose) { cat(" added colnames\n") }
  rownames(bigTrans) <- cN
  if(verbose) { cat(" added rownames\n") }
  d2 <- d1 <- 0
  #try({
  split.to <- 10*round(estimate.memory(bigMat)) # split into .1GB chunks, save RAM without creating groups too small to process
  #if(n.cores>4) { split.to <- split.to * 4 } # divide more if using multicores
  stepz <- round(seq(from=1,to=nC+1,length.out=round((split.to+1))))
  # if the last step is too small, merge with previous
  sc.lst <- head(tail(stepz,2),1); lst <- tail(stepz,1)
  if((tail(stepz,1)) != nC+1) { stepz <- c(stepz,nC+1) }
  if(lst-sc.lst <=2) { ll <- length(stepz); if(ll>2) { stepz <- stepz[-(ll-1)] } else { warning("number of columns quite small, may cause issues") } }
  #
  split.to <- length(stepz)-1
  if(verbose) { cat(" transposing 'bigMat' into new big.matrix object:\n") }
  if(is.na(max.gb)) { max.gb <- split.to + 10 }
  for (cc in 1:split.to)
  {
    # within submatrix cols
    c1 <- stepz[cc]; c2 <- stepz[cc+1]-1  # check this in FN!
    # do the copying
    lilColRange <- c(c1:c2)
    if(tracker) {      loop.tracker(cc,split.to) }
    if(is.finite(sum(as.numeric(lilColRange)))) {
      #cat(range(lilColRange)); cat(dim(bigTrans)); cat(dim(bigMat))
      bigTrans[lilColRange,1:nR] <- t(bigMat[1:nR,lilColRange])
    } else {
      cat(" Warning: empty interval ignored\n")
    }
    if(cc %% (max.gb*10) == 0) {
      # reset memory after every 'max.gb' 1GB chunks to prevent skyrocketing RAM use #
      fl.suc <- bigmemory::flush(bigTrans) ;  if(!fl.suc) { cat("flush failed\n") } ; gc()  
      if(T) {
        RR <- bigmemory::describe(bigTrans); rm(bigTrans); bigTrans <- attach.big.matrix(RR,path=dir)
      }
    }
  }
  #})
  if(verbose) { cat(" combining complete, converting result to big matrix\n") }
  descr <- bigmemory::describe(bigTrans)
  bigmemory::flush(bigTrans) # hopefully this will ensure the row/colnames are added to the file backing
  
  if(verbose) {
    cat(paste(" created big.matrix description file:",des,"\n"))
    cat(paste(" created big.matrix backing file:",bck,"\n"))
    if(length(R.descr)>0 & all(R.descr!="")) { 
      save(descr,file=cat.path(dir,R.descr)) 
      cat(paste(" created big.matrix binary description file:",basename(R.descr),"\n"))
    }
  }
  return(bigTrans)
}


### INTERNAL FUNCTIONS ###
#' Internal
#' pm is 'preserve.median'
PC.fn.mat <- function(next.rows, nPCs, add.int=FALSE, pm=FALSE)
{
  # matrix version of PC.fn (used to PC-correct one SNP at a time)
  col.sel <- 1:ncol(next.rows)
  for (dd in 1:nrow(next.rows)) {
    # compiled PC.fn should speed up these ops a little
    next.rows[dd,] <- PC.fn(next.rows[dd,],nPCs,col.sel,add.int=add.int,pm=pm) 
  }  
  return(next.rows)
}


#' Internal
#' pm is 'preserve.median'
PC.fn.mat.apply <- function(nextrows, nPCs, add.int=F, pm=FALSE)
{
  # matrix version of PC.fn (used to PC-correct one SNP at a time), vectorized version
  # testing shows the for-loop (non-vectorized) to be slightly faster, maybe because of t()
  # when using PC.fn.2 must pass in vec of 1's if you want the intecept
  nc <- ncol(nextrows); if(nc<1) { warning("nextrows had less than 1 column"); return(rep(0,length(nextrows))) }
  col.sel <- 1:nc
  nextrows <- t(apply(nextrows,1,PC.fn.2,nPCs=nPCs,col.sel=col.sel,add.int=add.int,pm=pm))
  return(nextrows)
}


#' Internal
PC.fn.mat.multi <- function(nextrows, nPCs, mc.cores=1, add.int=F, pm=FALSE)
{
  # matrix version of PC.fn (used to PC-correct one SNP at a time), vectorized version
  # testing shows the for-loop (non-vectorized) to be slightly faster, maybe because of t()
  # when using PC.fn.2 must pass in vec of 1's if you want the intecept
  #mc.cores <- 2
  col.sel <- 1:ncol(nextrows)
  nextrows <- lapply(seq_len(nrow(nextrows)), function(i) nextrows[i,]) # multi slows this down
  #nextrows <- multicore::mclapply(nextrows,PC.fn,nPCs=nPCs,col.sel=col.sel,mc.cores=mc.cores)
 # cat("~")
 # save(nextrows,nPCs,col.sel,mc.cores,add.int,pm,file="TESTOPOO.RData"); stop()
  nextrows <- parallel::mclapply(nextrows,PC.fn.2,nPCs=nPCs,col.sel=col.sel,mc.cores=mc.cores, add.int=add.int, pm=pm)
 # cat("+")
  nextrows <- do.call("rbind",nextrows)
 # cat("`")
  return(nextrows)
}


#' Internal
PC.fn.previous <- function(next.row, nPCs, col.sel, add.int=F)
{
  # apply PC correction for a single SNP, allowing for missing data.
  bad1 <- which(is.na(next.row))
  if(length(bad1)>0) { sel <- -bad1 } else { sel <- col.sel }
  if(add.int) {  int <- mean(next.row[sel]) } else { int <- 0 }
  next.row[sel] <- lm(next.row ~ nPCs,na.action="na.exclude")$residuals + int
  return(next.row)
}

PC.fn <- function(next.row, nPCs, col.sel, add.int=F, pm=FALSE)
{
  # apply PC correction for a single SNP, allowing for missing data.
  bad1 <- which(is.na(next.row))
  if(length(bad1)>0) { sel <- -bad1 } else { sel <- col.sel }
  if(add.int) { 
    if(pm) { 
      int <- median(next.row[sel])
      next.row[sel] <- lm(next.row ~ nPCs,na.action="na.exclude")$residuals
      next.row[sel] <- next.row[sel] - median(next.row[sel]) + int
    } else { 
      int <- mean(next.row[sel]) 
      next.row[sel] <- lm(next.row ~ nPCs,na.action="na.exclude")$residuals + int
    }
  } else { 
    next.row[sel] <- lm(next.row ~ nPCs,na.action="na.exclude")$residuals
  }
  return(next.row)
}


#' Internal
PC.fn.2 <- function(next.row, nPCs, col.sel, add.int=F, pm=FALSE)
{
  # apply PC correction for a single SNP, allowing for missing data.
  # when using PC.fn.2 must pass in vec of 1's if you want the intecept
  bad1 <- which(is.na(next.row))
  if(length(bad1)>0) { sel <- -bad1 } else { sel <- col.sel }
  if(add.int) { 
    if(pm) { 
      int <- median(next.row[sel])
      next.row[sel] <- lm.fit(x=nPCs[sel,],y=next.row[sel])$residuals
      next.row[sel] <- next.row[sel] - median(next.row[sel]) + int
    } else { 
      int <- mean(next.row[sel]) 
      next.row[sel] <- lm.fit(x=nPCs[sel,],y=next.row[sel])$residuals + int
    }
  } else { 
    #prv(nPCs[sel,],next.row[sel])
    next.row[sel] <- stats::lm.fit(x=nPCs[sel,],y=next.row[sel])$residuals
    #next.row[sel] <- stats::lm.fit(x=sel*2,y=sel+rnorm(length(sel)))$residuals
    #next.row[sel] <- rep(1,length(sel))
  }
  return(next.row)
}


#' Internal
PC.fn.2.previous <- function(next.row, nPCs, col.sel, add.int=F, pm=FALSE)
{
  # apply PC correction for a single SNP, allowing for missing data.
  # when using PC.fn.2 must pass in vec of 1's if you want the intecept
  bad1 <- which(is.na(next.row))
  if(length(bad1)>0) { sel <- -bad1 } else { sel <- col.sel }
  if(add.int) { int <- mean(next.row[sel]) } else { int <- 0 }
  next.row[sel] <- lm.fit(x=nPCs[sel,],y=next.row[sel])$residuals + int
  return(next.row)
}


#' Internal
matmul <- function(A, x, transpose=FALSE)
{
  # bigalgebra friendly version of matrix multiplier function
  if(transpose) {
    return(t( t(x) %*% A)) 
  } else {
    return (A %*% x) 
  }
}


