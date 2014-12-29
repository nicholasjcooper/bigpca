Crossprod <- function(x,y=NULL,always.big=FALSE) {
  use.big.size <- 1 # size in GB to make big.matrix worth using
  typ.x <- is(x)[1]
  if(typ.x=="big.matrix") {
    if(estimate.memory(x)<use.big.size) {
       # if less than 1GB, run transpose as a regular matrix
       tx <- t(as.matrix(x))
    } else {
       tx <- big.t(x)
    }
  } else {
    if(is.null(y) | (is(y)[1]=="matrix") ) { return(crossprod(x,y)) }
    if(typ.x %in% c("matrix","data.frame")) {
      tx <- t(x) 
    } else { 
      stop("x should be a matrix/big.matrix") 
    }
  }
  if(is.null(y)) { y <- x }
  typ.tx <- is(tx)[1]; typ.y <- is(y)[1]
  if(typ.tx!="big.matrix" & typ.y!="big.matrix") {
     return(tx %*% y)
  }
  if(typ.tx!="big.matrix") {
    if(estimate.memory(y)>=use.big.size) { 
     # conv tx to big
     tx <- as.big.matrix(tx)
    } else {
     # conv y to reg
     y <- as.matrix(y)
    }
  } else {
    if(typ.y!="big.matrix") {
      if(estimate.memory(x)>=use.big.size) { 
       # conv y to big
       y <- as.big.matrix(y)
      } else {
       # conv tx to reg
       tx <- as.matrix(tx)
      }
    }
  }
  if(is(tx)[1]!=is(y)[1]) { stop("matrix types got out of sync, tx was ",is(tx)[1]," and y was ",is(y)[1]) }
  if(is(y)[1]=="big.matrix") { cat("using bigalgebra to calculate crossproduct for large matrices"); track <- TRUE  } else { track <- FALSE }
  iii <- (tx %*% y)
  if(track) { cat("complete\n" ) }
  if(!always.big) {
    if(estimate.memory(iii)<use.big.size) {
       # if less than 1GB, return result as a regular matrix
       iii <- as.matrix(iii)
    } else {
      #print(estimate.memory)  ## what what what?
    }
  }  else {
    if(is(iii)[1]!="big.matrix") { iii <- as.big.matrix(iii) }      
  }  
  return(iii)
}


crossprod2 <- function(x,y=NULL) { 
  if(is(x)[1]=="big.matrix") {
    x <- as.matrix(x)
  } 
  if(!is.null(y)) { 
    if(is(y)[1]=="big.matrix") {
      y <- as.matrix(y) 
      cat("Dim X:",comma(Dim(x)),"Dim Y:",comma(Dim(y)),"\n")
    }
  }
  return(crossprod(x,y))
}

.orthog <- function(Y, X) {
  if(is(Y)[1]=="big.matrix") {
    if(is(X)[1]!="big.matrix") { X <- as.big.matrix(X) } 
    use.big <- TRUE
  } else {
    if(is(X)[1]=="big.matrix") { Y <- as.big.matrix(Y) ; use.big <- TRUE } else { use.big <- FALSE }
  }  
  if (dim(X)[2] < dim(Y)[2]) {
    dotY <- Crossprod(X, Y, always.big=use.big)
  } else {
    uU <- Crossprod(Y, X, always.big=use.big) 
    if(is(uU)[1]=="big.matrix") { dotT <- big.t(uU) } else { dotY <- t(uU) }
  }
  iii <- Y - X %*% dotY
  if(estimate.memory(iii)<10) {
    # if less than 10GB, return result as a regular matrix
    iii <- as.matrix(iii)
  }  else {
    if(is(iii)[1]=="big.matrix") { warning(".orthog() returned result as big.matrix as was too large for a regular matrix") }
  }
  return(iii)
}

#crossprod2 <- crossprod

irlba2 <- function (A, nu = 5, nv = 5, adjust = 3, aug = c("ritz", "harm"), 
    sigma = c("ls", "ss"), maxit = 1000, m_b = 20, reorth = 2,  
    tol = 1e-06, V = NULL, matmul = NULL) 
{
    eps <- .Machine$double.eps
    options(digits.secs = 3)
    m <- nrow(A)
    n <- ncol(A)
    k <- max(nu, nv)
    interchange <- FALSE
    sigma = match.arg(sigma)
    aug = match.arg(aug)
    if (n > m && sigma == "ss") {
        t <- m
        m <- n
        n <- t
        interchange <- TRUE
    }
    k_org <- k
    k <- k + adjust
    if (k <= 0) 
        stop("max(nu,nv)+adjust must be positive")
    if (k > min(m, n)) 
        stop("max(nu,nv) must be less than min(nrow(A),ncol(A))")
    if (m_b <= 1) 
        stop("m_b must be greater than 1")
    if (tol < 0) 
        stop("tol must be non-negative")
    if (maxit <= 0) 
        stop("maxit must be positive")
    if (m_b >= min(n, m)) {
        m_b <- floor(min(n, m) - 0.1)
        if (m_b - k - 1 < 0) {
            adjust <- 0
            k <- m_b - 1
        }
    }
    if (m_b - k - 1 < 0) 
        m_b <- ceiling(k + 1 + 0.1)
    if (m_b >= min(m, n)) {
        m_b <- floor(min(m, n) - 0.1)
        adjust <- 0
        k <- m_b - 1
    }
    if (tol < eps) 
        tol <- eps
    W <- matrix(0, m, m_b)
    F1 <- matrix(0, n, 1)
    if (is.null(V)) {
        V <- matrix(0, n, m_b)
        V[, 1] <- rnorm(n)
    }
    else {
        V <- cbind(V, matrix(0, n, m_b - ncol(V)))
    }
    #####prv(W,F1,V)
    B <- NULL
    Bsz <- NULL
    eps23 <- eps^(2/3)
    I <- NULL
    J <- NULL
    iter <- 1
    mprod <- 0
    R_F <- NULL
    sqrteps <- sqrt(eps)
    Smax <- 1
    Smin <- NULL
    SVTol <- max(sqrteps, tol)
    S_B <- NULL
    U_B <- NULL
    V_B <- NULL
    V_B_last <- NULL
    S_B2 <- NULL
    U_B2 <- NULL
    V_B2 <- NULL
    norm2 <- function(x) return(as.numeric(sqrt(Crossprod(x))))
    convtests <- function(Bsz, tol, k_org, U_B, S_B, V_B, residuals, 
        k, SVTol, Smax) {
        Len_res <- sum(residuals[1:k_org] < tol * Smax)
        if (Len_res == k_org) {
            return(list(converged = TRUE, U_B = U_B[, 1:k_org, 
                drop = FALSE], S_B = S_B[1:k_org, drop = FALSE], 
                V_B = V_B[, 1:k_org, drop = FALSE], k = k))
        }
        Len_res <- sum(residuals[1:k_org] < SVTol * Smax)
        k <- max(k, k_org + Len_res)
        if (k > Bsz - 3) 
            k <- Bsz - 3
        return(list(converged = FALSE, U_B = U_B, S_B = S_B, 
            V_B = V_B, k = k))
    }
    while (iter <= maxit) {
        j <- 1
        if (iter == 1) 
            V[, 1] <- V[, 1, drop = FALSE]/norm2(V[, 1, drop = FALSE])
        else j <- k + 1
        if (!is.null(matmul)) {
            if (interchange) 
                W[, j] <- matmul(A, V[, j, drop = FALSE], transpose = TRUE)
            else W[, j] <- matmul(A, V[, j, drop = FALSE])
        }
        else {
            if (interchange) 
                W[, j] <- t(as.matrix(Crossprod(V[, j, drop = FALSE], 
                  A)))
            else W[, j] <- as.matrix(A %*% V[, j, drop = FALSE])
        }
        mprod <- mprod + 1
        if (iter != 1) {
            W[, j] <- .orthog(W[, j, drop = FALSE], W[, 1:(j - 
                1), drop = FALSE])
        }
        S <- norm2(W[, j, drop = FALSE])
        if ((S < SVTol) && (j == 1)) 
            stop("Starting vector near the null space")
        if (S < SVTol) {
            W[, j] <- rnorm(nrow(W))
            W[, j] <- .orthog(W[, j, drop = FALSE], W[, 1:(j - 
                1), drop = FALSE])
            W[, j] <- W[, j, drop = FALSE]/norm2(W[, j, drop = FALSE])
            S <- 0
        }
        else W[, j] <- W[, j, drop = FALSE]/S
        while (j <= m_b) {
            if (!is.null(matmul)) {
                if (interchange) 
                  F1 <- matmul(A, W[, j, drop = FALSE])
                else F1 <- matmul(A, W[, j, drop = FALSE], transpose = TRUE)
            }
            else {
                if (interchange) {
                  F1 <- as.matrix(A %*% W[, j, drop = FALSE])
                } else {
                  uU <- Crossprod(W[, j, drop = FALSE], A)
                  #prv(uU)
                  if(is(uU)[1]=="big.matrix") { F1 <- big.t(uU) } else { F1 <- t(as.matrix(uU)) }
                 }
            }
            mprod <- mprod + 1
            F1 <- F1 - S * V[, j, drop = FALSE]
            F1 <- .orthog(F1, V[, 1:j, drop = FALSE])
            if (j + 1 <= m_b) {
                R <- norm2(F1)
                if (R <= SVTol) {
                  F1 <- matrix(rnorm(dim(V)[1]), dim(V)[1], 1)
                  F1 <- .orthog(F1, V[, 1:j, drop = FALSE])
                  V[, j + 1] <- F1/norm2(F1)
                  R <- 0
                }
                else V[, j + 1] <- F1/R
                if (is.null(B)) 
                  B <- cbind(S, R)
                else B <- rbind(cbind(B, 0), c(rep(0, j - 1), 
                  S, R))
                if (!is.null(matmul)) {
                  if (interchange) 
                    W[, j + 1] <- matmul(A, V[, j + 1, drop = FALSE], 
                      transpose = TRUE)
                  else W[, j + 1] <- matmul(A, V[, j + 1, drop = FALSE])
                }
                else {
                  if (interchange) 
                    W[, j + 1] <- t(as.matrix(Crossprod(V[, j + 
                      1, drop = FALSE], A)))
                  else W[, j + 1] <- as.matrix(A %*% V[, j + 
                    1, drop = FALSE])
                }
                mprod <- mprod + 1
                W[, j + 1] <- W[, j + 1, drop = FALSE] - W[, 
                  j, drop = FALSE] * R
                if (iter == 1 || reorth == 2) 
                  W[, j + 1] <- .orthog(W[, j + 1, drop = FALSE], 
                    W[, 1:j, drop = FALSE])
                S <- norm2(W[, j + 1, drop = FALSE])
                if (S <= SVTol) {
                  W[, j + 1] <- rnorm(nrow(W))
                  W[, j + 1] <- .orthog(W[, j + 1, drop = FALSE], 
                    W[, 1:j, drop = FALSE])
                  W[, j + 1] <- W[, j + 1, drop = FALSE]/norm2(W[, 
                    j + 1, drop = FALSE])
                  S <- 0
                }
                else W[, j + 1] <- W[, j + 1, drop = FALSE]/S
            }
            else {
                B <- rbind(B, c(rep(0, j - 1), S))
            }
            j <- j + 1
        }
        Bsz <- nrow(B)
        R_F <- norm2(F1)
        F1 <- F1/R_F
        #prv(B)     # how big is the matrix passed to svd? only 20 x 20
        Bsvd <- svd(B)
        if (iter == 1) {
            Smax <- Bsvd$d[1]
            Smin <- Bsvd$d[Bsz]
        }
        else {
            Smax <- max(Smax, Bsvd$d[1])
            Smin <- min(Smin, Bsvd$d[Bsz])
        }
        Smax <- max(eps23, Smax)
        if ((Smin/Smax < sqrteps) && reorth < 2) {
            warning("The matrix is ill-conditioned. Each basis will be reorthogonalized.")
            reorth <- 2
            aug <- "ritz"
        }
        if (sigma == "ss") {
            jj <- seq(ncol(Bsvd$u), 1, by = -1)
            Bsvd$u <- Bsvd$u[, jj]
            Bsvd$d <- Bsvd$d[jj]
            Bsvd$v <- Bsvd$v[, jj]
        }
        R <- R_F * Bsvd$u[Bsz, , drop = FALSE]
        ct <- convtests(Bsz, tol, k_org, Bsvd$u, Bsvd$d, Bsvd$v, 
            abs(R), k, SVTol, Smax)
        k <- ct$k
        if (ct$converged) 
            break
        if (iter >= maxit) 
            break
        if (aug == "harm") {
            Bsvd2.d <- Bsvd$d
            Bsvd2.d <- diag(Bsvd2.d, nrow = length(Bsvd2.d))
            Bsvd2 <- svd(cbind(Bsvd2.d, t(R)))
            if (sigma == "ss") {
                jj <- seq(ncol(Bsvd2$u), 1, by = -1)
                Bsvd2$u <- Bsvd2$u[, jj]
                Bsvd2$d <- Bsvd2$d[jj]
                Bsvd2$v <- Bsvd2$v[, jj]
            }
            Bsvd$d <- Bsvd2$d
            Bsvd$u <- Bsvd$u %*% Bsvd2$u
            Bsvd$v <- cbind(rbind(Bsvd$v, rep(0, Bsz)), c(rep(0, 
                Bsz), 1)) %*% Bsvd2$v
            V_B_last <- Bsvd$v[Bsz + 1, 1:k, drop = FALSE]
            s <- R_F * solve(B, cbind(c(rep(0, Bsz - 1), 1)))
            Bsvd$v <- Bsvd$v[1:Bsz, , drop = FALSE] + s %*% Bsvd$v[Bsz + 
                1, , drop = FALSE]
            qrv <- qr(cbind(rbind(Bsvd$v[, 1:k], 0), rbind(-s, 
                1)))
            Bsvd$v <- qr.Q(qrv)
            R <- qr.R(qrv)
            V[, 1:(k + 1)] <- cbind(V, F1) %*% Bsvd$v
            UT <- t(R[1:(k + 1), 1:k, drop = FALSE] + R[, k + 
                1, drop = FALSE] %*% V_B_last)
            B <- diag(Bsvd$d[1:k], nrow = k) %*% (UT * upper.tri(UT, 
                diag = TRUE))
        }
        else {
            V[, 1:(k + dim(F1)[2])] <- cbind(V[, 1:(dim(Bsvd$v)[1]), 
                drop = FALSE] %*% Bsvd$v[, 1:k, drop = FALSE], 
                F1)
            B <- cbind(diag(Bsvd$d[1:k], nrow = k), R[1:k, drop = FALSE])
        }
        W[, 1:k] <- W[, 1:(dim(Bsvd$u)[1]), drop = FALSE] %*% 
            Bsvd$u[, 1:k, drop = FALSE]
        iter <- iter + 1
    }
    d <- Bsvd$d[1:k_org]
    u <- W[, 1:(dim(Bsvd$u)[1]), drop = FALSE] %*% Bsvd$u[, 1:k_org, 
        drop = FALSE]
    v <- V[, 1:(dim(Bsvd$v)[1]), drop = FALSE] %*% Bsvd$v[, 1:k_org, 
        drop = FALSE]
    if (sigma == "ss") {
        reverse <- seq(length(d), 1)
        d <- d[reverse]
        u <- u[, reverse, drop = FALSE]
        v <- v[, reverse, drop = FALSE]
    }
    return(list(d = d, u = u[, 1:nu, drop = FALSE], v = v[, 1:nv, 
        drop = FALSE], iter = iter, mprod = mprod))
}
