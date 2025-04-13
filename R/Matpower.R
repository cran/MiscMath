Matpower <- function(X, p) {
    # pth power of matrix X
    # Golub and Van Loan (1983) Matrix Computations. Algorithm 11.2-1. p. 393.
    pBinExp <- IntDecToBin(as.integer(p))
    T <- max(which(pBinExp>0)) - 1
    W <- X
    Y <- X
    if (pBinExp[1] == 0L) Y <- diag(rep(1,nrow(X)))
    if (T > 0) {
    for (k in 1:T) {
        W <- W%*%W
        if (pBinExp[k+1] == 1L) Y <- Y%*%W
    }
    }
    return(Y)
}
