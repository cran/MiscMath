rMWC <- function(n, par) {
    X <- par[1]; C <- par[2]; A <- par[3]; B <- par[4]
    f <- function(X, C, A, B) {
        c(floor((A*X + C)/B), (A*X + C)%%B)
    }
    U <- numeric(n)
    out <- f(X, C, A, B)
        for (i in 1:n) {
            C <- out[1]; X <- out[2]
            U[i] <- (C*B + X)/(A*B)
            if (i < n) out <- f(X, C, A, B)
    }    
    return(U)
}
