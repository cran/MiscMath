rngV <- function(n, seed, par) {
    a <- par[1]; cc <- par[2]; m <- par[3]; L <- par[4]
    x <- numeric(n)
    ai <- numeric(L+1)
    ell <- 0:L
    for (i in ell) {
        ai[i+1] <- modpower(a, ell[i+1], m)
}
    ci <- (cc*cumsum(ai[-(L+1)]))%%m
    ai <- ai[-1]
    x[1:L] <- (ai*seed  + ci)%%m
    M <- ceiling(n/L)
    for (j in 1:(M-1)) {
        seed <- x[j*L]
        x[(j*L+1):((j+1)*L)] <- (ai*seed + ci)%%m
    }
    return(x[1:n]/m)
}

