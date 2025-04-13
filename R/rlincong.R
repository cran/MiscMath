rlincong <- function(n, seed, par = c(2^8+1, 3, 2^16)) {
    x <- numeric(n+1); a <- par[1]; cc <- par[2]; m <- par[3]
    x[1] <- seed
    for (i in 2:(n+1)){
        x[i] <- (a*x[i-1] + cc)%%m
    }
    U <- x[2:(n+1)]/m 
    return(U)
}
