microLASSO <- function(x, y, lambda) {
    n <- length(x)
    C <- var(y)*(n-1)
    denom <- sum(x*(x-mean(x)))
    numer <- sum(x*(y-mean(y)))
    bplus <- (numer - lambda/2)/denom
    if (bplus > 0) {
        b <- bplus
    } else {
        b <- (numer + lambda/2)/denom
    }
    Cb <- sum((y - mean(y) - b*(x - mean(x)))^2) + lambda*abs(b)
    b <- (Cb < C)*b
    return(list(Coefficients = c("(Intercept)" = mean(y) - b*mean(x), "x" = b), 
    RMSE = c(sqrt((Cb - 3*lambda*abs(b))/n))))
} 
