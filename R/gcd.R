gcd <- function(x) {
    n <- length(x)
    if (n < 2) { 
        g <- x   
        warning("Input vector should normally have length greater than 1.")
    } else {
        ints <- x
        g <- ints[1]
        z <- .Fortran("gcd", as.integer(n), as.integer(ints), as.integer(g))
        g <- z[[3]]
    }
    return(g)
}
