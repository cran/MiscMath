IntDecToBin <- function(x, m = 31) {
  # Find binary expansion of positive integer x
     if (!is.integer(x) | any(x < 0)) warning("Nonnegative integers required.  Results may be unreliable.\n")
     n <- length(x)
     b <- matrix(0, nrow = n, ncol = m)
     for (i in 1:m) {
         b[, i] <- (x%%2L)!=0L   
         x <- x%/%2L
     }
     return(b)
}

