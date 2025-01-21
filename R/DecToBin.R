DecToBin <- function(x, m = 32, format = "character") {
     if (any(x >= 1|x <= 0)) warning("Not all entries are in (0,1).  Results may be unreliable.\n")
     n <- length(x)
     b <- matrix(0, nrow = n, ncol = m)
     for (i in 1:m) {
         x2 <- x*2
         b[, i] <- pmax(x2, 1) > 1
         x <- x2 - floor(x2)
     }
     if (format == "character") {
         tmp <- character(n)
         for (j in 1:n) {
             tmp[j] <- paste(".", paste(as.character(b[j, ]), collapse=""), sep="")
         } 
         b <- tmp
     } 
     return(b)
     invisible()
}

