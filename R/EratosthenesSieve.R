EratosthenesSieve <- function(n) {
   # Print all prime numbers up to n (based on the sieve of Eratosthenes)
    if (n >= 2) {
        noMultiples <- function(j) sieve[(sieve %% j) != 0]
        sieve <- seq(2, n)
        primes <- c()
        for (i in seq(2, n)) {
            if (any(sieve == i)) {
                primes <- c(primes, i)
                sieve <- c(noMultiples(i), i)
            }
        }
        return(primes)
    } else {
        stop("Input value of n should be at least 2.")
    }
}

