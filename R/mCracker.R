mCracker <- function(U, par = 1e6, maxit = 100) {
  # function to infer the modulus m for a congruential random number generator 
  # U = pseudorandom uniform numbers on (0, 1) of the form X/m
  # par = initial upper bound on minimum integer X(1) internally calculated in the RNG
  MinU <- min(U)
  N <- 1:par
  U1OverMinU <- U[1]/MinU
  Y <- N*U1OverMinU
  nonInteger <- TRUE
  X1 <- NULL; iter <- 0
  while (nonInteger & iter < maxit) {
      iter <- iter + 1
      if (length(X1) > 0) {
          X1 <- c(X1, which.min(Y[-X1] - floor(Y[-X1])) + length(X1)) # value of first integer X in input sequence
      } else {
          X1 <- c(X1, which.min(Y - floor(Y)))  # value of first integer X in input sequence
      }
      m <- max(X1)/MinU
      Xtrial <- m*U
      nonInteger <- any(Xtrial - floor(Xtrial) > .1) # nonInteger detected
  }
  list(m = m, firstInteger = max(X1))
}

