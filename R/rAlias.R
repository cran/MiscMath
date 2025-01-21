rAlias <- function(n, P) {
    Setup <- function(P) {
        N <- length(P[abs(P) > 1e-15])
        Q <- numeric(length(P))
        I <- min(which(abs(P) > 1e-15 & P < (1/(N - 1))))
        indices <- which((P[I] + P[-I] + 1e-16) >= (1/(N-1)))
        indices[indices >= I] <- indices[indices >= I] + 1
        J <- max(indices) 
        Q[I] <- (N - 1)*P[I]
        Q[J] <- 1 - Q[I]
        P <- (P - Q/(N-1))*(N-1)/(N-2)
        list(P = P, Q = Q, I = I, J = J)
    }
    N <- length(P)
    Q <- matrix(0, nrow = 2, ncol = (N - 1))
    J <- I <- integer(N - 1)
    for (j in 1:(N-1)) {
        out <- Setup(P)
        Q[, j] <- out$Q[c(out$I, out$J)]
        I[j] <- out$I
        J[j] <- out$J
        P <- out$P
    }
    U1 <- runif(n)
    U2 <- runif(n)
    M <- 1 + floor((N-1)*U1)
    X <- I[M]*(Q[1, M] > U2) + J[M]*(Q[1, M] <= U2)
    X 
}

