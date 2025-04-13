\name{rngV}
\alias{rngV}
\title{Vectorized Congruential Random Number Generator}
\description{
Basic vectorized implementation of the linear congruential generator for 
simulating uniform random numbers on the interval (0, 1).  
}
\usage{
rngV(n, seed, par) 
}
\arguments{
  \item{n}{
numeric: number of variates to generate.
}
  \item{seed}{
numeric: initial seed.
}
  \item{par}{
an integer vector containing parameters for the generator: a, cc, m, L.
}
}
\value{
a vector of n uniform random numbers
}

\references{Anderson, S.L. (1990) Random number generators on 
vector supercomputers and other advanced architectures.  SIAM Review,
32(2):221-251.} 

\examples{
x <- rngV(1000, 6976, c(171, 0, 30269, 10))
summary(x)
}

\keyword{math}




