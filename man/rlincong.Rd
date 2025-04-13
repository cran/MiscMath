\name{rlincong}
\alias{rlincong}
\title{Linear Congruential Generator}
\description{
Basic implementation of the linear congruential random number generator.  
}
\usage{
rlincong(n, seed, par) 
}
\arguments{
  \item{n}{
numeric: number of variates to generate.
}
  \item{seed}{
numeric: initial seed.
}
  \item{par}{
an integer vector containing parameters for the generator: a, cc, m.
}
}
\value{
a vector of n uniform random numbers
}

\examples{
x <- rlincong(1000, 6976, c(171, 0, 30269))
summary(x)
}

\keyword{math}




