\name{EratosthenesSieve}
\alias{EratosthenesSieve}
\title{Sieve of Eratosthenes}
\description{
The sieve of Eratosthenes is an ancient method for listing all
prime numbers up to a given value \code{n}.  
}
\usage{
EratosthenesSieve(n)
}
\arguments{
  \item{n}{
a numeric vector consisting of a single positive integer.
}
}
\details{
The algorithm scans through the vector from 2 through n, eliminating
all multiples of 2, then eliminating all multiples of the next smallest
integer (3), and so on, until only the prime numbers less than n remain.
}
\value{
a numeric vector containing all primes less than n.
}

\examples{
EratosthenesSieve(100)
}

\keyword{math}
