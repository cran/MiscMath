\name{shuffle}
\alias{shuffle}
\title{Shuffling Algorithm}
\description{
Implementation of a simple shuffling algorithm that can be used
to randomly permute a given set of simulated random numbers.  
}
\usage{
shuffle(n, k = 100, x = runif(n))
}
\arguments{
  \item{n}{
numeric: number of variates to be output.
}
  \item{k}{
numeric: a tuning parameter for the shuffler.
}
  \item{x}{
a vector containing a sequence to be randomly permuted with the shuffler.
}
}
\value{
a numeric vector
}

\keyword{math}




