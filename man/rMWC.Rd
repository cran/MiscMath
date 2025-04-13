\name{rMWC}
\alias{rMWC}
\title{Multiply-With-Carry Random Number Generator}
\description{
Basic implementation of the multiply-with-carry generator.  
}
\usage{
rMWC(n, par) 
}
\arguments{
  \item{n}{
numeric: number of variates to generate.
}
  \item{par}{
an integer vector containing parameters for the generator: X, C, A, B.
}
}
\value{
a vector of n uniform random numbers
}

\references{Marsaglia, G. (2003) Random Number Generators. Journal
of Modern Applied Statistical Methods. 2(1):2.}

\examples{
x <- rMWC(58, c(5, 3, 6, 10))
summary(x)
}

\keyword{math}




