\name{gcd}
\alias{gcd}
\title{Greatest Common Divisor}
\description{
Greatest common divisor or factor for all elements of a positive-integer-valued vector.
}
\usage{
gcd(x)
}
\arguments{
  \item{x}{
a numeric vector consisting of at least two positive integer values.
}
}
\details{
  The gcd is calculated using the Euclidean algorithm applied to successive pairs of the
elements of x.
}
\value{
a numeric constant containing the greatest common divisor.
}

\examples{
x <- c(81, 57, 333)
gcd(x)
}

\keyword{math}
