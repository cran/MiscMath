\name{IntDecToBin}
\alias{IntDecToBin}
\title{Binary Expansion of Positive Integers}
\description{
Convert positive integers to their corresponding
binary representation.
}
\usage{
IntDecToBin(x, m = 31) 
}
\arguments{
  \item{x}{
an integer vector 
}
\item{m}{a numeric constant specifying the number of binary digits to use in the output}
}
\value{
a matrix containing the binary representations
}

\examples{
x <- c(81, 57, 333) 
IntDecToBin(as.integer(x)) 
}
\keyword{math}
