\name{DecToBin}
\alias{DecToBin}
\title{Convert Decimal to Binary}
\description{
Convert a given decimal constant in the interval (0, 1) to the corresponding
binary representation.
}
\usage{
DecToBin(x, m = 32, format = "character") 
}
\arguments{
  \item{x}{
a numeric vector of values in the interval (0, 1)
}
\item{m}{a numeric constant specifying the number of binary digits to use in the output}
\item{format}{a character string constant specifying the form of the output}
}
\value{
a vector containing the binary representation
}
\details{
Default format is as a character string.  Alternatives are \code{plain} which prints
results to the device, and \code{vector} which output a binary vector.   
}

\examples{
x <- c(.81, .57, .333) 
DecToBin(x) # character output
DecToBin(x, format="vector") #  binary vector output
DecToBin(x, format="plain") 
}
\keyword{math}
