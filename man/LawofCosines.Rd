\name{LawofCosines}
\alias{LawofCosines}
\title{
Law of Cosines
}
\description{
Use of the ancient law of cosines to determine the angle between two sides of
a triangle, given lengths of all three sides. 
This is a generalization of Pythagoras' Theorem. 
}
\usage{
LawofCosines(sides)
}
\arguments{
  \item{sides}{a numeric vector of length 3, containing the side lengths.}
}
\value{
  a numeric constant giving the angle in between the sides corresponding to the
first two components in \code{sides}. Result is expressed in degrees.  
}
\examples{
LawofCosines(c(3, 4, 5))
}
\keyword{math}
