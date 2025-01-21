\name{LawofSines}
\alias{LawofSines}
\title{
Law of Sines
}
\description{
Use of the ancient law of sines to determine the angle opposite one side of
a triangle, given the length of the opposite side as well as the angle opposite 
another side whose length is also known.  Alternatively, one can find the length
of the side opposite a given angle.  
}
\usage{
LawofSines(sides, angles, findAngle)
}
\arguments{
  \item{sides}{a numeric vector of length 1 or 2, containing the side lengths.}
  \item{angles}{a numeric vector of length 1 or 2, containing the angles (in degrees).}
  \item{findAngle}{a logical constant}
}
\value{
  a numeric constant giving the angle opposite the given side, if \code{findAngle} is
\code{TRUE}, or giving the length of the side opposite the given angle, if \code{findAngle} is
\code{FALSE}.  
}
\details{
If \code{findAngle} is \code{TRUE}, \code{sides} should be of length 2 and the 
function will compute angle opposite the side with length given by the second element of
\code{sides}.  Otherwise, \code{angles} should be of length 2, and the function will
calculate the length of the side opposite the angle corresponding to the second
element of \code{angles}.  
} 
\examples{
LawofSines(c(3, 4), 50)  # find angle opposite the side of length 4.
LawofSines(3, c(70, 50), findAngle = FALSE) # find length of side opposite the 50 degree angle
}
\keyword{math}
