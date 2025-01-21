\name{rAlias}
\alias{rAlias}
\title{
Alias Method for Generating Discrete Random Variates
}
\description{
Efficient method for generating discrete random variates from any 
distribution with a finite number (N) of states.  The method
is described in detail in Section 10.1 of the given reference.    
}
\usage{
rAlias(n, P) 
}
\arguments{
  \item{n}{numeric, constant number of variates to be simulated}
  \item{P}{numeric, probability mass function, assuming states from 1 through N}
}
\value{
  numeric vector of containing n simulated values from the given discrete distribution
}
\references{
S. Ross (1990) A Course in Simulation, MacMillan.  
}
\examples{
  x <- rAlias(n = 100, P = c(1:5)/15)
  table(x)/100
}
\keyword{graphics}
