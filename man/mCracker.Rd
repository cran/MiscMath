\name{mCracker}
\alias{mCracker}
\title{Modulus Cracker}
\description{
Infers the modulus m for a congruential random number generator. 
}
\usage{
mCracker(U, par = 1e6, maxit = 100) 
}
\arguments{
  \item{U}{
a numeric vector consisting of n (say 10000) uniform(0,1) pseudorandom numbers of
the form x1/m, x2/m, ..., xn/m.
}
  \item{par}{
an integer guess as to an upper bound on the smallest integer in
the sequence x1, x2, ..., xn.
}
  \item{maxit}{
maximum number of iterations allowed.
}
}
\details{
Basic idea:  Let x(1) denote the minimum order statistic in x1, x2, ..., xn.
Then the set (x1/m)/(x(1)/m)*(1:par) must contain at least one integer, and
m is in that set, if par has been set correctly.      
}
\value{
a list consisting of 
\item{m}{the integer value of m}
\item{firstInteger}{the minimum order statistic of the set x1, x2, ..., xm}
}

\examples{
# set.seed(33663)
x <- runif(1000000)
Y <- mCracker(x)$m
log(Y, 2)  # should be 32
}

\keyword{math}
