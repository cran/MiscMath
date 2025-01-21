\name{microLASSO}
\alias{microLASSO}
\title{
Simplest Case of LASSO Regression
}
\description{
Simple linear regression estimators for slope, intercept and noise standard deviation
with absolute value penalty on slope.
}
\usage{
microLASSO(x, y, lambda)
}
\arguments{
  \item{x}{a numeric vector of covariate values}
  \item{y}{a numeric vector of response values}
  \item{lambda}{a numeric constant which should be nonnegative}
}
\value{a list consisting of
\item{Coefficients}{a numeric vector containing intercept and slope estimates}
\item{RMSE}{a numeric constant containing the (penalized) maximum likelihood estimate of the 
noise standard deviation}
} 
\examples{
x <- runif(30)
y <- x + rnorm(30)
microLASSO(x, y, lambda = 0.5)
}
\keyword{models}
