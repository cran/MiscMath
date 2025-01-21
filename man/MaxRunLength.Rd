\name{MaxRunLength}
\alias{MaxRunLength}
\title{Maximum Run Length}
\description{
Calculate the maximum run length of of 0's in a binary vector.
}
\usage{
MaxRunLength(x) 
}
\arguments{
  \item{x}{a binary vector
}
}
\value{
the maximum run length of 0's
}

\examples{
x <- c(0L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L)
MaxRunLength(x) # should be 2
MaxRunLength(1L - x) # should be 3
# Test of Mersenne Twishter

RNGkind("Mers") # ensure that default generator is in use
N <- 10000
x <- runif(N)
y <- DecToBin(x, format = "vector", m = 40)
MaxHeadRunsM <- apply(y, 1, MaxRunLength) # 0 run lengths (Heads)
MaxTailRunsM <- apply(1-y, 1, MaxRunLength) # 1 run lengths (Tails)
# distributions of Max 0 run lengths and Max 1 run lengths should match
boxplot(MaxHeadRunsM, MaxTailRunsM, axes=FALSE, main="Maximum Run Length")
axis(side=1, at=c(1, 2), label=c("Heads", "Tails"))
axis(2)
box()

# Comparison with Wichmann-Hill Generator

RNGkind("Wich")
x <- runif(N)
y <- DecToBin(x, format = "vector", m = 40)
MaxHeadRunsW <- apply(y, 1, MaxRunLength)
MaxTailRunsW <- apply(1-y, 1, MaxRunLength)
boxplot(MaxHeadRunsW, MaxTailRunsW, axes=FALSE, main="Maximum Run Length")
axis(side=1, at=c(1, 2), label=c("Heads", "Tails"))
axis(2)
box()
RNGkind("Mers") # restore default generator
}
\keyword{math}
