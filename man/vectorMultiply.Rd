\name{vectorMultiply}
\alias{vectorMultiply}

\title{
Product: Tropical Algebra Function
}
\description{
This functio returns the product of two vectors Vectors length must be the equal.
}
\usage{
vectorMultiply(x,y)
}

\arguments{
  \item{x}{
 A numeric vector
}
  \item{y}{
 A numeric vector
}
}
\details{
e.g.
  X<-c(5,6,7)
  Y<-c(1,2,3)
  Now function call:
  vectorAdd(X,Y)
 Then the Product is:
 [1]  [2]  [3]
  6    8    10
}
\value{
Returns the product of \code{x} and \code{y} vectors.
}
\examples{
 x<-c(5,6,7)
y<-c(1,2,3)
 vectorMultiply(x,y)
}
