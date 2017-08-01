\name{vectorAdd}
\alias{vectorAdd}
\title{
Addition of vectors: Tropical Algebra Function.
}
\description{
This function add two vectors and returns the resultant vector Vectors length must be the equal.
}
\usage{
vectorAdd(x,y)
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
 Then the sum is:
 [1]  [2]  [3]
  1    2    3
}
\value{
Returns the tropical sum of \code{x} and \code{y}
}
\examples{
x<-c(5,6,7)
y<-c(1,2,3)
  vectorAdd(x,y)
}
