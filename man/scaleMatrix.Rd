\name{scaleMatrix}
\alias{scaleMatrix}
\title{
 Scaling of a Matrix in Tropical Algebra
 }
\description{
This function returns the scaled matrix 'Y' by a value X .
}
\usage{
scaleMatrix(x,y)
}

\arguments{
  \item{x}{
  A numeric value
}
\item{y}{
  A numeric matrix
}
}
\details{
 e.g.
      [1]  [2]
  [1]  2    3      matrix  Y
  [2]  5    7

  x<-5
  Now function call to scale matrix Y by 5
      [1]  [2]
  [1]  7    8      Resultant matrix
  [2]  10   12
}
\value{
Returns the scaled matrix \code{y} by \code{x}
}
\examples{
x<-5
y<-matrix(c(2,3,5,7),ncol=2,nrow=2)
 scaleMatrix(x,y)
}
