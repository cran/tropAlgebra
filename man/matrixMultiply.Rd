\name{matrixMultiply}
\alias{matrixMultiply}

\title{
Tropical Product of Matrices
}
\description{
   This function returns the product of two matrices. Keep in mind that Rows of 1st matrix must be eqaual to columns of 2nd one.
   }
\usage{
matrixMultiply(x,y)
}

\arguments{
  \item{x}{
  A numeric Matrix
}
\item{y}{
  A numeric Matrix
}
}
\details{
 e.g.
      [1]  [2]
  [1]  2    3     1st matrix that is X
  [2]  5    7
      [1]  [2]
  [1]  6    3     2nd matrix that is Y
  [2]  1    9
  Now, calling function...
  matrixMultiply(X,Y)
  Then Tropical sum of Matrices will be...

      [1]  [2]
  [1]  3    5     Resultant Matrix
  [2]  8    8

}
\value{
Returns the tropical product of \code{x} and \code{y}
}
\examples{
 x<-matrix(c(2,3,5,7),ncol=2,nrow=2)
y<-matrix(c(6,3,1,9),ncol=2, nrow=2)
  matrixMultiply(x,y)
}
