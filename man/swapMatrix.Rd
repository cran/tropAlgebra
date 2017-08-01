\name{swapMatrix}
\alias{swapMatrix}

\title{
  Swap Matrices
  }
\description{
This function inchange the values of both matrices Both Matrix must have equal number of rows and columns.
}
\usage{
swapMatrix(x,y)
}
\arguments{
  \item{x}{
 A matrix
}
\item{y}{
 A matrix
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
  swapMatrix(X,Y)
  Then the swapped  Matrices will be...
      [1]  [2]
  [1]  6    3     1st matrix that is X
  [2]  1    9

  and

      [1]  [2]
  [1]  2    3     2nd matrix that is Y
  [2]  5    7

}
\value{
Returns the swapped Matrix \code{x} and \code{y}
}
\examples{
x<-matrix(c(2,3,5,7),ncol=2,nrow=2)
y<-matrix(c(6,3,1,9),ncol=2, nrow=2)
   swapMatrix(x,y)

}
