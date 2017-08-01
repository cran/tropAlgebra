\name{matrixAdd}
\alias{matrixAdd}

\title{
Tropical Additon of Matrices
}
\description{
 This funciton sums two matrices and return the resultant matrices.Keep in mind both matrices must have equal number of rows and columns.
 }
\usage{
matrixAdd(x,y)
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
  matrixAdd(X,Y)
  Then Tropical sum of Matrices will be...

      [1]  [2]
  [1]  2    3     Resultant Matrix
  [2]  1    9

}
\value{
returns the sum of \code{x} and \code{y}
}
\examples{
x<-matrix(c(2,3,5,7),ncol=2,nrow=2)
y<-matrix(c(6,3,1,9),ncol=2, nrow=2)
matrixAdd(x,y)
}
