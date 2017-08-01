\name{dotProduct}
\alias{dotProduct}
\title{
Dot Product of X and Y: Tropical Algebra Function
}
\description{
 Dot product is actually the multiplication of transpose of 1st matrix then multiplicatoin of that transposed matrix and 2nd matrix(Y). This function does this and returns the resultant matrix after calculation of thes steps, Colunms of 1st matrix must be eqaual to rows of 2nd one..
}
\usage{
dotProduct(x,y)
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
  .........................................
      [1]  [2]
  [1]  6    3     2nd matrix that is Y
  [2]  1    9
 ..........................................

  Now, calling function...
  dotProduct(X,Y)
 Transpose of matrix 'X is obtained by interchanging rows into coulums OR columns into rows.
      [1]  [2]
  [1]  2     5     Trransposed matrix
  [2]  3     7
..............................................
   Then Tropical Dot Product of Matrices will be...

      [1]  [2]
  [1]  6    10     Resultant Matrix
  [2]  3    6
}
\value{
Returns the dot product of \code{x} and \code{y}
}
\examples{
x<-matrix(c(2,5,3,7),ncol=2,nrow=2)
y<-matrix(c(6,10,3,6),ncol=2, nrow=2)
dotProduct(x,y)

}
