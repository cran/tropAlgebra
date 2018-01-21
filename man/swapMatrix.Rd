\name{swapMatrix}
\alias{swapMatrix}

\title{
  Swap Matrices
  }
\description{
This function interchanges the values of both matrices.This function works only if both matrices are of same size.
}
\usage{
swapMatrix(X,Y)
}
\arguments{
  \item{X}{
 A matrix.
}
\item{Y}{
 A matrix.
}
}
\details{
If the size of both matrices are not same, it generates an error. This function swaps the matrices in memory(like pass by reference), it does not return the matrices, but interchanges their values.
}
\value{
Swapped Matrix \code{X} and \code{Y}.
}
\examples{
x<-matrix(c(2,3,5,7),ncol=2,nrow=2)
y<-matrix(c(6,3,1,9),ncol=2, nrow=2)
swapMatrix(x,y)

}
