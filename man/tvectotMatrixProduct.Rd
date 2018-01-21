\name{tvectotMatrixProduct}
\alias{tvectotMatrixProduct}
\title{
Tropical Vector Matrix Product
 }
\description{
 This function returns a vector in result of vector matrix tropical product.This function works only if length of vector equal to number of columns of matrix.
}
\usage{
tvectotMatrixProduct(Y,x)
}

\arguments{
\item{Y}{
A numeric matrix.
}
\item{x}{
A numeric vector.
}
}
\details{
 If the given argument \code{x} is not a numeric vector or argument \code{Y} is not a numeric matrix and the length of vector is not equal to the number of columns of matrix then the function generates an error.
 }
\value{
Returns the tropical product of vectror \code{x} and matrix \code{Y}.
}
\examples{
x<-c(1,2,3)
Y<-matrix(c(1,2,3,4,5,6,1,2,3), nrow = 3, ncol = 3)
tvectotMatrixProduct(Y,x)
}
