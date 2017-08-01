\name{vectotMatrixProduct}
\alias{vectotMatrixProduct}
\title{
 Vector Matrix Product
 }
\description{
 This function returns a vector in result of vector matrix product.
}
\usage{
vectotMatrixProduct(x,y)
}

\arguments{
  \item{x}{
A numeric vector
}
  \item{y}{
A numeric Matrix
}
}
\details{
e.g
 a vector x
 x<-c(1,2,3)
 [1] 1 2 3
 a matrix y
 y<-matrix(c(1,2,3,4,5,6,1,2,3), nrow = 3, ncol = 3)
      [,1] [,2] [,3]
 [1,]    1    4    1
 [2,]    2    5    2
 [3,]    3    6    3
 Now function call
 vectotMatrixProduct(x,y)
 Then the resultant vector is returned
 [1] 2 6 4
}
\value{
Returns the product of vectror \code{x} and Matrix \code{y}
}
\examples{
x<-c(1,2,3)
y<-matrix(c(1,2,3,4,5,6,1,2,3), nrow = 3, ncol = 3)
    vectotMatrixProduct(x,y)
}
