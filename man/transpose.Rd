\name{transpose}
\alias{transpose}
\title{
Transpose of a Matrix
}
\description{
This fucntion interchage the rows into columns or columns into rows.
}
\usage{
transpose(X)
}
\arguments{
  \item{X}{
    A Matrix.
}
}
\details{
  If the given argument is not a matrix this function generates an error, otherwise, it tronsposed the matrix and returns in the same variable that was passed in argument like pass by reference. It changes values in memory.
 }
\value{
Returns transposed matrix \code{X}.
}
\examples{
X<-matrix(c(2,5,3,7),ncol=2,nrow=2)
transpose(X)

}
