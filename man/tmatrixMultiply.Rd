\name{tmatrixMultiply}
\alias{tmatrixMultiply}

\title{
Tropical Product of Matrices
}
\description{
   This function returns the product of two matrices. This function works only if columns of first matrix  eqaual to rows of seconnd matrixs.
   }
\usage{
tmatrixMultiply(X,Y)
}

\arguments{
  \item{X}{
  A numeric Matrix.
}
\item{Y}{
  A numeric Matrix.
}
}
\details{
  If number of columns of first matrix is not equal to the number of rows of second matrix or any one matrix is not a numeric matrix then this functions generates an error. If mxn is first matrix and axb is a second matrix then nxa will be the resultant matrix.
}
\value{
Returns the tropical product of \code{X} and \code{Y}
}
\examples{
X<-matrix(c(2,3,5,7),ncol=2,nrow=2)
Y<-matrix(c(6,3,1,9),ncol=2, nrow=2)
tmatrixMultiply(X,Y)
}
