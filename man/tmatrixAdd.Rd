\name{tmatrixAdd}
\alias{tmatrixAdd}

\title{
Tropical Sum of Matrices
}
\description{
   This funciton sums two matrices(first matrix scaled by \code{alpha}, if user doesnt't pass \code{alpha} first matrix will not be scalled) and return the resultant matrix.This function works only if both matrices are numeric and of same size, and alpha is a numeric value.
 }
\usage{
tmatrixAdd(x,y,alpha=0)
}
\arguments{
  \item{x}{
 A numeric Matrix.
}
\item{y}{
 A numeric Matrix.
}
\item{alpha}{
    A numeric value.
  }
}
\details{
  If either the size of both matrices is not same or if any one or both matrices are not numeric matrix or alpha is not a numeric value then it will generates an error.
}
\value{
Returns the tropical sum of \code{x}(scalled by \code{alpha}) and \code{y}.
}
\examples{
x<-matrix(c(2,3,5,7),ncol=2,nrow=2)
y<-matrix(c(6,3,1,9),ncol=2, nrow=2)
alpha<-5
tmatrixAdd(x,y,alpha)
}
