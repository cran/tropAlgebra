\name{copyMatrix}
\alias{copyMatrix}
\title{
Copy Matrices
}
\description{
 This function copies the first matrix in second one. Thhis fucntion will work only if both matrices are of same size.
}
\usage{
copyMatrix(X,Y)
}

\arguments{
  \item{X}{
    A matrix to be copied.
}
\item{Y}{
    Target Matrix.
}
}
\details{
  If the size of source and target matrices are not same it generates an error.
}
\value{
Returns \code{Y} Matrix.
}
\examples{
X<-matrix(c('a','b','c','d'),nrow=2, ncol=2)
Y<-matrix(c(NA,NA,NA,NA),nrow=2, ncol=2)
copyMatrix(X,Y)
}

