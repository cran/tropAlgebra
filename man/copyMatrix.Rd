\name{copyMatrix}
\alias{copyMatrix}
\title{
Copy Matrices
}
\description{
 This function copies the 1st matrix in second one if both the matirces are of equal number of rows and columns.
}
\usage{
copyMatrix(x,y)
}

\arguments{
  \item{x}{
    A matrix to be copy
}
\item{y}{
    A matrix in which a matrix is to copy
}
}
\details{
e.g.
  x is a matrix.
  x<-matrix(c(1,2,3,4), nrow=2, ncol=2)
  and y is an empty matrix
  y<-matrix(data=NA,  nrow=2, ncol=2)
  we want copy matirx 'x' into 'y'. This fuction do this.
  copyMatrix(x,y)
  now values of 'y' matirx will be same as of 'x' matrix.

}
\value{
returns \code{y} copied from \code{x}
}
\examples{
    x<-matrix(c(1,2,3,4),nrow=2, ncol=2)
    y<-matrix(c(NA,NA,NA,NA),nrow=2, ncol=2)
    copyMatrix(x,y)
}

