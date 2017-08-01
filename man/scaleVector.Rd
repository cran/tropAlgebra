\name{scaleVector}
\alias{scaleVector}
\title{
  Scalign of a Vector in Tropical Algebra
}
\description{
This function returns the scaled vector Y by a value X.
}
\usage{
scaleVector(x,y)
}

\arguments{
  \item{x}{
  A numeric value
}
  \item{y}{
  A numeric vector
}
}
\details{
e.g.
 Y<-c(1,2,3)
 x<-5
 Now calling fucntiion:
 scaleVector(x,y)
 it returns
 [1]  [2]  [3]
  6    7    8

}
\value{
Returns the scaled vector \code{y} by \code{x}
}
\examples{
x<-5
y<-c(1,2,3)
    scaleVector(x,y)
}
