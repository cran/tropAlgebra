\name{swapVector}
\alias{swapVector}

\title{
 Swap  Vectors
 }
\description{
 This function swaps the values of both vectors, Both Vectors must have equal length.
 }
\usage{
swapVector(x,y)
}

\arguments{
  \item{x}{
 A vector
}
\item{y}{
 A vector
}
}
\details{
e.g.
  [1]  [2]  [3]
  6    7    8     vector X
  ...................
  [1]  [2]  [3]
   3    2    1     vector Y
  ...................
  Now function call:
  swapVector(x,y)
  Then swapped vectors are :
  [1]  [2]  [3]
  6    7    8     vector Y
  ...................
  [1]  [2]  [3]
   3    2    1     vector X
}
\value{
Returns the swapped vectors \code{x} and \code{y}
}
\examples{
x<-c(6,7,8)
y<-c(3,2,1)
    swapVector(x,y)
}
