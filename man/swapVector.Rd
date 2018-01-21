\name{swapVector}
\alias{swapVector}

\title{
 Swap  Vectors
 }
\description{
 This function swaps the values of both vectors. This function works only if both vectors have equal length.
 }
\usage{
swapVector(x,y)
}

\arguments{
  \item{x}{
 A vector.
}
\item{y}{
 A vector.
}
}
\details{
If the lengths of both vectors are not same, it generates an error. This function swaps the vectors in memory(like pass by reference), it does not return the vectors, but interchanges their values.
}
\value{
Swapped vectors \code{x} and \code{y}.
}
\examples{
x<-c(6,7,8)
y<-c(3,2,1)
swapVector(x,y)
}
