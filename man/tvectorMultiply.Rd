\name{tvectorMultiply}
\alias{tvectorMultiply}

\title{
   Tropical Product of Vectors
}
\description{
  This function multiplies two or more vectors and returns the resultant vector. This function works only if vectors are of same length.
}
\usage{
tvectorMultiply(...)
}
\arguments{
  \item{...}{
  Any number of numeric vectors.
}
}
\details{
If the arguments are not numeric vectors and are not of same length, this function generates an error. If a single vector is passed to the function it will return the tropical product of all the elements in the vector.
}
\value{
Returns the product of argumented vectors.
}
\examples{
x<-c(5,6,7)
y<-c(1,2,3)
tvectorMultiply(x,y)
}
