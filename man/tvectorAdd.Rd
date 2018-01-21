\name{tvectorAdd}
\alias{tvectorAdd}
\title{
 Tropical Sum of Vectors
}
\description{
   This function add two or more vectors and returns the resultant vector. This function works only if vectors are of same length.
}
\usage{
tvectorAdd(...)
}

\arguments{
  \item{...}{
  Any number of numeric vectors.
}
}
\details{
If the arguments are not numeric vectors and are not of same length, this function generates an error. If a single vector is passed to the function it will return the tropical sum of all the elements in the vector.
 }
\value{
 Returns tropical sum of argumented vectors.
}
\examples{
x<-c(5,6,7)
y<-c(1,2,3)
tvectorAdd(x,y)
}
