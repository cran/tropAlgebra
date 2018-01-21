\name{tadd}
\alias{tadd}
\title{
Tropical Sum Function
}
\description{
 Calculates tropical sum of  numeric values. This function workks only if arguments are numeric values. The tropical sum of two numbers is the minimum number. If arguments are not numeric values then it generates an error. If a vector is passed as argument to the funcion it will return the tropical sum of all the elements in the vector.
}
\usage{
tadd(...)
}
\arguments{
  \item{...}{
  Any number of numeric values or a single vector.
}
}
\details{
  The tropical sum of two numbers is the minimum number. This function returns the minimum of arguments. If arguments are not numeric values then it generates an error.
}
\value{
 Returns  tropical sum of arguments.
}
\examples{
x<-5
y<-6
tadd(x,y)
}

