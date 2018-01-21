\name{tmultiply}
\alias{tmultiply}

\title{
    Tropical Product Function
}
\description{
The tropical product of two numbers is the odinary sum of numbers. This function returns the tropical product of arguments. If arguments are not numeric values then it generates an error. If a vector is passed as argument to the funcion it will return the tropical product of all the elements in the vector.
}
\usage{
tmultiply(...)
}

\arguments{
  \item{...}{
  Any number of numeric values or a single vector.
}
}
\details{
 The tropical product of two numbers is the oridnary sum of numers. This function returns the tropical product of arguments. If arguments are not numeric values then it generates an error.
}
\value{
  Returns tropical product of arguments.
}
\examples{
 x<-6
 y<-5
 tmultiply(x,y)
}
