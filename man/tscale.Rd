\name{tscale}
\alias{tscale}
\title{
 Tropical Scaling of Matrix Or vector
 }
\description{
 This function returns the scaled matrix or vector \code{Y} by a value \code{x} .This function works only if \code{Y} is matrix or vector and \code{x} is a numeric value.
}
\usage{
tscale(x,y)
}

\arguments{
  \item{x}{
  A numeric value.
}
\item{y}{
  A numeric matrix or a numeric vector.
}
}
\details{
If the given argument \code{x} is not a numeric value or argument \code{Y} is not a numeric matrix or a numeric vector then this function generates an error.
}
\value{
Returns the scaled \code{Y}.
}
\examples{

x<-5
y<-matrix(c(2,3,5,7),ncol=2,nrow=2)
tscale(x,y)
y<-c(1,2,3)
tscale(x,y)
}
