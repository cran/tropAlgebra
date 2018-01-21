\name{copyVector}
\alias{copyVector}

\title{
Copy Vectors
}
\description{
This function copies the first vector in second one. This fucntion will work only if both vectors are of same length.
}
\usage{
copyVector(x,y)
}

\arguments{
  \item{x}{
  Vector to be copied.
}
\item{y}{
  Target vector.
}
}
\details{
If the length of source and target vectors are not same it generates an error.
}
\value{
Returns \code{y}.
}
\examples{
  x<-c(1,2,3,4)
  y<-c(NA,NA,NA,NA)
  copyVector(x,y)
}
