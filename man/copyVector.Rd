\name{copyVector}
\alias{copyVector}

\title{
Copy Vector
}
\description{
This function copies the 1st vector's values in the 2nd vector, Vectors must have equal length..}
\usage{
copyVector(x,y)
}

\arguments{
  \item{x}{
  A vector to be copy
}
\item{y}{
  A vector in which vector  is to be copy.
}
}
\details{
e.g.
  x is a vector
  x<-c(1,2,3,4)
  and y is an empty vector
  y<-c(NA,NA,NA,NA)
  we want copy vector 'x' into 'y'. This fuction do this
  copyVector(x,y)
  now values of 'y' vector will be same as of 'x' vector

}
\value{
returns \code{y} copied from \code{x}
}
\examples{
  x<-c(1,2,3,4)
  y<-c(NA,NA,NA,NA)
   copyVector(x,y)
}
