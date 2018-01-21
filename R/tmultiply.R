#'   Tropical Product Function.
#'   Calculates the product of two numbers. This function works only if both the arguments are numeric values.
#'   The tropical product of two number is the sum of these numbers. If \code{x} or \code{y} are not numeric values, this function returns 'NULL'.
#'  @param  ... any number of numeric values or a single numeric vector.
#'
#'  @return Tropical product of arguments.
#'
#'  @examples
#'  tmultiply(6,5)
#'
#'@export
tmultiply<-function(...){
  y<-0
  z<-cbind(list(...))
  n<-length(z)
  if(n==1)
  {
    tvectorMultiply(z[[1]])
  }
  else{
    for (i in 1:length(z)) {
      if(is.numeric(z[[i]])){
        y<-y+z[[i]]
      }else
        return(stop("arguments must be numeric values"))
    }
    return(y)
  }
}
