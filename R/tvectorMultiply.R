#'
#'
#'   Tropical   Product of Vectors
#'   This functio returns the product of two vectors. This function works only if both Vectors are of same length.
#'  @param x  A vector.
#'  @param y A vector.
#'
#'  @return Product of \code{x} and \code{y} in tropical algebra.
#'
#'
#'  @examples
#'  x<-c(5,6,7)
#'  y<-c(1,2,3)
#'  tvectorMultiply(x,y)
#'  @export
#'
tvectorMultiply<-function(...){
  z<-cbind(list(...))
  n<-length(z)
  if(n==1){
    if(is.vector(z[[1]])&&is.numeric(z[[1]])){
      y<-0
      for (i in 1:length(z[[1]])) {
        y<-z[[1]]+y
      }
      return(y)
    }else{
      return(stop("argument must be a numeric vector"))
    }
  }else{
    m<-length(z[[n]])
    lname<-vector()
    for (i in 1:n) {
      if(is.vector(z[[i]])&&is.numeric(z[[i]])&&length(z[[i]])==m){
        assign(paste("var",i, sep = ""),z[[i]])
        lname[i]<-paste("var",i, sep = "")
      }else{
        stop("arguments must be numeric vectors of equal lengths")
      }
    }
    result<-vector(,length = m)
    for (i in 1:m) {
      result[i]<-0
    }
    for (i in 1:n) {

      for (j in 1:m) {
        result[j]<-tmultiply(result[j],eval(parse(text=paste("var",i,"[",j,"]", sep = ""))))
      }
    }
    return(result)
  }
}
#'
#'
