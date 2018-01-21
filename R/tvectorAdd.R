#'    Tropical Sum of Vectors
#'   This function add two vectors and returns the resultant vector. This function works only if both vectors are of same length.
#'  if the both \code{x} and \code{y} are not numeric vector and are not of same length, this function returns 'NULL'.
#'  @param x is a vector.
#'  @param  y is a vector.
#'  @return sum of X and Y in tropical algebra.
#'
#'
#'  @examples
#' x<-c(5,6,7)
#' y<-c(1,2,3)
#' tvectorAdd(x,y)
#' @export
tvectorAdd<-function(...){
  z<-cbind(list(...))
  n<-length(z)
  if(n==1){
    if(is.vector(z[[1]])&&is.numeric(z[[1]])){
      y<-Inf
      for (i in 1:length(z[[i]])) {
        y<-if(z[[1]]<y) z[[1]] else y
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
  result<-vector(mode = "numeric",length = m)
  for (i in 1:m) {
    result[i]<-Inf
  }
  for (i in 1:n) {

  for (j in 1:m) {
    result[j]<-tadd(result[j],eval(parse(text=paste("var",i,"[",j,"]", sep = ""))))
  }
}
  return(result)
  }
}
#'
#'
