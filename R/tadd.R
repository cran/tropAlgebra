#' Tropical Sum Function
#'
#'  Calculates tropical sum of  numeric values. This function workks only if argument(s) are numeric values.
#'  The tropical sum of two numbers is the minimum number. If argument(s) are not numeric values then it generates an error.
#'  @param ... any number of numeric values.
#'
#'  @return  Tropical sum of all the arguments.
#'
#'  @examples
#'   add(6,5)
#'
#' @export
tadd<-function(...){
  y<-Inf
  z<-cbind(list(...))
  n<-length(z)
  if(n==1)
  {
    tvectorAdd(z[[1]])
  }
  else{
  for (i in 1:length(z)) {
  if(is.numeric(z[[i]])){
  y<-if(y<z[[i]]) y else z[[i]]
    }else
    return(stop("arguments must be numeric values"))
  }
  return(y)
  }
}
