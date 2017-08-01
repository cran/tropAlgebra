#'
#'
#'    Addition of vectors: Tropical Algebra Function.
#'   This function add two vectors and returns the resultant vector Vectors length must be the equal.
#'  @param x is a vector.
#'  @param  y is a vector.
#'  @return sum of X and Y in tropical algebra.
#'
#'
#'  @examples
#'   vectorAdd(c(5,6,7),(c(1,2,3))
#'  e.g.
#'  X<-c(5,6,7)
#'  Y<-c(1,2,3)
#'  Now function call:
#'  vectorAdd(X,Y)
#' Then the sum is:
#' [1]  [2]  [3]
#'  1    2    3
#'
#' @export
vectorAdd<-function(x,y){
  i<-1
  if(length(x) == length(y))
  {
    for(val in 1:x){
      if(x[i]>y[i])
        x[i]<-x[i]

    }
    return (x)
  }else
    return(NULL)
}
#'
#'
