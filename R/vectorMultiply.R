#'
#'
#'    Product: Tropical Algebra Function
#'   This functio returns the product of two vectors Vectors length must be the equal.
#'  @param X  A vector.
#'  @param Y A vector.
#'
#'  @return Product of \code{X} and \code{Y} in tropical algebra.
#'
#'
#'  @examples
#'     vectorMultiply(c(5,6,7),(c(1,2,3)))
#'e.g.
#'  X<-c(5,6,7)
#'  Y<-c(1,2,3)
#'  Now function call:
#'  vectorAdd(X,Y)
#' Then the Product is:
#' [1]  [2]  [3]
#'  6    8    10
#'
#'  @export
vectorMultiply<-function(x,y){
  i<-1
  if(length(x) == length(y))
  {
    for(val in 1:x){
      x[i]<-x[i]+y[i]

    }
    return(x)
  }else
    return(NULL)
}
#'
#'
