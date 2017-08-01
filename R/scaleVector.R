#'
#'
#'  Scalign of a Vector in Tropical Algebra.
#' This function returns the scaled vector Y by a value X.
#'  @param X a number.
#'  @param Y a Vector.
#'
#'  @return  The scaled vector Y by X.
#'
#'
#'  @examples
#'  scaleMatrix(5,matrix(c(2,3,5,7),ncol=2,nrow=2))
#'e.g.
#' Y<-c(1,2,3)
#' x<-5
#' Now calling fucntiion:
#' scaleVector(x,y)
#' it returns
#' [1]  [2]  [3]
#'  6    7    8
#'
#'
#'  @export
scaleVector<-function(x,y){
  i<-1
  for(i in 1:length(y)){
    y[i]<-y[i]+x
  }
  return(y)
}
