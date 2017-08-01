#'
#'
#' Scaling of a Matrix in Tropical Algebra
#'   This function returns the scaled matrix 'Y' by a value X .
#'  @param X a number.
#'  @param Y a Matrix.
#'
#'  @return  The scaling of Y to x.
#'
#'
#'  @examples
#'  scaleMatrix(5,matrix(c(2,3,5,7),ncol=2,nrow=2))
#'    e.g.
#'      [1]  [2]
#'  [1]  2    3      matrix  Y
#'  [2]  5    7
#'
#'  x<-5
#'  Now function call to scale matrix Y by 5
#'      [1]  [2]
#'  [1]  7    8      Resultant matrix
#'  [2]  10   12
#' @export
#'
scaleMatrix<-function(x,y){
  j<-1
  i<-1
  for(i in 1:nrow(y)){
    for(j in 1:ncol(y)){
      y[i,j]=y[i,j]+x
    }

  }
  return(y)
}
