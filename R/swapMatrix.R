#'
#'
#'  Swap Matrices
#'  This function inchange the values of both matrices Both Matrix must have equal number of rows and columns.
#'  @param X a Matrix
#'  @param Y a Matrix.
#'
#'
#'  @return  Exchaged Valued Matrix.
#'
#'
#'  @examples
#'   swapMatrix(matrix(c(2,3,5,7),ncol=2,nrow=2),matrix(c(6,3,1,9),ncol=2, nrow=2))
#' e.g.
#'
#'      [1]  [2]
#'  [1]  2    3     1st matrix that is X
#'  [2]  5    7
#'      [1]  [2]
#'  [1]  6    3     2nd matrix that is Y
#'  [2]  1    9
#'  Now, calling function...
#'  swapMatrix(X,Y)
#'  Then the swapped  Matrices will be...
#''      [1]  [2]
#'  [1]  6    3     1st matrix that is X
#'  [2]  1    9
#'
#'  and
#'
#'      [1]  [2]
#'  [1]  2    3     2nd matrix that is Y
#'  [2]  5    7
#'
#'  @export
swapMatrix<-function(x,y){
  if(nrow(x)==nrow(y)&&ncol(x)==ncol(y)){
    xx<-x
    yy<-y
    temp<-matrix(data = NA,nrow = nrow(x), ncol = ncol(x))
    i<-1
    j<-1
    for(i in 1:ncol(xx)){
      for(j in 1:nrow(xx)){
        temp[i,j]<-xx[i,j]
      }
    }
    for(i in 1:ncol(x)){
      for(j in 1:nrow(xx)){
        xx[i,j]<-yy[i,j]
      }
    }
    for(i in 1:ncol(xx)){
      for(j in 1:nrow(xx)){
        yy[i,j]<-temp[i,j]
      }
    }
    eval.parent(substitute(x<-xx))
    eval.parent(substitute(y<-yy))
  }else
    return(print("Matrices rows and colums must be equal.\n"))
}
