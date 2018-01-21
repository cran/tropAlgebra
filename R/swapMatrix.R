#'
#'
#'  Swap Matrices
# ' This function interchange the values of both matrices.This function works only if both matrices are of same size.
#' If the size of source and target matrices are not same it returns 'NULL'. This function swaps the matrices in memory(like pass by reference), it does not return the matrices.
#'  @param X a Matrix
#'  @param Y a Matrix.
#'
#'
#'  @return  Exchaged Valued Matrix.
#'
#'
#'  @examples
#' x<-matrix(c(2,3,5,7),ncol=2,nrow=2)
#' y<-matrix(c(6,3,1,9),ncol=2, nrow=2)
#' swapMatrix(x,y)
#'
#'
#'  @export
swapMatrix<-function(X,Y){
  if(nrow(X)==nrow(Y)&&ncol(X)==ncol(Y)){
    xx<-X
    yy<-Y
    temp<-matrix(data = NA,nrow = nrow(X), ncol = ncol(X))
    i<-1
    j<-1
    for(i in 1:ncol(xx)){
      for(j in 1:nrow(xx)){
        temp[i,j]<-xx[i,j]
      }
    }
    for(i in 1:ncol(xx)){
      for(j in 1:nrow(xx)){
        xx[i,j]<-yy[i,j]
      }
    }
    for(i in 1:ncol(xx)){
      for(j in 1:nrow(xx)){
        yy[i,j]<-temp[i,j]
      }
    }
    eval.parent(substitute(X<-xx))
    eval.parent(substitute(Y<-yy))
  }else
    return(stop("matrices' size must be equal"))
}
