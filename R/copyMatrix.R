#'
#'
#'  Copy Matrices
#'
#' This function copies the first matrix in second one. this fucntion will work only if both matrices are of same size.
#'  If the size of source and target matrices are not same it returns 'NULL'.
#'  @param X  Matrix to br copied.
#'  @param Y Target Matrix.
#'
#'
#'  @return  Copy first Matrix in second one.
#'
#'
#'  @example
#'  X<-matrix(c('a','b','c','d'),nrow=2, ncol=2)
#'  Y<-matrix(c(NA,NA,NA,NA),nrow=2, ncol=2)
#'  copyMatrix(X,Y)
#'
#' @export
copyMatrix<-function(X,Y){
  if(nrow(X)==nrow(Y)&&ncol(X)==ncol(Y)){
    xx<-X
    yy<-Y
    j<-1
    i<-1

    for(i in 1:nrow(xx)){
      for(j in 1:ncol(xx))
        yy[i,j]<-xx[i,j]
    }


    eval.parent(substitute(X<-xx))
    eval.parent(substitute(Y<-yy))
  }else
    return(stop("Matrices' size must be equal"))
}
