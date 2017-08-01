#'
#'
#'  Copy Matrices
#'
#' This function copies the 1st matrix in second one if both the matirces are of equal number of rows and columns.
#'  @param X a Matrix.
#'  @param Y a Matrix.
#'
#'
#'  @return  Copy first Matrix in second one.
#'
#'
#'    @example
#'  copyMatrix(matrix(c(1,2,3,4),ncol=2,nrow=2),matrix(data=NA,ncol=2, nrow=2))
#'  e.g.
#'  x is a matrix.
#'  x<-matrix(c(1,2,3,4), nrow=2, ncol=2)
#'  and y is an empty matrix
#'  y<-matrix(data=NA,  nrow=2, ncol=2)
#'  we want copy matirx 'x' into 'y'. This fuction do this.
#'  copyMatrix(x,y)
#'  now values of 'y' matirx will be same as of 'x' matrix.
#'
#'
#'
#' @export
copyMatrix<-function(x,y){
  if(nrow(x)==nrow(y)&&ncol(x)==ncol(y)){
    xx<-x
    yy<-y
    j<-1
    i<-1

    for(i in 1:nrow(xx)){
      for(j in 1:ncol(xx))
        yy[i,j]<-xx[i,j]
    }


    eval.parent(substitute(x<-xx))
    eval.parent(substitute(y<-yy))
  }else
    return(print("Vectors  must be equal in length.\n"))
}
