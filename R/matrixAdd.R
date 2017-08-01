#'
#'
#'  Tropical Additon of Matrices:
#'
#'   This funciton sums two matrices and return the resultant matrices.Keep in mind both matrices must have equal number of rows and columns.
#'  @param X is a Matirx.
#'  @param Y a is Matrix.
#'
#'  @return Resultant Matrix.
#'
#'  @exa
#'  matrixAdd(matrix(c(2,3,5,7),ncol=2,nrow=2),matrix(c(6,3,1,9),ncol=2, nrow=2))
#'
#'
#'  e.g.
#'      [1]  [2]
#'  [1]  2    3     1st matrix that is X
#'  [2]  5    7
#'      [1]  [2]
#'  [1]  6    3     2nd matrix that is Y
#'  [2]  1    9
#'  Now, calling function...
#'  matrixAdd(X,Y)
#'  Then Tropical sum of Matrices will be...
#'
#'      [1]  [2]
#'  [1]  2    3     Resultant Matrix
#'  [2]  1    9
#'
#'@export
#'
matrixAdd<-function(x,y){
  i<-1
  j<-1
  if((nrow(x) == nrow(y)) && (ncol(x) == ncol(y)))
  {
    for(i in 1:nrow(x)){
      for(j in 1:ncol(x)){
        if(x[i,j]>y[i,j])
          x[i,j]<-y[i,j]
      }

    }
    return(x)
  }else
    return(NULL)
}
#'
#'
