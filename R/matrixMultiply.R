#'  Tropical Product of Matrices
#'    This function returns the product of two matrices. Keep in mind that Rows of 1st matrix must be eqaual to columns of 2nd one.
#'  @param X is a Matirx.
#'  @param Y a is Matrix.
#'
#'  @return Matrix X.
#'
#'  @examples
#'  matrixMultiply(c(2,3,5,7),ncol=2,nrow=2),matrix(c(6,3,1,9),ncol=2, nrow=2))
#'
#' e.g.
#'      [1]  [2]
#'  [1]  2    3     1st matrix that is X
#'  [2]  5    7
#'      [1]  [2]
#'  [1]  6    3     2nd matrix that is Y
#'  [2]  1    9
#'  Now, calling function...
#'  matrixMultiply(X,Y)
#'  Then Tropical sum of Matrices will be...
#'
#'      [1]  [2]
#'  [1]  3    5     Resultant Matrix
#'  [2]  8    8
#'
#'
#'@export
matrixMultiply<-function(x,y){
  i<-1
  k<-1
  rmatirxRow<-ncol(x)
  rmatirxCol<-nrow(y)
  j<-1
  a<-matrix(data = NA, nrow =rmatirxRow, ncol = rmatirxCol )
  if(rmatirxRow == rmatirxCol)
  {
    for(i in 1:nrow(x)){
      for(j in 1:ncol(y)){

        for(k in 1:nrow(y)){

          if(k == 1){
            a[i,j]<-x[i,k]+y[k,j]
          }else{
            a[i,j]<-add(a[i,j],(x[i,k]+y[k,j]))
          }
        }
      }
    }
    return(a)
  }else
    return(print("Invalid parameters."))
}
#'
#'
