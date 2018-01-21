#'  Tropical Product of Matrices
#'    This function returns the product of two matrices. Keep in mind that Rows of 1st matrix must be eqaual to columns of 2nd one.
#'   If number of columns of first matrix is not equal to the number of rows of second matrix or any one matrix is not a numeric matrix then this functions returns 'NULL'. If mxn is first matrix and axb is a second matrix then mxb will be the resultant matrix.
#'   @param X is a Matirx.
#'  @param Y a is Matrix.
#'
#'  @return Tropical Product of \code{X} and \code{y}
#'
#'  @examples
#'  X<-matrix(c(2,3,5,7),ncol=2,nrow=2)
#'  Y<-matrix(c(6,3,1,9),ncol=2, nrow=2)
#'  tmatrixMultiply(X,Y)
#'
#'@export
tmatrixMultiply<-function(X,Y){
if(is.numeric(X)&& is.numeric(Y)&&is.matrix(X)&&is.matrix(Y)){

  rmatirxRow<-ncol(X)
  rmatirxCol<-nrow(Y)

  a<-matrix(data = Inf, nrow =nrow(X), ncol = ncol(Y) )
  if(rmatirxRow == rmatirxCol)
  {
    for(i in 1:nrow(X)){
      for(j in 1:ncol(Y)){

        for(k in 1:ncol(X)){

          if(k == 1){
            a[i,j]<- X[i,k]+Y[k,j]
          }else{
            a[i,j]<-tadd(a[i,j],(X[i,k]+Y[k,j]))
          }
        }
      }
    }

    return(a)
  }else
    return(stop("Rows of 1st matrix must be eqaual to columns of 2nd one."))
}else
  return(stop("arguments must be numeric matrices, logical values and numeric constants."))
  }
#'
#'
