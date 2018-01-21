#'
#'
#' Transpose of a Matrix
#'This fucntion interchage the rows into columns or columns into rows.
#'  If the given argument is not a matrix this function returns 'NULL' otherwise it tronsposed the matrix and returns in the same variable that was passed in argument like pass by reference. It change values in memory.
#'  @param X is a Matirx.
#'
#'  @return  Transposed matrix \code{X}
#'
#'
#'    @examples
#'  X<-matrix(c(2,5,3,7),ncol=2,nrow=2)
#'  transpose(X)
#'  @export
transpose<-function(X){
if(is.matrix(X)){
    i<-1
    k<-1
    j<-1
    temp<-matrix(data = NA, nrow = ncol(X), ncol = nrow(X))
    for(i in 1:nrow(X))
      for(j in 1:ncol(X))
        temp[j,i]<-X[i,j]
    eval.parent(substitute(X<-temp))
}else
  return(stop("argument should be a matrix."))
}
#'
#'
