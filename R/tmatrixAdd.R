#'
#'
#'  Tropical Additon of Matrices:
#'
#'   This funciton sums two matrices(first matrix scaled by \code{alpha}; if user doesnt't pass \code{alpha} first matrix will not be scalled) and return the resultant matrix.This function works only if both matrices are of same size.
#'   Either if the size of both matrices is not same or if any one or both matrices is not numeric matrix then this function returns 'NULL'.
#'  @param alpha is a numeric value
#'  @param X is a Matirx.
#'  @param Y a is Matrix.
#'
#'  @return Tropical sum of \code{X} and \code{Y}
#'
#'  @examples
#'  x<-matrix(c(2,3,5,7),ncol=2,nrow=2)
#'  y<-matrix(c(6,3,1,9),ncol=2, nrow=2)
#'  tmatrixAdd(x,y)
#'
#'@export
#'
tmatrixAdd<-function(x,y,alpha=0){
 if(is.numeric(x)&&is.numeric(y)&&is.numeric(alpha)){
 x<-tscale(alpha,x)
   i<-1
  j<-1
  if((nrow(x) == nrow(y)) && (ncol(x) == ncol(y)))
  {
    for(i in 1:nrow(x)){
      for(j in 1:ncol(x))
          x[i,j]<-tadd(x[i,j],y[i,j])

    }
    return(x)
  }else
    return(stop("matrices' size must be equal"))
 }else
   return(stop("arguments must be numeric values"))
}
#'
#'
