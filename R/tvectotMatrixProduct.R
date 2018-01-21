#' Tropical Vector Matrix Product
#' This function returns a vector in result of vector matrix tropical product.This function works only if length of vector equal to number of rows matrix.
#'  If the given argument \code{x} is not a numeric vector or argument \code{Y} is not a numeric matrix and the length of vector is not equal to the number of rows of matrix then the function returns 'NULL'.
#' @param Y a Matrix
#' @param x a Vector
#'
#' @param transM logical value
#' @return Resultant vector of vector matrix product
#' @examples
#'x<-c(1,2,3)
#'Y<-matrix(c(1,2,3,4,5,6,1,2,3), nrow = 3, ncol = 3)
#'tvectotMatrixProduct(x,Y)
#' @export
tvectotMatrixProduct<-function(Y,x){

  if(is.vector(x)&&is.matrix(Y)&& is.numeric(x)&&is.numeric(Y)){
    r<-as.vector(Inf,mode = 'any')
    if(length(x) == ncol(Y)){

         for(j in 1:nrow(Y)){

        for(k in 1:ncol(Y)){


          if(k == 1){
            r[j]<- x[k]+Y[j,k]

          }else{
            r[j]<-tadd(r[j],(x[k]+Y[j,k]))
          }

        }
      }

       return(r)
  }else
    return(stop("vector length must be equal to columns of matrix."))
  }else
    return(stop("arguments must be numeric vector and matrix"))
}
