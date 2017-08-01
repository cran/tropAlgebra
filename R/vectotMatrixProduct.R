#' Vector Matrix Product
#' This function returns a vector in result of vector matrix product.
#' @param x a Vector
#' @param y a Matrix
#' @return Resultant vector of vector matrix product
#' @examples
#'vectorMatrixProduct(c(1,2,3),matrix(c(1,2,3,4,5,6,1,2,3), nrow = 3, ncol = 3))
#' e.g
#' a vector x
#' x<-c(1,2,3)
#' [1] 1 2 3
#' a matrix y
#' y<-matrix(c(1,2,3,4,5,6,1,2,3), nrow = 3, ncol = 3)
#'      [,1] [,2] [,3]
#' [1,]    1    4    1
#' [2,]    2    5    2
#' [3,]    3    6    3
#' Now function call
#' vectotMatrixProduct(x,y)
#' Then the resultant vector is returned
#' [1] 2 6 4
#' @export
vectotMatrixProduct<-function(x,y){
  if(length(x) == nrow(y)){
    i<-1
    j<-1
    k<-1

      for(j in 1:ncol(y)){

        for(k in 1:nrow(y)){

          if(k == 1)
            x[j]<-x[j]+y[k,j]
          else
            x[j]<-add(x[j],(x[j]+y[k,j]))


      }
      }
    return(x)
  }else
    return(print("Invalid oparands..."))
}
