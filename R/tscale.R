#'
#'
#' Tropical Scaling of a Matrix OR a Vector
#'   This function returns the tropical scaled matrix or vector 'Y' by a value X .This function works only if Y is matrix or vector.
#'  @param X a number.
#'  @param Y a numeric matrix or a numeric vector.
#'
#'  @return  The scaled matrix Or vector Y
#'
#'
#'  @examples
#' x<-5
#' y<-matrix(c(2,3,5,7),ncol=2,nrow=2)
#' tscale(x,y)
#' y<-c(1,2,3)
#' tscale(x,y)
#' @export
#'
tscale<-function(x,y){
if(is.numeric(x)&& is.numeric(y)){
  if(is.matrix(y)){
  j<-1
  i<-1
  for(i in 1:nrow(y)){
    for(j in 1:ncol(y)){
      y[i,j]<-y[i,j]+x
    }

  }
  return(y)
  }
  else if(is.vector(y)){
    for(i in 1:length(y)){
      y[i]<-y[i]+x
    }
    return(y)
  }else{
    return(stop("arguments must be a numeric value and a numeric vector or numeric matrix."))
  }
}else
 { return(stop("arguments must be a numeric value and a numeric vector or numeric matrix."))}
}
