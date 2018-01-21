#'
#'
#'  Swap  Vectors
#'   This function swaps the values of both vectors. This function works only if both vectors have equal length.
#'  If the length of source and target vectors are not same it returns 'NULL'. This function swaps the vectors in memory(like pass by reference), it does not return the vectors.
#'  @param X a Vector.
#'  @param Y a Vector.
#'
#'
#'  @return  Returns the swapped vectors \code{x} and \code{y}
#'
#'
#'  @examples
#'  x<-c(6,7,8)
#'  y<-c(3,2,1)
#'  swapVector(x,y)
#'
#'
#'@export
swapVector<-function(x,y){
  if(length(x)==length(y)){
    xx<-x
    yy<-y
    temp<-c(data=NA,length(x))
    i<-1

    for(i in 1:length(xx)){
      temp[i]<-xx[i]
    }

    for(i in 1:length(xx)){

      xx[i]<-yy[i]

    }
    for(i in 1:length(xx)){
      yy[i]<-temp[i]

    }
    eval.parent(substitute(x<-xx))
    eval.parent(substitute(y<-yy))
  }else
    return(stop("vectors' length must be equal"))
}
