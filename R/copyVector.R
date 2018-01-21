#'
#'
#'  Copy Vector
#'
#'   This function copies the first vector in second one. this fucntion will work only if both vectors are of same length.
#'   If the length of source and target vectors are not same it returns 'NULL'.
#'  @param x  Vector to be copied.
#'  @param y  Target Vector.
#'  @param Both Vectors must have equal length.
#'
#'  @return  Target vector y
#'
#'
#'    @examples
#'
#'  x<-c(1,2,3,4)
#'  y<-c(NA,NA,NA,NA)
#'  copyVector(x,y)
#'
#'@export
copyVector<-function(x,y){
  if(length(x)==length(y)){
    xx<-x
    yy<-y

    i<-1

    for(i in 1:length(xx)){
      yy[i]<-xx[i]
    }


    eval.parent(substitute(x<-xx))
    eval.parent(substitute(y<-yy))
  }else
    return(stop("vectors' length must be equal"))
}
