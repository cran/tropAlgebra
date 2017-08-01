#'
#'
#'  Copy Vector
#'
#'   This function copies the 1st vector's values in the 2nd vector, Vectors must have equal length..
#'  @param X a Vector.
#'  @param Y a Vector.
#'  @param Both Vectors must have equal length.
#'
#'  @return  Copy first Vector in second one.
#'
#'
#'    @examples
#'   copyVector(c(1,2,3,4),c(NA,NA,NA,NA))
#'
#'  e.g.
#'  x is a vector
#'  x<-c(1,2,3,4)
#'  and y is an empty vector
#'  y<-c(NA,NA,NA,NA)
#'  we want copy vector 'x' into 'y'. This fuction do this
#'  copyVector(x,y)
#'  now values of 'y' vector will be same as of 'x' vector
#'
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
    return(print("Vectors  must be equal in length.\n"))
}
