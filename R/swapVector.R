#'
#'
#'  Swap  Vectors
#'   This function swaps the values of both vectors, Both Vectors must have equal length.
#'  @param X a Vector.
#'  @param Y a Vector.
#'
#'
#'  @return  Exchaged Valued vectors.
#'
#'
#'  @examples
#'  swapVector((c(6,7,8),(c(3,2,1))
#'e.g.
#'  [1]  [2]  [3]
#'  6    7    8     vector X
#'  ...................
#'  [1]  [2]  [3]
#'   3    2    1     vector Y
#'  ...................
#'  Now function call:
#'  swapVector(x,y)
#'  Then swapped vectors are :
#'  [1]  [2]  [3]
#'  6    7    8     vector Y
#'  ...................
#'  [1]  [2]  [3]
#'   3    2    1     vector X
#'  ...................
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
    return(print("Vectors  must be equal in length.\n"))
}
