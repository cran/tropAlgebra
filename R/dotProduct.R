#'
#'
#'Dot Product of X and Y: Tropical Algebra Function.
#' Dot product is actually the multiplication of transpose of 1st matrix then multiplicatoin of that transposed matrix and 2nd matrix(Y). This function does this and returns the resultant matrix after calculation of thes steps, Colunms of 1st matrix must be eqaual to rows of 2nd one..
#'  @param X is a Matirx.
#'  @param Y a is Matrix.
#'
#'  @return Resultant Matrix of dot product.
#'
#'
#'    @examples
#'  dotProduct(matrix(c(2,5,37),ncol=2,nrow=2),matrix(c(6,10,3,6),ncol=2, nrow=2))
#'
#'   e.g.
#'      [1]  [2]
#'  [1]  2    3     1st matrix that is X
#'  [2]  5    7
#'  .........................................
#'      [1]  [2]
#'  [1]  6    3     2nd matrix that is Y
#'  [2]  1    9
#' ..........................................
#'
#'  Now, calling function...
#'  dotProduct(X,Y)
#' Transpose of matrix 'X is obtained by interchanging rows into coulums OR columns into rows.
#''      [1]  [2]
#'  [1]  2     5     Trransposed matrix
#'  [2]  3     7
#...............................................
#'   Then Tropical Dot Product of Matrices will be...
#'
#'      [1]  [2]
#'  [1]  6    10     Resultant Matrix
#'  [2]  3    6
#'
#'
#'  @export
dotProduct<-function(x,y){
  rmatirxRow<-nrow(x)
  rmatirxCol<-nrow(y)
  if(rmatirxRow== rmatirxCol)
  {
    i<-1
    k<-1
    j<-1
    temp<-matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
    for(i in 1:nrow(x))
      for(j in 1:col(x))
        temp[j,i]<-x[i,j]
    a<-matrix(data = NA, nrow =rmatirxRow, ncol = rmatirxCol )

    for(i in 1:nrow(temp)){
      for(j in 1:ncol(y)){

        for(k in 1:nrow(y)){

          if(k == 1){
            a[i,j]<-x[i,k]+y[k,j]
          }else{
            a[i,j]<-add(a[i,j],(x[i,k]+y[k,j]))
          }
        }
      }
    }
    return(a)
  }else
    return(print("Invalid parameters."))
}
#'
#'
