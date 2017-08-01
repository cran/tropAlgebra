#' Sum(+): Tropical Algebra Function
#'
#' It takes two parameters(numeric) in X and y and returns the tropical sum of these numbers e.g. x + y=minimum(x,y).
#'
#'  @param x a number.
#'  @param y  a number.
#'
#'  @return  The sum of x and y.
#'
#'  @examples
#'   add(6,5)
#'
#' @export
add<-function(x,y){
  if(x<y)
    return(x)
  else
    return(y)
}
