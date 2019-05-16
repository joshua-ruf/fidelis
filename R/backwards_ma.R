
#' Backwards Looking Moving Average Function
#'
#' Calculates the moving average of object x using the last n (including current) observations
#' @param x Numeric object
#' @param n Number of periods
#'
#' @examples
#' backwards_ma(x, n = 3)
#'

#' @export
backwards_ma <- function(x, n){

  x0 = x

  for(i in 1:(n-1)){x = x + shift(x0, i)}

  return(x/n)

}


