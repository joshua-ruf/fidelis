
#' Formatting Large Currency Values
#'
#' @description These functions are extensions to formattable's comma() function.
#' \itemize{
#'    \item\code{.k} divides by 1,000 and adds "k" suffix;
#'    \item\code{.m} divides by 1,000,000 and adds "M" suffix;
#'    \item\code{.b} divides by 1,000,000,000 and adds "B" suffix.
#'    \item\code{.auto} automatically determines and applies whichever of the above formatting options is best.
#'
#' }
#'
#' @param x A numeric object
#' @param digits Number of rounded digits, default is 2
#'
#' @seealso [formattable::comma()]
#'
#' @name comma.
NULL
#> NULL

#' @export
#' @rdname comma.
comma.k <- function(x, digits = 2L){

  formattable::suffix(formattable::comma(x/1e3, digits = digits), 'k')

}

#' @export
#' @rdname comma.
comma.m <- function(x, digits = 2L){

  formattable::suffix(formattable::comma(x/1e6, digits = digits), 'M')

}

#' @export
#' @rdname comma.
comma.b <- function(x, digits = 2L){

  formattable::suffix(formattable::comma(x/1e9, digits = digits), 'B')

}

#' @export
#' @rdname comma.
comma.auto <- function(x, digits = 2L){

  min <- min(x[x != 0])

  if(min >= 1e8){
    fidelis::comma.b(x, digits = digits)
  } else if(min >= 1e5){
    fidelis::comma.m(x, digits = digits)
  } else if(min >= 1e3 & max(x) < 1e6){
    fidelis::comma.k(x, digits = 1)
  } else{
    formattable::comma(x, digits = 0)
  }

}
