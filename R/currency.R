
#' Formatting Large Comma Values
#'
#' @description These functions are extensions to formattable's currency() function.
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
#' @seealso [formattable::currency()]
#'
#' @name currency.
NULL
#> NULL

#' @export
#' @rdname currency.
currency.k <- function(x, digits = 2L){

  formattable::suffix(formattable::currency(x/1e3, digits = digits), 'k')

}

#' @export
#' @rdname currency.
currency.m <- function(x, digits = 2L){

  formattable::suffix(formattable::currency(x/1e6, digits = digits), 'M')

}

#' @export
#' @rdname currency.
currency.b <- function(x, digits = 2L){

  formattable::suffix(formattable::currency(x/1e9, digits = digits), 'B')

}

#' @export
#' @rdname currency.
currency.auto <- function(x, digits = 2L){

  min <- min(x[x != 0])

 if(min >= 1e8){
   fidelis::currency.b(x, digits = digits)
 } else if(min >= 1e5){
   fidelis::currency.m(x, digits = digits)
 } else if(min >= 1e3 & max(x) < 1e6){
   fidelis::currency.k(x, digits = 1)
 } else{
     formattable::currency(x, digits = 0)
   }

}


