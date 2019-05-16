#' Convert Date to USA style date
#' 
#' Converts a date object to month-day-year style USA date
#' @param x The date object
#' @param numeric If TRUE, returns date in numbers (ie. 02/14/2019); if FALSE, returns Febraury 14, 2019. Default TRUE
#' 
#' @export

usdate <- function(x, numeric = T){
  if (numeric == T){format(x, '%m/%d/%Y')}
  else {format(x, '%B %d, %Y')}
}