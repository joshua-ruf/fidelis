#' Percent Change Without Div by 0 Error
#' 
#' Calculates the percent change of two numbers or vectors, if prior = 0 then percent change is 100%
#' 
#' @param prior A numeric object
#' @param current A numeric object
#' @param format If TRUE (default) then output is converted to a percentage with formattable, otherwise proportion
#' @param ... Any arguments passed to formattable::percent, most likely digits
#' 
#' @export
#' 
#' @examples
#' pct_change(0, 1) returns 100.00%
#' pct_change(2, 3, format = F) returns 0.5
#' 



pct_change <- function(prior, current, format = T, ...){
  
  out <- ifelse(prior == 0, sign(current), (current/prior) - 1)
  
  if(format == T){
    formattable::percent(out, ...)
  }else{
      out}
  
}