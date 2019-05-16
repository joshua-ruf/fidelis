
#' Convert NAs to Zero in data.table
#' 
#' @description This function turns NA values to zero (or user specified value)
#' 
#' @param D A data.table or data.frame
#' @param cols A vector of column names to rid of NAs
#' @param value Defaults to 0, the value with which to replace NAs
#' 
#' @export
#' 

NAto0 <- function(D, cols, value = 0){
  
  if(!is.data.table(D)){setDT(D)}
  
  for(n in cols) set(D, i = which(is.na(D[[n]])), j = n, value = value)
  
}