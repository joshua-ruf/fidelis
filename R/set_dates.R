
#' Create a data.table with 3,6,9,12 month periods
#' 
#' @description This function returns a 24x5 data.table; the first column is the 24 effective periods, the second to fifth
#'     columns assign each effper to either 'current' or 'prior' depending on the rolling window. The second column \code{col}
#'     is the 3 month rolling window and the others are identified by their number
#'     
#' @param last_month A date object, the final effective period. Ensure it is the first day of the month.
#' 
#' @export
#' 
#' @examples
#' last_month <- as.Date('2019-02-01')
#' dates_df <- set_dates(last_month)
#' dates_df[!is.na(col), .(effper, col)] #could then be used to merge on to keep only certain effpers
#' 
#' 

set_dates <- function(last_month){
  
  dates_df <- data.frame(effper = last_month - months(0:23))
  
  dates_df[, c('col', 'col6', 'col9', 'col12')] <- NA
  
  dates_df[1:3, c('col', 'col6', 'col9', 'col12')] <- 'current'
  dates_df[4:6, c('col6', 'col9', 'col12')] <- 'current'
  dates_df[7:9, c('col9', 'col12')] <- 'current'
  dates_df[10:12, c('col12')] <- 'current'
  
  dates_df[1:3+12, c('col', 'col6', 'col9', 'col12')] <- 'prior'
  dates_df[4:6+12, c('col6', 'col9', 'col12')] <- 'prior'
  dates_df[7:9+12, c('col9', 'col12')] <- 'prior'
  dates_df[10:12+12, c('col12')] <- 'prior'
  
  as.data.table(dates_df)
  
}