
#' Assign months to current period/ prior period/ NA
#'
#' @description This function returns a 24x5 data.table; the first column is the 24 effective periods, the second to fifth
#'     columns assign each effper to either 'current' or 'prior' depending on the rolling window. The second column \code{col}
#'     is the 3 month rolling window and the others are identified by their number
#'
#' @param last_month Date object, or character object formatted "\%Y-\%m-\%d"; automatically rounds down to the first day of the month.
#' @param months Numeric, number of months to include in each period (max 12).
#'
#' @note `set_dates2()` is a more efficient version of `set_dates()`; it returns a 24x2 data.table, with the second column determined
#'      by the `months` input.
#'
#' @examples
#' last_month <- as.Date('2019-02-01') or last_month <- '2019-02-01'
#' dates_df <- set_dates(last_month)
#' dates_df[!is.na(col), .(effper, col)] #could then be used to merge on to keep only certain effpers
#'
#' @export

set_dates <- function(last_month){

  if(is.character(last_month)){last_month <- as.Date(last_month, format = '%Y-%m-%d')}

  if(lubridate::day(last_month) != 1){last_month <- lubridate::floor_date(last_month, unit = 'months')}

  dates_df <- data.frame(effper = seq(last_month, length = 24, by = '-1 month'))

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

#' @export
#' @rdname set_dates
set_dates2 <- function(last_month, months = 3){

  if(is.character(last_month)){last_month <- as.Date(last_month, format = '%Y-%m-%d')}

  if(lubridate::day(last_month) != 1){last_month <- lubridate::floor_date(last_month, unit = 'months')}

  data.table(
    effper = seq(last_month, length = 24, by = '-1 month'),
    col = c(rep('current', months),
            rep(NA, 12-months),
            rep('prior', months),
            rep(NA, 12-months))
    )

}
