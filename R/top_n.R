#'
#' Return the Top n values of a data.frame
#'
#' This function returns the top n values of a data.frame or data.table based on user provided function and column
#'
#' @param df A data.table or data.frame
#' @param col A character string of the column used to pick top values
#' @param n The number of top values to keep, nrow(df) < n then nrow(df) is used
#' @param fun The ordering function to apply to col. Default is function(x){-x} which keeps largest.
#' @param keep_ordering Logical, if TRUE then maintain original ordering, if FALSE order based on fun
#'
#' @export
#'
#' @examples
#' n_top(df, col = 'v1', n = 5) returns a data.table with the 5 largest values of v1 in the original ordering
#' n_top(df, col = 'v1', n = 10, fun = function(x){-abs(x)}, keep_ordering = F) returns a data.table with 10 largest absolute values of v1, ordered by descending absolute values of v1
#'

n_top <- function(df, col, n, fun = function(x){-x}, keep_ordering = T) {

  if(is.data.table(df)){
    d <- copy(df)
  }else {
    d <- as.data.table(df)
      }

  n <- min(n, nrow(d))

  if(keep_ordering){d[, original_order := 1:nrow(d)]}

  d <- d[order(fun(get(col)))][1:n,]

  if(keep_ordering){d <- d[order(original_order)][, original_order := NULL]}

  as.data.table(d)
}




