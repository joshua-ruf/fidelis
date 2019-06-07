
#' Aggregate a data.frame and create 'ALL' values
#'
#' This function aggregates the specified columns of a data.frame by the specified grouping variables,
#' It also prepares additional aggregations in which each grouping var is set to 'ALL'
#'
#' @param df A data.table or data.frame
#' @param group_vars A character string of variable names to group by
#' @param agg_vars A character string of variable names to aggregate
#' @param fun The function with which to perform aggregation, default is \code{sum} (currently applies the function to all \code{agg_vars})
#' @param no_all A character string indicating which variables are not required to have an 'ALL' row in the final aggregated product, defaults to NULL
#'
#' @details If \code{no_all} == \code{group_vars} then this function merely performs a standard data.table aggregation,
#'      but if \code{no_all} != \code{group_vars} then this function returns all unique combinations of aggregations. Drops
#'      columns that do not appear in \code{group_vars} or \code{no_all}.
#'
#' @export
aggregate_all <- function(df, group_vars, agg_vars, fun = sum, no_all = NULL) {

  `%do%` <- foreach::`%do%`

  if(!is.data.table(df)) {setDT(df)}

  df <- df[, c(group_vars, agg_vars), with = F]

  for (j in names(which(lapply(df[, ..group_vars], is.character)==F)))
    set(df, j=j, value = as.character(df[[j]]))

  grid <- expand.grid(
    setNames(
      data.frame(
        matrix(rep(0:1, length(group_vars)),ncol = length(group_vars))),
        group_vars)
    )

  if(!is.null(no_all)) {

    grid[no_all] <- 0

    grid <- unique(grid)
    }

  foreach::foreach(i = 1:nrow(grid), .combine = rbind, .multicombine = T) %do% {

    D <- copy(df)

    vars <- group_vars[grid[i,] == 1]

    if(length(var) != 0){
      for (j in vars) {set(D, j=j, value = 'ALL')}
      D[, lapply(.SD, fun), by = group_vars]
    } else D

  }

}

