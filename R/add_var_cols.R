
#' Add Variance Columns for 3612 Reports
#'
#' @description These function adds variance and percent variance columns to a data.table for each of the vars specified.
#'      \code{add_var_cols2()} performs the same operations as \code{add_var_cols2()} but it also rehsapes the data.table
#'      using data.table's \code{dcast()} based on the variable names provided in \code{row_vars}.
#'
#' @param D A data.table
#' @param vars A character vector of column names to use to calculate new variance columns
#' @param reorder Logical, if \code{TRUE} (default) then reorders the columns in the order the vars are specified -
#'      order goes as follows: prior, current, var, var_pct
#' @param format.pct Logical, if \code{TRUE} (default) then \code{pct_change()} will format the percentage variable
#' @param row_vars A character vector of column names that will serve as the unique rows of the final data.table -
#'      usually product, region, and mmcor_desc
#'
#' @note \code{add_var_cols()} does not need to be reassigned to an R object, however \code{add_var_cols2()} does need to be reassigned.
#'      Variables cannot be included in both \code{var} and \code{row_vars} and variables appearing in neither are automatically dropped.
#'      If using \code{add_var_cols2()} a column \code{col} must exist containing two values 'prior' and 'current'.



#' @export
add_var_cols <- function(D, vars = c('cost_pmpm', 'claims_per_1000', 'cost_per_claim'),
                         reorder = T, format.pct = T){

  if(!is.data.table(D)){setDT(D)}

  for(var in vars){

    set(D,
        j = c(paste0(var, '_var'), paste0(var, '_var_pct')),
        value = list(D[[paste0(var, '_current')]] - D[[paste0(var, '_prior')]],
                     pct_change(D[[paste0(var, '_prior')]], D[[paste0(var, '_current')]], format = format.pct)))
  }

  if(reorder){
    new_vars <- c(sapply(vars,
                  FUN = function(x){paste0(x, c('_prior', '_current', '_var', '_var_pct'))}))

    setcolorder(D, neworder = c(names(D)[!(names(D) %in% new_vars)], new_vars))
    }
}



#' @export
#' @rdname add_var_cols
add_var_cols2 <- function(D, vars = c('cost_pmpm', 'claims_per_1000', 'cost_per_claim'),
                          reorder = T, format.pct = T,
                          row_vars = c('product', 'region', 'mmcor_desc')){

  if(!is.data.table(D)){setDT(D)}

  if(!all(vars %in% names(D))){stop('Not all vars are found in D.')}

  if(!('col' %in% names(D)) && !setequal(unique(D$col), c('prior', 'current'))){
    stop("col column must exist and contain only two values: 'prior' and 'current'.")
    }

  D <- dcast(D,
             formula = as.formula(paste(paste(intersect(row_vars, names(D)), collapse = '+'), 'col', sep = '~')),
             value.var = vars,
             fill = 0)

  if(length(vars)==1){
    names(D)[which(names(D)=='current')] <- paste0(vars, '_current');
    names(D)[which(names(D)=='prior')] <- paste0(vars, '_prior')
  }

  for(var in vars){

    D[, c(paste0(var, '_var'), paste0(var, '_var_pct')) :=
        list(D[[paste0(var, '_current')]] - D[[paste0(var, '_prior')]],
             pct_change(D[[paste0(var, '_prior')]], D[[paste0(var, '_current')]], format = format.pct))]
  }

  if(reorder){
    new_vars <- c(sapply(vars,
                         FUN = function(x){paste0(x, c('_prior', '_current', '_var', '_var_pct'))}))

    setcolorder(D, neworder = c(names(D)[!(names(D) %in% new_vars)], new_vars))
  }

  D
}


