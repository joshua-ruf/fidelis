
#' Create Binary Variables
#'
#' @description This function creates k-1 binary variables, where k is the number of unique values of \code{var}.
#'     It prints to the console which value will be used as the reference, and therefore won't have a binary variable.
#'
#' @param D A data.table or data.frame
#' @param var A character string of the desired variable to dummy
#' @param rm.var Logical, defaults to TRUE where the original variable will be removed
#'
#' @note In keeping with data.table's methodoly this function does not need to be assigned to a new object
#'
#' @export

dummy <- function(D, var, rm.var = T){

  if(!is.data.table(D)){D <- as.data.table(D)}

  col <- unique(D[[var]])

  for(n in col[-1]) {

    set(D, j = gsub(' ', '_', paste0(var, '_', n)), value = as.integer(D[[var]]==n))

    }

  if(rm.var == T){D[, (var) := NULL]}

  cat('Reference value is', var, '==', col[1])

}
