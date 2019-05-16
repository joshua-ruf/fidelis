
#' Easily Select Elements from a Character Vector
#'
#' @description This function returns elements that contain characters specified in with and do not contain characters specified in without
#'
#' @param names A character vector
#' @param with A character vector of items we want \code{names} to contain. Default to NULL.
#' @param without A character vector of items we do not want \code{names} to contain. Default to NULL.
#'
#' @export
#'
#' @examples
#'
#' names <- c('one', 'two', 'three', 'three3')
#' with <- c('one', 'three')
#' without <- c('three3')
#'
#' names_with(names, with, without) ##returns c('one', 'three')
#'
#' names_with(names, with) ##returns c('one', 'three', 'three3')
#'

names_with <- function(names, with = NULL, without = NULL){

  if(!is.null(with)){
    names <- grep(paste(with, collapse = "|"), names, value = T)
  }

  if(!is.null(without)){
    without <- grep(paste(without, collapse = "|"), names, value = T)
    names[!(names %in% without)]
      } else {names}

}

