
#' Split a Character String Across 2 Lines
#'
#' @description This function splits a character string across two lines, keeping lines as even as possible.
#'     Replaces the middlemost " " (determined by string length) with "\\n"; and does not perform replacement if
#'     lines would have words with 3 or fewer characters.
#'
#' @param text A vector of character strings.
#'
#' @export
#'
#' @note To use with ggplot2 add \code{scale_x_discrete(labels = split_line)} to your ggobject.
#'
#' @examples
#' split_line(c('Hello World!', 'Goodbye Cruel World', 'To Myself'))
#' # Returns: 'Hello\nWorld!' 'Goodbye\nCruel World' 'To Myself'

split_line <- function(text){

  sapply(text, function(i){

    n <- nchar(i)

    spaces <- as.numeric(gregexpr(' ', i)[[1]])

    spaces <- spaces[spaces > 4 & (n - spaces) > 3]

    if(length(spaces) > 0){
      place <- spaces[which.min(abs(spaces - n/2))]
      substr(i, place, place) <- '\n'
      }

    i

  }, USE.NAMES = F)
}



