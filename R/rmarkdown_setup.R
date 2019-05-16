
#' Rmarkdown Page and Column Setup, and Line Breaks!
#'
#' @description Changing the page size and adding two column support for HTML RMarkdown documents is relatively easy
#'     but requires either altering an external .html file or a fair bit of typing in your .Rmd file. These functions
#'     make it easier to add these supports, fine-tuned to each .Rmd file.
#'     \itemize{
#'       \item\code{setup2cols()} sets the size of the left and right columns
#'       \item\code{setup_pagewidth()} sets the width of the page
#'       \item\code{begin_left()} and \code{begin_right} begin column support
#'       \item\code{end_left()} and \code{end_right} ends column support
#'       \item\code{br()} is thrown in for good measure as line breaks in Rmarkdown are finicky. Just call \code{br()}
#'          in an r chunk with the option \code{results='asis'}
#'          }
#'
#' @param left Integer, percent of the page width to devote to the left column, default is 50
#' @param right Integer, percent of the page width to devote to the right column, default is 50
#' @param width Integer, page width in px
#' @param n Integer, number of line breaks to include, deafult is 1
#'
#' @details To use these functions put \code{setup2cols()} and/or \code{setup_pagewidth()} in a r chunk with the option
#'    \code{results='asis'} at the beginning of your document. Then, when you want column output, make another r chunk
#'    (also with \code{results='asis'}) and surround your desired output with \code{begin_left()} and \code{end_left()}.
#'    These functions were designed to be used with a for loop, hence why \code{results='hold'} is not used.
#'
#' @name rmarkdown_setup
#'
#' @note To use ggplot objects with \code{begin_left()} and \code{begin_right()} pass them through \code{plot()}.
#'     To use kable objects pass them through \code{htmltools::knit_print.html()}.
#'


#' @export
#' @rdname rmarkdown_setup
setup2cols <- function(left = 50, right = 50){

cat(
  sprintf("<style>
    .column-left{
      float: left;
      width: %s%%;
      text-align: left;
    }
  .column-right{
    float: right;
    width: %s%%;
    text-align: left;
  }
  </style>",
          left,
          right)
)

}


#' @export
#' @rdname rmarkdown_setup
begin_left <- function(){

  cat('  \n<div class="column-left">  \n')

}


#' @export
#' @rdname rmarkdown_setup
begin_right <- function(){

  cat('  \n<div class="column-right">  \n')


}

#' @export
#' @rdname rmarkdown_setup
end_left <- function(){cat('  \n</div>   \n')}

#' @export
#' @rdname rmarkdown_setup
end_right <- function(){cat('  \n</div>   \n')}

#' @export
#' @rdname rmarkdown_setup
setup_pagewidth <- function(width = 1700){

  cat(
    sprintf('<style type="text/css">
    .main-container {
      max-width: %spx;
      margin-left: auto;
      margin-right: auto;
    }
    </style>',
            width)
  )

}

#' @export
#' @rdname rmarkdown_setup
br <- function(n = 1){cat(rep('  \n', n), sep = '')}

