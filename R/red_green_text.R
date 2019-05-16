
#' Red/Green Font Formatters
#'
#' @description These functions change the font color of a cell of a kable obejct.
#' \itemize{
#'     \item\code{.g} makes positive values green, negative values red, and zero values black
#'     \item\code{.r} does the reverse, making positive values red
#' }
#'
#' @param x A numeric column of a data.frame or data.table
#'
#' @note For best usage, apply the color transformation with mutate() and then pass the mutated object
#'     through kable() with escape = F and format = 'html'. If kableExtra::kable_styling() is used
#'     then use protect_latex = F.
#'
#'
#' @name red_green_text.
NULL
#> NULL

#' @export
#' @rdname red_green_text.
red_green_text.g <- formattable::formatter("span",
                     style = x ~ formattable::style(color = ifelse(x > 0, "green",
                                                                   ifelse(x < 0, "red", "black"))))

#' @export
#' @rdname red_green_text.
red_green_text.r <- formattable::formatter("span",
                     style = x ~ formattable::style(color = ifelse(x > 0, "red",
                                                                   ifelse(x < 0, "green", "black"))))
