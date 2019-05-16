
#' Red/Green Color Bars
#'
#' @description These functions change the color bar of a cell of a kable obejct. The length of the color bar is determined
#'     for each tile by the absolute percentage of the value divided by the absolute max.
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
#' @name red_green_bars.
NULL
#> NULL

#' @export
#' @rdname red_green_bars.
red_green_bars.g <- formattable::formatter("span",
                     style = x ~ formattable::style(`background-color` = ifelse(x >= 0, "lightgreen", "lightpink"),
                                                    direction = "rtl",
                                                    display = "inline-block",
                                                    `border-radius` = "4px",
                                                    `padding-right` = "2px",
                                                    width = abs(formattable::percent(x/max(abs(x))))
                     ))

#' @export
#' @rdname red_green_bars.
red_green_bars.r <- formattable::formatter("span",
                     style = x ~ formattable::style(`background-color` = ifelse(x < 0, "lightgreen", "lightpink"),
                                                    direction = "rtl",
                                                    display = "inline-block",
                                                    `border-radius` = "4px",
                                                    `padding-right` = "2px",
                                                    width = abs(formattable::percent(x/max(abs(x))))
                     ))
