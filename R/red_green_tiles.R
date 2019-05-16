
#' Red/Green Tile Formatters
#'
#' @description These functions shade the background of a kable cell either red or green, preserving the distance from zero which remains white.
#' \itemize{
#'     \item\code{.g} makes positive values green and negative values red
#'     \item\code{.r} makes negative values green and positive values red
#'     \item\code{w2rg()} is the function that creates the html colors to be passed to \code{red_green_tiles.}
#'    }
#'
#' @param x A numeric column of a data.frame or data.table
#' @param green_up Logical, default is TRUE where green values are positive. Though a user is unlikely to ever use w2rg by itself
#'
#' @note For best usage, apply the color transformation with mutate() and then pass the mutated object
#'     through kable() with escape = F and format = 'html'. If kableExtra::kable_styling() is used
#'     then use protect_latex = F.
#'
#'
#' @name red_green_tiles.
NULL
#> NULL

#' @export
#' @rdname red_green_tiles.
red_green_tiles.g <- formattable::formatter('span',
            style = x ~ formattable::style(display = 'block',
                              padding = '0 4px',
                              'border-radius' = '4px',
                              'background-color' = fidelis::w2rg(x)))

#' @export
#' @rdname red_green_tiles.
red_green_tiles.r <- formattable::formatter('span',
            style = x ~ formattable::style(display = 'block',
                              padding = '0 4px',
                              'border-radius' = '4px',
                              'background-color' = fidelis::w2rg(x, green_up = F)))

#' @export
#' @rdname red_green_tiles.
w2rg <- function(x, green_up = T){

    width <- round(x/max(abs(x))*100)

    d <- data.table(green_upT = -100:100,
                    green_upF = 100:-100,
                    color = c(formattable::csscolor(formattable::gradient(1:176, 'red', 'white')[,76:176]),
                              formattable::csscolor(formattable::gradient(1:176, 'white', 'green')[,2:101])))


    if (green_up) {

      setkey(d, green_upT)

      d[.(width), color]

    } else {

      setkey(d, green_upF)

      d[.(width), color]

    }

}
