#' Execute Dynamic PostgreSQL Queries
#'
#' @description The \code{query()} function sends a query with dynamic inputs to a PostgreSQL database, allowing loops and automated reporting.
#'    The \code{madlib()} function just prints the query to console after replacing dynamic inputs, useful for debugging.
#'
#' @param query A character string with dynamic inputs surrounded by "\%" (i.e. \code{\%<variable>\%})
#' @param ... A named series of objects to insert into the query, these CAN be reused.
#'    Objects of length 1 will be left as is, whereas objects of length > 1 will be surrounded with brackets.
#'    Objects of type character will be surrounded by single quotations and objects of type numeric will be left as is.
#'
#' @note Ensure there is a connection obejct 'conn' in your environment.
#'
#' @export
#'
#' @examples
#' query(query = "select * from table where place in %places% and year in %years%;",
#'        places = c('NYC', 'Vancouver', 'Hamilton'),
#'        years = c(2019, 2018, 2017))
#'
#'
#' query <- "
#' select *
#' from table
#' where place in %places%
#' and year in %years%
#' ;
#' "
#' places <- c('NYC', 'Vancouver', 'Hamilton')
#' years <- c(2019, 2018, 2017)
#'
#' query(query = query,
#'        places = places,
#'        years = years)
#'
#' madlib(query = "select * from table where place in %places% and year in %years%;",
#'        places = c('NYC', 'Vancouver', 'Hamilton'),
#'        years = c(2019, 2018, 2017)) # returns a character object

query <- function(query, ...){

  dots <- lapply(list(...),
                 FUN = function(x){

                   if(length(x)==1){

                     if(is.character(x)){paste0("'", x, "'")}
                     else if(is.numeric(x)){x}
                     else {stop('Type Issue')}

                   }else if(length(x) > 1){

                     if(is.character(x)){paste0("('", paste(x, collapse = "','"), "')")}
                     else if(is.numeric(x)){paste0("(", paste(x, collapse = ","), ")")}
                     else {stop('Type Issue')}

                   } else {stop('Length Issue')}

                 })


  for(i in names(dots)){

    query <- gsub(paste0('%', i, '%'), dots[[i]], query)

  }

  query <- gsub("\n+|\t+|  +", " ", query)

  RPostgreSQL::dbGetQuery(conn, query)

}



#' @export
#' @rdname query
madlib <- function(query, ...){

  dots <- lapply(list(...),
                 FUN = function(x){

                   if(length(x)==1){

                     if(is.character(x)){paste0("'", x, "'")}
                     else if(is.numeric(x)){x}
                     else {stop('Type Issue')}

                   }else if(length(x) > 1){

                     if(is.character(x)){paste0("('", paste(x, collapse = "','"), "')")}
                     else if(is.numeric(x)){paste0("(", paste(x, collapse = ","), ")")}
                     else {stop('Type Issue')}

                   } else {stop('Length Issue')}

                 })


  for(i in names(dots)){

    query <- gsub(paste0('%', i, '%'), dots[[i]], query)

  }

  cat(query)

}


