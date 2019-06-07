#' Save data.frame to Greenplum
#'
#' This function saves an R data.frame to a satabase using PostgreSQL's "INSERT INTO" function.
#'
#' @param data The data.frame to upload. Only supports numeric, character, and date column types.
#' @param name A character name to call the table in Greenplum.
#'
#' @export
#'

send_to_database <- function(data, name){

  types <- sapply(data,
                  function(b){
                    if (class(b)=='Date'){"DATE"}
                    else if (is.numeric(b)){"NUMERIC"}
                    else {"VARCHAR"}
                  })

  RPostgreSQL::dbSendQuery(conn, paste("DROP TABLE IF EXISTS", name, ";"))

  RPostgreSQL::dbSendQuery(conn,
                           paste("CREATE TABLE",
                                 name,
                                 " (",
                                 paste(paste(names(types), types, sep = ' '), collapse = ', '),
                                 ");"))

  RPostgreSQL::dbSendQuery(conn,
                           paste("INSERT INTO ",
                                 name,
                                 " (",
                                 paste(names(data), collapse = ', '),
                                 ") VALUES ",
                                 paste(apply(data, 1, function(x){paste0("('", paste(x, collapse = "','"), "')")}), collapse = ',')))

}
