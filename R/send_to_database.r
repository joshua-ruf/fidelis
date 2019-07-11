#' Save a data.frame to Greenplum
#'
#' This function sends a data.frame to Greenplum using PostgreSQL's "INSERT INTO" function.
#'
#' @param data The data.frame to upload; only supports numeric, character, and date column types.
#' @param name A character name to call the table in Greenplum. If the name begins with "sandbox." then
#'      a table will be created in the sandbox, otherwise a temp table will be created.
#'
#' @note Make sure there is a connection object 'conn' in your envrionment!
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

  data <- as.data.frame(data)

  data[names(types[types=='VARCHAR'])] <- lapply(data[names(types[types=='VARCHAR'])],
                                                 function(x){gsub("'", "''", x)})

  RPostgreSQL::dbSendQuery(conn, paste("DROP TABLE IF EXISTS", name, ";"))

  RPostgreSQL::dbSendQuery(conn,
                           paste("CREATE",
                                 if(grepl("^sandbox\\.", name)){""} else{"TEMP"},
                                 "TABLE",
                                 name,
                                 " (",
                                 paste(paste(names(types), types), collapse = ', '),
                                 ");"))

  RPostgreSQL::dbSendQuery(conn,
                           paste("INSERT INTO ",
                                 name,
                                 " (",
                                 paste(names(data), collapse = ', '),
                                 ") VALUES ",
                                 paste(apply(data, 1, function(x){paste0("('", paste(x, collapse = "','"), "')")}), collapse = ',')))

}
