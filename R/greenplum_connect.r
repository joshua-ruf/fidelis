#' Greenplum Database Connection
#'
#' greenplum_connect() asks for a password to connect to the Greenplum Database, and creates \code{conn} the connection object.
#' greenplum_disconnect() disconnects \code{conn} as well as removes it from the environment
#'
#' @param ask Logical, if TRUE (default) then rstuidioapi::askForPassword() will be called, otherwise uses secret.json file if setup
#'
#' @return A PostgreSQL database connection


#' @export
greenplum_connect <- function(ask = T){

  user <- Sys.info()['login']
  password <- if(ask){
    rstudioapi::askForPassword(sprintf("Password for %s:", user))
  } else {

    if (file.exists('./Secrets/secret.json')) {
      jsonlite::read_json('./Secrets/secret.json', simplifyVector = T)
    } else {stop("Can't find secret file")}

    }

  conn <<- RPostgreSQL::dbConnect(
    DBI::dbDriver('PostgreSQL'),
    dbname = 'fidprd',
    host = 'greenplum.fideliscare.org',
    port = 5432,
    user = user,
    password = password)

}

#' @export
#' @rdname greenplum_connect
greenplum_disconnect <- function(){

  RPostgreSQL::dbDisconnect(conn = conn)
  rm(conn, envir = .GlobalEnv)

}
