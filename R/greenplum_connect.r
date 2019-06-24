#' Setup Greenplum Database Connection
#'
#' @description This function establishes a RPostgreSQL database connection.
#'
#' \itemize{
#'    \item\code{greenplum_connect()} creates a Greenplum connection object, \code{conn};
#'    \item\code{greenplum_disconnect()} disconnects \code{conn} and removes it from the global environment;
#'    \item\code{create_secret()} writes a password to 'C:/Users/<user>/Documents/Secrets/secret.rds',
#'         and creates a new directory if necessary
#'
#' }
#'
#' @param ask Logical, if \code{TRUE} (default) then RStudio will ask the user to input their database password,
#'      if \code{FALSE} then password will be read from secret.rds
#'
#' @note Database connection created with RPostgreSQL rather than RPostgres or RGreenplum as only RPostgreSQL can run multiple queries with
#'      one db call.
#'


#' @export
greenplum_connect <- function(ask = T){

  user <- Sys.info()[['login']]
  file <- sprintf('C:/Users/%s/Documents/Secrets/secret.rds', user)

  password <- if(ask){
    rstudioapi::askForPassword(sprintf("Database Password for %s:", user))
  } else {

    if (file.exists(file)) {
      readRDS(file)
    } else {stop("No secret file found; create secret file with fidelis::create_secret()")}

    }

  conn <<- RPostgreSQL::dbConnect(
    DBI::dbDriver("PostgreSQL"),
    dbname = 'fidprd',
    host = 'greenplum.fideliscare.org',
    port = 5432,
    user = user,
    password = password)

}

#' @export
#' @rdname greenplum_connect
greenplum_disconnect <- function(){

  DBI::dbDisconnect(conn = conn)
  rm(conn, envir = .GlobalEnv)

}

#' @export
#' @rdname greenplum_connect
create_secret <- function(){

  folder <- sprintf('C:/Users/%s/Documents/Secrets', Sys.info()[['login']])

  if(!dir.exists(folder)){

    dir.create(folder)

  }

  saveRDS(object = rstudioapi::askForPassword(),
          file = sprintf('%s/secret.rds', folder))

}
