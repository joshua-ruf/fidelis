#' Save data.frame to Greenplum
#'
#' Takes a data.frame in R's working memory and creates a sandbox'ed version of it in Greenplum.
#' Make sure date columns have been converted to dates with as.Date() and that columns are the desired type.
#'
#' @param x The data.frame to upload. Only supports numeric, character, and date column types.
#' @param name A character name to call the table in Greenplum.
#' @return An object called 'sandbox.<name>' in greenplum
#' @export
#'

send_to_database <- function(x, name){

`%do%` <- foreach::`%do%`
`%dopar%` <- foreach::`%dopar%`
require(data.table)
doParallel::registerDoParallel(4)


cols <- names(x)

ff <- function(b){
  if (lubridate::is.Date(b)){'DATE'}
  else if (is.numeric(b)){'NUMERIC'}
  else {'VARCHAR'}
}

types <- x[,lapply(.SD, ff), .SDcols = cols]

date_cols <- cols[grep('DATE', types)]

if (idenitical(date_cols, character(0))){x <- x}
else {
x <- x[, (date_cols) := lapply(.SD, fidelis::usdate), .SDcols = date_cols]
}

col_type <- foreach::foreach(i = cols, j = types, .combine = function(a,b){paste(a,b, sep = ",")}) %do% paste(i,j)

text0 <- foreach::foreach(i = 1:nrow(x)) %dopar% paste0("('", paste(x[i,], collapse = "','"), "'),")

text <- foreach::foreach(i = text0, .combine = 'paste0') %dopar% i
text <- substr(text, 1, nchar(text)-1)


create_query <- paste0('create temp table ',
                name,
                ' (',
                col_type,
                ');')

insert_query <- paste0('insert into ',
                       name,
                       ' (',
                       paste(cols, collapse = ","),
                       ') values',
                       text,
                       ';'
                       )

sandbox_query <- paste0('drop table if exists sandbox.',
                        name,
                        '; ',
                        'create table sandbox.',
                        name,
                        ' as select * from ',
                        name,
                        ';'
                        )

fidelis::query(paste(create_query,
            insert_query,
            sandbox_query))

}
