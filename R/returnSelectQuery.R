#' Returns the result of a select query
#'
#' @param query The query to execute.
#' @param dbname The database name.
#' @param password The password to the database.
#' @param host The hostname.
#' @return The data.
#' @examples
#' 
returnSelectQuery <- function(query, dbname="mydb", username="root", password="root", host="localhost") {
  con <- dbConnect(drv=MySQL(), dbname=dbname, username=username, password=password, host=host)
  rs <- dbSendQuery(con, query)
  data <- fetch(rs, n=-1)
  huh <- dbHasCompleted(rs)
  dbClearResult(rs)
  dbDisconnect(con)
  return(data)
}