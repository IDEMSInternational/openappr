#' Get data from Postgres
#' 
#' @description Call ParentApp data from Postgres
#'
#' @param site Connection to database to get original data (using DBI::dbConnect).
#' @param name Data to call from connection. Default `"app_users"`, but also takes `"app_notification_interaction"`. Only if `qry = NULL`.
#' @param qry A character string containing SQL. Note that to call the named data (e.g., `app_users`) with `qry`, use `qry = "select * from app_users`.
#'
#' @return Data from Postgres
#' @export
#'
#' @examples # TODO
get_postgres_data <- function (site, name = c("app_users", "app_notification_interaction"), qry = NULL){
  name <- match.arg(name)
  if (is.null(qry)){
    df <- DBI::dbReadTable(conn = site, name = name)
  } else {
    df <- DBI::dbGetQuery(site, qry)
  }
  return(df)
}
