#' Get data from Postgres
#' 
#' @description Call ParentApp data from Postgres
#'
#' @param site Connection to database to get original data (using DBI::dbConnect).
#' @param name Data to call from connection. Default `"app_users"`, but also takes `"app_notification_interaction"`.
#'
#' @return Data from Postgres
#' @export
#'
#' @examples # TODO
get_postgres_data <- function(site, name = c("app_users", "app_notification_interaction")){
  name <- match.arg(name)
  df <- DBI::dbReadTable(conn = site,
                         name = name)
  return(df)
}
