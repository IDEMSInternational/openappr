#' Get data from OpenAppBuilder
#' 
#' @description Retrieves data from OpenAppBuilder by querying the specified PostgreSQL database. The function can either retrieve all data from a specific table (e.g., `app_users` or `app_notification_interaction`) or execute a custom SQL query provided by the user.
#'
#' @param site The name of the PostgreSQL database connection (using `DBI::dbConnect` or `set_app_connection()`).
#' @param name A character string specifying the table to retrieve data from. Default is `"app_users"`, but `"app_notification_interaction"` can also be specified. This parameter is ignored if `qry` is provided.
#' @param filter A logical value indicating whether to filter the data based on a specific column (defaults to `FALSE`).
#' @param filter_variable A character string representing the name of the column to filter if `filter == TRUE`.
#' @param filter_variable_value A character string or vector representing the value(s) of the `filter_variable` column to filter if `filter == TRUE`.
#' @param qry An optional character string containing an SQL query. If provided, this query overrides the `name` parameter and allows for custom SQL to be executed.
#'
#' @return A data frame containing the retrieved data from the specified PostgreSQL table or the result of the executed SQL query.
#' @export
#' 
#' @examples
#' # First we need to set an app connection
#' set_app_connection(
#'   dbname = "vmc",
#'   host = "apps-server.idems.international",
#'   port = 5432,
#'   user = "vmc",
#'   password = "LSQkyYg5KzL747"
#' )
#' 
#' # Retrieve all data from the 'app_users' table
#' data_all_users <- get_openapp_data(name = "app_users")
#' 
#' # Retrieve filtered data from the 'app_users' table where 'app_user_id' is
#' # a specified ID.
#' valid_ids <- c("3e68fcda-d4cd-400e-8b12-6ddfabced348",
#'                "223925c7-443a-411c-aa2a-a394f991dd52")
#' data_filtered_users <- get_openapp_data(
#'   name = "app_users",
#'   filter = TRUE,
#'   filter_variable = "app_user_id",
#'   filter_variable_value = valid_ids
#' )
#' 
#' # Retrieve data using a custom SQL query
#' custom_query_data <- get_openapp_data(
#'   qry = "SELECT * FROM app_users WHERE app_version = '0.16.33'"
#' )
get_openapp_data <- function (site = get_app_connection(),
                              name = c("app_users", "app_notification_interaction"),
                              filter = FALSE,
                              filter_variable = NULL,
                              filter_variable_value = NULL,
                              qry = NULL){
  name <- match.arg(name)
  if (filter){
    if (!is.null(filter_variable)){
      qry <- stringr::str_c("select * from ", name, " where ", filter_variable, " in ('", paste0(filter_variable_value, collapse = "', '"), "')")
    } else {
      qry <- qry
    }
  } else {
    qry <- NULL
  }
  
  if (is.null(qry)){
    df <- DBI::dbReadTable(conn = site, name = name)
  } else {
    df <- DBI::dbGetQuery(site, name = name, qry)
  }
  
  return(df)
}