#' Get notification data from OpenAppBuilder
#'
#' @description This function retrieves data from the `app_notification_interaction` table in OpenAppBuilder and efficiently parses the `notification_meta` column from JSON format.
#' 
#' @param site The name of the PostgreSQL database connection (using `DBI::dbConnect` or `set_app_connection()`).
#' @param filter A logical value indicating whether to filter the data (defaults to `FALSE`).
#' @param filter_variable A character string representing the name of the column to filter if `filter == TRUE` and `filter_variable_value` is provided.
#' @param filter_variable_value A character string representing the value of the `filter_variable` column to filter if `filter == TRUE`.
#' 
#' @return A data frame containing notification interaction data from OpenAppBuilder, with the `notification_meta` column parsed into separate columns.
#' 
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
#' # Retrieve all notification data
#' data_all_nf <- get_nf_data()
#' 
#' # Retrieve data where 'app_user_id' is '3e68fcda-d4cd-400e-8b12-6ddfabced348' 
#' # or '223925c7-443a-411c-aa2a-a394f991dd52'
#' valid_ids <- c("3e68fcda-d4cd-400e-8b12-6ddfabced348",
#'                "223925c7-443a-411c-aa2a-a394f991dd52")
#' data_filtered_users <- get_nf_data(
#'   filter = TRUE,
#'   filter_variable = "app_user_id",
#'   filter_variable_value = valid_ids
#' )
get_nf_data <- function(site = get_app_connection(), filter = FALSE, filter_variable = NULL,
                        filter_variable_value = NULL) {
  
  df <- get_openapp_data(site = site,
                         name = "app_notification_interaction",
                         filter = filter,
                         filter_variable = filter_variable,
                         filter_variable_value = filter_variable_value)
  
  appdata_df <- purrr::map(df$notification_meta, jsonlite::fromJSON) %>% dplyr::bind_rows()
  return_data <- dplyr::bind_cols(df, appdata_df)
  
  return(return_data)
}
