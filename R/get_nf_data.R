#' Get notification data from OpenAppBuilder
#'
#' @description This function retrieves data from the `app_notification_interaction` table in OpenAppBuilder and efficiently parses the `notification_meta` column from JSON format.
#'
#' @param site The name of the PostgreSQL database connection (using `DBI::dbConnect` or `set_app_connection()`).
#' @param filter A logical value indicating whether to filter the data (defaults to `FALSE`).
#' @param filter_variable A character string representing the name of the column to filter if `filter == TRUE` and `filter_variable_value` is provided.
#' @param filter_variable_value A character string representing the value of the `filter_variable` column to filter if `filter == TRUE`.
#' 
#' @return A data frame containing notification interaction data from the PostgreSQL database, with the `notification_meta` column parsed into separate columns.
#' @export
#' 
#' @examples
#' # TODO
#' 
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
