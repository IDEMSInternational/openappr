#' Get user data from OpenAppBuilder
#' 
#' @description Retrieves data from the `app_users` table in OpenAppBuilder, and efficiently converts the `contact_fields` column from JSON format to a data frame.
#' If `filter` is `TRUE`, the function further filters the data to include only rows where the specified `filter_variable` column matches `filter_variable_value`.
#'
#' @param site The name of the PostgreSQL database connection (using `DBI::dbConnect` or `set_app_connection()`).
#' @param filter A logical value indicating whether to filter data (defaults to `FALSE`).
#' @param filter_variable A character string representing the name of the column to filter if `filter == TRUE` and `filter_variable_value` is provided.
#' @param filter_variable_value A character string representing the value of the `filter_variable` column to filter if `filter == TRUE`.
#' @param date_from An optional character string representing the date from which to retrieve data.
#' @param date_to An optional character string representing the date to which to retrieve data.
#' @param format_date A character string specifying the format of the date strings (defaults to "%Y-%m-%d").
#' @param tzone_date A character string specifying the time zone for the date strings (defaults to "UTC").
#' System-specific (see \code{\link[base]{as.POSIXlt}}), but "" uses the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
#' Invalid values are most commonly treated as UTC, on some platforms with a warning.
#'
#' @return A data frame containing user data from the PostgreSQL database, with the `contact_fields` column parsed into separate columns.
#' @export
#' @importFrom utils capture.output
#'
#' @examples
#' # TODO
get_user_data <- function(site = get_app_connection(),
                           filter = FALSE,
                           filter_variable = NULL,
                           filter_variable_value = NULL,
                           date_from = NULL,
                           date_to = NULL,
                           format_date = "%Y-%m-%d",
                           tzone_date = "UTC") {
  
  df <- get_openapp_data(site = site,
                         name = "app_users",
                         filter = filter,
                         filter_variable = filter_variable,
                         filter_variable_value = filter_variable_value)
  
  appdata_df <- purrr::map(df$contact_fields, jsonlite::fromJSON) %>% dplyr::bind_rows()
  return_data <- dplyr::bind_cols(df, appdata_df)
  
  if (!missing(date_from)){
    return_data <- return_data %>%
      dplyr::filter(as.POSIXct(date_from, format = format_date, tzone = tzone_date) <
                      as.POSIXct(return_data$createdAt, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  if (!missing(date_to)) {
    return_data <- return_data %>%
      dplyr::filter(as.POSIXct(date_to, format = format_date, tzone = tzone_date) >
                      as.POSIXct(return_data$createdAt, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  return(return_data)
}
