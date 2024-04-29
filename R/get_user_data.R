#' Get user data from postgres site
#' 
#' @description Retrieves data from the `app_users` table in the specified PostgreSQL database, and converts the contact_fields column to a data frame using jsonlite::fromJSON().
#' If filter is TRUE, the function further filters the data to include only rows where the Country column matches country and the Study column matches study.
#'
#' @param site The name of the PostgreSQL database (using `DBI::dbConnect` or `set_app_connection()`).
#' @param filter A logical value indicating whether to filter data (defaults to `FALSE`).
#' @param country A character string representing the country for which to retrieve data (required if filter is `TRUE`).
#' @param study A character string representing the study for which to retrieve data (required if filter is `TRUE`).
#' @param UIC_Tracker A data frame containing UIC data (if `include_UIC_data = TRUE`). 
#' @param app_user_id A character string representing the name of the column containing app user IDs in `UIC_Tracker`. Default `NULL`.
#' @param filter_variable A character string representing the name of the column to filter to if `filter == TRUE` and `app_user_id` is `NULL`.
#' @param filter_variable_value A character string representing the value of the column `filter_variable` to filter to if `filter == TRUE` and `app_user_id` is `NULL`.
#' @param include_UIC_data A logical value indicating whether to include UIC data (defaults to `FALSE`).
#' @param merge_check A logical value indicating whether to merge check data (defaults to `TRUE`).
#' @param join_UIC A character string representing the name of the column used to join UIC_Tracker and the main data frame.
#' @param max_dist An integer specifying the maximum distance for UIC data (defaults to 5).
#' @param date_from An optional character string representing the date from which to retrieve data.
#' @param date_to An optional character string representing the date to which to retrieve data.
#' @param format_date A character string specifying the format of the date strings (defaults to "%Y-%m-%d").
#' @param tzone_date A character string specifying the time zone for the date strings (defaults to "UTC").
#' System-specific (see \code{\link[base]{as.POSIXlt}}), but "" is the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
#' Invalid values are most commonly treated as UTC, on some platforms with a warning.

#' @return Data frame containing user data from postgres
#' @export
#' @importFrom utils capture.output
#'
#' @examples # TODO
get_user_data <- function (site = get_app_connection(), filter = FALSE, country = "Tanzania", study, UIC_Tracker, app_user_id = NULL,
                           filter_variable = NULL, filter_variable_value = NULL,
                           include_UIC_data = FALSE, merge_check = TRUE, join_UIC = "UIC", max_dist = 5,
                           date_from = NULL, date_to = NULL, format_date = "%Y-%m-%d",tzone_date = "UTC") {
  
  if (filter){
    if (!is.null(app_user_id)){
      app_id <- UIC_Tracker %>% 
        dplyr::filter(Country == country, Study == study) %>% 
        dplyr::pull(YourParentAppCode)
      qry <- stringr::str_c("select * from app_users where ", app_user_id, " in ('", paste0(app_id, collapse="', '"), "')")
    } else {
      qry <- stringr::str_c("select * from app_users where ", filter_variable, " in ('", paste0(filter_variable_value, collapse = "', '"), "')")
    }
    
    df <- get_postgres_data(site = site,  name = "app_users", qry = qry)
  } else {
    df <- get_postgres_data(site = site, name = "app_users")
  }
  appdata_df <- purrr::map(df$contact_fields, jsonlite::fromJSON) %>% dplyr::bind_rows()
  plhdata <- dplyr::bind_cols(df, appdata_df)
  
  if (include_UIC_data) {
    plhdata_org_fuzzy <- fuzzyjoin::stringdist_full_join(plhdata, UIC_Tracker, 
                                              by = c(app_user_id = join_UIC), 
                                              max_dist = max_dist)
    plhdata_org_fuzzy_comp <- plhdata_org_fuzzy %>% 
      dplyr::filter(stringr::str_detect(!!rlang::sym(join_UIC), "\\w+")) %>% 
      dplyr::select(app_user_id, !!rlang::sym(join_UIC))
    
    return_data <- dplyr::if_else(merge_check && 
                             yesno::yesno2("Fuzzy matches are:\n", 
                                           paste0(capture.output(plhdata_org_fuzzy_comp), 
                                                  collapse = "\n"), 
                                           "\nDo you want to merge these changes in?"), 
                           plhdata_org_fuzzy, 
                           {warning("merging in fuzzy matches:\n", 
                                    paste0(capture.output(plhdata_org_fuzzy_comp %>% 
                                                            dplyr::filter(stats::complete.cases(app_user_id))), 
                                           collapse = "\n"))
                             plhdata_org_fuzzy})
  } else {
    return_data <- plhdata
  }
  
  if (!missing(date_from)) {
    return_data <- return_data %>% dplyr::filter(as.POSIXct(date_from, 
                                                            format = format_date, tzone = tzone_date) < as.POSIXct(return_data$createdAt, 
                                                                                                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  if (!missing(date_to)) {
    return_data <- return_data %>% dplyr::filter(as.POSIXct(date_to, 
                                                            format = format_date, tzone = tzone_date) > as.POSIXct(return_data$createdAt, 
                                                                                                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  return(return_data)
}
