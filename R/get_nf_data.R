#' Retrieve data from the app_notification_interaction table
#'
#' This function retrieves data from the `app_notification_interaction`` table in a PostgreSQL database.
#' 
#' @param site The name of the PostgreSQL database (using DBI::dbConnect).
#' @param filter A logical value indicating whether to filter the UIC data frame (defaults to `FALSE`).
#' @param UIC_Tracker A data frame containing UIC data (if filter is `TRUE`). 
#' @param app_user_id A character string representing the name of the column containing app user IDs in `UIC_Tracker`.
#' @param country A character string representing the country for which to retrieve data (required if filter is `TRUE`).
#' @param study A character string representing the study for which to retrieve data (required if filter is `TRUE`).
#' 
#' @return Data frame containing notification data from postgres
#' @export
#' 
#' @examples #TODO
get_nf_data <- function(site, filter = FALSE, UIC_Tracker = NULL,
                         app_user_id = "app_user_id", country = "Tanzania", study) {
  if (filter){
    app_id <- UIC_Tracker %>% 
      dplyr::filter(Country == country, Study == study) %>% 
      dplyr::pull(YourParentAppCode)
    qry <- stringr::str_c("select * from app_notification_interaction where ", app_user_id, " in ('", paste0(app_id, collapse="', '"), "')")
    df <- get_postgres_data(site = site, qry = qry)
  } else {
    df <- get_postgres_data(site = site, name = "app_notification_interaction")
  }
  appdata_df <- list()
  for (i in 1:nrow(df)) {
    if (!is.na(df$notification_meta[i])) {
      appdata_df[[i]] <- data.frame(jsonlite::fromJSON(df$notification_meta[i]))
    }
    else {
      appdata_df[[i]] <- data.frame(i)
    }
  }
  appdata <- dplyr::bind_rows(appdata_df)
  plhdata <- dplyr::bind_cols(df, appdata)
  return(plhdata)
}
