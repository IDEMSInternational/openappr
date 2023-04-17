#' Get notification data from Postgres site
#'
#' @param site Connection to database to get original data (using DBI::dbConnect).
#' @param UIC_Tracker If `include_UIC_data = TRUE`, UIC tracker data. Required if `filter == TRUE`.
#' @param app_user_id Default "app_user_id". If `include_UIC_data = TRUE`, variable in the postgres data to merge the UIC data by.
#' @param filter logical. Default `FALSE`. Whether to filter UIC tracker data by country and study.
#' @param country character. Only if `filter == TRUE`. The country to filter the UIC tracker data to.
#' @param study character. Only if `filter == TRUE`. The study to filter the UIC tracker data to.
#'
#' @return Notification data from Postgres
#' @export
#'
#' @examples #TODO
get_nf_data <- function(site, UIC_Tracker = NULL, filter = FALSE,
                        app_user_id = "app_user_id",
                        country = "Tanzania", study) {
  if (filter){
    app_id <- (UIC_Tracker %>% dplyr::filter(Country == country) %>% dplyr::filter(Study == study))$YourParentAppCode
    qry <- sprintf(paste0("select * from app_notification_interaction where ", app_user_id, " in (%s)"),
                   paste0("'", rep(app_id), "'", collapse=","))
    df <- get_postgres_data(site = site, qry = qry)
  } else {
    df <- get_postgres_data(site = site, name = "app_notification_interaction")
  }
  appdata_df <- list()
  for (i in 1:nrow(df)) {
    if (!is.na(df$notification_meta[i])){
      appdata_df[[i]] <- data.frame(jsonlite::fromJSON(df$notification_meta[i]))
    } else {
      appdata_df[[i]] <- data.frame(i)
    }
  }
  # combine the list into a data frame 
  appdata <- plyr::ldply(appdata_df)
  # bind into main data frame
  plhdata <- dplyr::bind_cols(df, appdata) %>% dplyr::select(-c("i"))
  return(plhdata)
}
