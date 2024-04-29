#' Get the app connection from the environment
#' @description Call the app connection. The connection is set in the function `set_app_connection`. 
#'
#' @return returns a the app connection to the app data.
#' @export
get_app_connection <- function() {
  get("app_con", envir = pkg_env)
}
