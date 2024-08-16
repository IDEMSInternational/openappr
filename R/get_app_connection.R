#' Get the app connection from the environment
#' @description Call the app connection. The connection is set in the function `set_app_connection`. 
#'
#' @return returns a the app connection to the app data.
#' @export
#' 
#' @examples
#' # Establish a connection to the PostgreSQL database
#' set_app_connection(
#'   dbname = "vmc",
#'   host = "apps-server.idems.international",
#'   port = 5432,
#'   user = "vmc",
#'   password = "LSQkyYg5KzL747"
#' )
#' 
#' get_app_connection()
#' 
get_app_connection <- function() {
  get("app_con", envir = pkg_env)
}
