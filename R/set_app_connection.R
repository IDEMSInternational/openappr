#' Set Application Database Connection
#'
#' Establishes a connection to a PostgreSQL database using provided credentials.
#' This function utilises the `DBI` and `RPostgres` packages to set up the connection.
#'
#' @param dbname The name of the database to connect to.
#' @param host The host name of the server where the database is located.
#' @param port The port number to connect through.
#' @param user The username for database authentication.
#' @param password The password for database authentication.
#' @param ... Additional arguments passed to `DBI::dbConnect`.
#'
#' @return A database connection object of class `DBIConnection`.
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
#' @seealso
#' \code{\link[DBI]{dbConnect}} for more details on the underlying connection function.
#' For additional information on database interfaces, see \url{https://dbi.r-dbi.org/}.
#' 
set_app_connection <- function(dbname, host, port, user, password, ...){
  app_con <- DBI::dbConnect(RPostgres::Postgres(),
                            dbname = dbname,
                            host = host,
                            port = port,
                            user = user,
                            password = password,
                            ...)
  pkg_env$app_con <- app_con 
}
