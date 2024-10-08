% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nf_data.R
\name{get_nf_data}
\alias{get_nf_data}
\title{Get notification data from OpenAppBuilder}
\usage{
get_nf_data(
  site = get_app_connection(),
  filter = FALSE,
  filter_variable = NULL,
  filter_variable_value = NULL
)
}
\arguments{
\item{site}{The name of the PostgreSQL database connection (using \code{DBI::dbConnect} or \code{set_app_connection()}).}

\item{filter}{A logical value indicating whether to filter the data (defaults to \code{FALSE}).}

\item{filter_variable}{A character string representing the name of the column to filter if \code{filter == TRUE} and \code{filter_variable_value} is provided.}

\item{filter_variable_value}{A character string representing the value of the \code{filter_variable} column to filter if \code{filter == TRUE}.}
}
\value{
A data frame containing notification interaction data from OpenAppBuilder, with the \code{notification_meta} column parsed into separate columns.
}
\description{
This function retrieves data from the \code{app_notification_interaction} table in OpenAppBuilder and efficiently parses the \code{notification_meta} column from JSON format.
}
\examples{
# First we need to set an app connection
set_app_connection(
  dbname = "vmc",
  host = "apps-server.idems.international",
  port = 5432,
  user = "vmc",
  password = "LSQkyYg5KzL747"
)

# Retrieve all notification data
data_all_nf <- get_nf_data()

# Retrieve data where 'app_user_id' is '3e68fcda-d4cd-400e-8b12-6ddfabced348' 
# or '223925c7-443a-411c-aa2a-a394f991dd52'
valid_ids <- c("3e68fcda-d4cd-400e-8b12-6ddfabced348",
               "223925c7-443a-411c-aa2a-a394f991dd52")
data_filtered_users <- get_nf_data(
  filter = TRUE,
  filter_variable = "app_user_id",
  filter_variable_value = valid_ids
)
}
