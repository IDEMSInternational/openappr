# Simple test to expand on later

library(RSQLite)
library(DBI)
library(postgresr)

# create some dummy data to read in when testing -----------
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "app_users", mtcars[1:10, ])

# test the postgres data
get_data <- get_postgres_data(site = con, name = "app_users")

test_that("Data imports from source", {
  testthat::expect_length(get_data, length(get_data))
})

# checking errors
test_that("check an error arises if app_users or nf isn't given", {
  expect_error(get_postgres_data(site = con, name = "hello"))
})
