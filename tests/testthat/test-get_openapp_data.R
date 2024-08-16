library(testthat)

set_app_connection(
  dbname = "vmc",
  host = "apps-server.idems.international",
  port = 5432,
  user = "vmc",
  password = "LSQkyYg5KzL747"
)

valid_ids <- c("3e68fcda-d4cd-400e-8b12-6ddfabced348", "223925c7-443a-411c-aa2a-a394f991dd52")

test_that("get_openapp_data retrieves data from the database", {
  data <- get_openapp_data()
  expect_s3_class(data, "data.frame")
  
  filtered_data <- get_openapp_data(
    name = "app_users",
    filter = TRUE,
    filter_variable = "app_user_id",
    filter_variable_value = valid_ids
  )
  expect_s3_class(filtered_data, "data.frame")
})

test_that("get_openapp_data retrieves notification data from the database", {
  filtered_data <- get_openapp_data(
    name = "app_notification_interaction",
    filter = TRUE,
    filter_variable = "app_user_id",
    filter_variable_value = valid_ids
  )
  expect_s3_class(filtered_data, "data.frame")
})

test_that("get_openapp_data retrieves data from the database for custom query", {
  custom_query_data <- get_openapp_data(
    qry = "SELECT * FROM app_users WHERE app_version = '0.16.33'"
  )
  expect_s3_class(custom_query_data, "data.frame")
})


test_that("get_user_data retrieves and processes user data", {
  data <- get_user_data()
  expect_s3_class(data, "data.frame")
  
  filtered_data <- get_user_data(
    filter = TRUE,
    filter_variable = "app_user_id",
    filter_variable_value = valid_ids
  )
  expect_s3_class(filtered_data, "data.frame")
  
  date_filtered_data <- get_user_data(
    filter = TRUE,
    filter_variable = "app_user_id",
    filter_variable_value = valid_ids,
    date_from = "2023-01-01",
    date_to = "2024-08-18"
  )
  expect_s3_class(date_filtered_data, "data.frame")
})

test_that("get_nf_data retrieves and processes notification data", {
  data <- get_nf_data()
  expect_s3_class(data, "data.frame")
  
  filtered_data <- get_nf_data(
    filter = TRUE,
    filter_variable = "app_user_id",
    filter_variable_value = valid_ids
  )
  expect_s3_class(filtered_data, "data.frame")
})
