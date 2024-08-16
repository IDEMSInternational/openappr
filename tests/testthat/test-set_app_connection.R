library(testthat)

test_that("set_app_connection establishes a connection", {
  set_app_connection(
    dbname = "vmc",
    host = "apps-server.idems.international",
    port = 5432,
    user = "vmc",
    password = "LSQkyYg5KzL747"
  )
  
  con <- get_app_connection()
  expect_s4_class(con, "PqConnection")
})

test_that("get_app_connection retrieves the connection", {
  con <- get_app_connection()
  expect_s4_class(con, "PqConnection")
})
