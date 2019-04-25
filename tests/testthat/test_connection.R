context("Opening and closing connections")

test_that("Server can be started and stopped", {
  r2r::start_server_locally()
  expect_true(stop_server())
})


context("Evaluating expression")

test_that("Expression can be evaluated", {
  r2r::start_server_locally()
  expect_equal(r2r::eval_remote(1+1), 2)
  expect_error(r2r::eval_remote(1+a))
  expect_equal(r2r::eval_remote(1+a, data = list(a = 2)), 3)
  expect_true(stop_server())
})


test_that("Functions can be called", {
  r2r::start_server_locally()
  expect_equal(r2r::do.call_remote("sum", args_remote = list(a = 1, b = 1)), 2)
  expect_equal(r2r::do.call_remote(sum, args_remote = list(a = 1, b = 1), quote = TRUE), 2)
  expect_error(r2r::do.call_remote("sum", args_remote = list(a = 1, b = "A"), quote = FALSE))
  expect_error(r2r::do.call_remote(sum, args_remote = list(a = 1, b = "A"), quote = TRUE))
  expect_true(r2r::stop_server())
})
