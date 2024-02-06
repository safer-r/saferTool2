test_that("slide() works correctly", {
  expect_error(slide(data = NULL, window.size = 5, step = 2, fun = length))

# Test case 2: Test 'window.size' argument
  expect_error(slide(data = 1:10, window.size = "5", step = 2, fun = length))

# Test case 3: Test 'step' argument
  expect_error(slide(data = 1:10, window.size = 5, step = "2", fun = length))

# Test case 4: Test 'fun' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = 123))

# Test case 5: Test 'args' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = length, args = 123))

# Test case 6: Test 'boundary' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = length, boundary = "invalid"))

# Test case 7: Test 'parall' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = length, parall = "not_logical"))

# Test case 8: Test 'thread.nb' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = length, parall = TRUE, thread.nb = "not_numeric"))

# Test case 9: Test 'print.count' argument
  # expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = length, print.count = "not_numeric"))

# Test case 10: Test 'res.path' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = length, res.path = "invalid_directory"))

# Test case 11: Test 'lib.path' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = length, lib.path = "invalid_directory"))

# Test case 12: Test 'verbose' argument
  # expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = length, verbose = "not_logical"))

# Test case 13: Test 'safer.path' argument
  # expect_error(slide(data = 1:10, window.size = 5, step = 2, fun = length, safer.path = "invalid_path"))

# Comprehensive test for all arguments
  expect_error(slide(data = NULL, window.size = "5", step = "2", fun = 123, args = 123, boundary = "invalid", parall = "not_logical", thread.nb = "not_numeric", print.count = "not_numeric", res.path = "invalid_directory", lib.path = "invalid_directory", verbose = "not_logical", safer.path = "invalid_path"))
})
