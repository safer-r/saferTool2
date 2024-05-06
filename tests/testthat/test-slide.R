test_that("slide() works correctly", {
  expect_error(slide(data = NULL, window.size = 5, step = 2, FUN  = length))
  expect_error(slide(data = c("a", "b", "c"), window.size = 5, step = 2, FUN  = length))

# Test case 2: Test 'window.size' argument
  expect_error(slide(data = 1:10, window.size = "5", step = 2, FUN  = length))
  expect_error(slide(data = 1:10, window.size = "t", step = 2, FUN  = length))

# Test case 3: Test 'step' argument
  expect_error(slide(data = 1:10, window.size = 5, step = "2", FUN  = length))
  expect_error(slide(data = 1:10, window.size = 5, step = 6, FUN  = length))

# Test case 4: Test 'FUN ' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = 123))

# Test case 5: Test 'args' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, args = 123))
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = mean, args = "NULL"))

# Test case 6: Test 'boundary' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, boundary = "invalid"))
  expect_no_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, boundary = "right"))

# Test case 7: Test 'parall' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, parall = "not_logical"))
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, parall = "END"))


# Test case 8: Test 'thread.nb' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, parall = TRUE, thread.nb = "not_numeric"))
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, parall = TRUE, thread.nb = "3"))

# Test case 9: Test 'print.count' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, print.count = "not_numeric"))
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, print.count = "10"))

# Test case 10: Test 'res.path' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, res.path = "invalid_directory"))
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, res.path = "/some/path"))

# Test case 11: Test 'lib.path' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, lib.path = "invalid_directory"))
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, lib.path = "/some/path"))

# Test case 12: Test 'verbose' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, verbose = "not_logical"))
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, verbose = 123))

# Test case 13: Test 'safer_check' argument
  expect_error(slide(data = 1:10, window.size = 5, step = 2, FUN  = length, verbose = "not_logical", safer_check = TRUE))

# Comprehensive test for all arguments
  expect_error(slide(data = NULL, window.size = "5", step = "2", FUN  = 123, args = 123, boundary = "invalid", parall = "not_logical", thread.nb = "not_numeric", print.count = "not_numeric", res.path = "invalid_directory", lib.path = "invalid_directory", verbose = "not_logical", safer_check = TRUE))
})
