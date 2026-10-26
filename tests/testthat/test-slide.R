test_that("slide() works correctly", {
  expect_error(slide(data = NULL, window_size = 5, step = 2, FUN  = length))
  expect_error(slide(data = c("a", "b", "c"), window_size = 5, step = 2, FUN  = length))

# Test case 2: Test 'window_size' argument
  expect_error(slide(data = 1:10, window_size = "5", step = 2, FUN  = length))
  expect_error(slide(data = 1:10, window_size = "t", step = 2, FUN  = length))

# Test case 3: Test 'step' argument
  expect_error(slide(data = 1:10, window_size = 5, step = "2", FUN  = length))
  expect_error(slide(data = 1:10, window_size = 5, step = 6, FUN  = length))

# Test case 4: Test 'FUN ' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = 123))

# Test case 5: Test 'args' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, args = 123))
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = mean, args = "NULL"))

# Test case 6: Test 'boundary' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, boundary = "invalid"))
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, boundary = "right"))

# Test case 7: Test 'parall' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, parall = "not_logical"))
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, parall = "END"))


# Test case 8: Test 'thread_nb' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, parall = TRUE, thread_nb = "not_numeric"))
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, parall = TRUE, thread_nb = "3"))

# Test case 9: Test 'print_count' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, print_count = "not_numeric"))
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, print_count = "10"))

# Test case 10: Test 'res_path' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, res_path = "invalid_directory"))
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, res_path = "/some/path"))

# Test case 11: Test 'lib_path' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, lib_path = "invalid_directory"))
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, lib_path = "/some/path"))

# Test case 12: Test 'verbose' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, verbose = "not_logical"))
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, verbose = 123))

# Test case 13: Test 'safer_check' argument
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN  = length, verbose = "not_logical", safer_check = TRUE))

# Comprehensive test for all arguments
  expect_error(slide(data = NULL, window_size = "5", step = "2", FUN  = 123, args = 123, boundary = "invalid", parall = "not_logical", thread_nb = "not_numeric", print_count = "not_numeric", res_path = "invalid_directory", lib_path = "invalid_directory", verbose = "not_logical", safer_check = TRUE))

# No-error tests for each argument (expected value(s) of each argument tested with no error as result)
  # function name block: pkg:: and bare zero-arg calls (function_name[3] and arg_user_setting_eval <- NULL lines)
  expect_no_error(saferTool2::slide(data = 1:10, window_size = 5, step = 2, FUN = length, verbose = FALSE, safer_check = FALSE))
  expect_error(slide()) # zero-arg call still covered up to the no-default-values block
  # data argument: numeric vector
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, verbose = FALSE, safer_check = FALSE))
  # window_size argument: single positive numeric value
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, verbose = FALSE, safer_check = FALSE))
  # step argument: single positive numeric value <= window_size
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, verbose = FALSE, safer_check = FALSE))
  # from argument: single numeric value or NULL
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, from = 2, FUN = length, verbose = FALSE, safer_check = FALSE))
  # to argument: single numeric value or NULL
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, to = 9, FUN = length, verbose = FALSE, safer_check = FALSE))
  # FUN argument: function or single character string of a function name
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, verbose = FALSE, safer_check = FALSE))
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = "length", verbose = FALSE, safer_check = FALSE))
  # args argument: single character string or NULL
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = mean, args = "na.rm = TRUE", verbose = FALSE, safer_check = FALSE))
  # boundary argument: "left" or "right"
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, boundary = "right", verbose = FALSE, safer_check = FALSE))
  # parall argument: single logical
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, parall = FALSE, verbose = FALSE, safer_check = FALSE))
  # thread_nb argument: single positive integer (only used if parall = TRUE)
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, parall = TRUE, thread_nb = 2, verbose = FALSE, safer_check = FALSE))
  # print_count argument: single positive integer (Inf allowed)
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, print_count = 100, verbose = FALSE, safer_check = FALSE))
  # res_path argument: existing directory or NULL
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, res_path = tempdir(), verbose = FALSE, safer_check = FALSE))
  # verbose argument: single logical
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, verbose = TRUE, safer_check = FALSE))
  # lib_path argument: NULL or vector of characters
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, lib_path = NULL, verbose = FALSE, safer_check = FALSE))
  # error_text argument: single character string
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, error_text = " IN TEST.", verbose = FALSE, safer_check = FALSE))
  # safer_check argument: single logical
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, safer_check = TRUE))
})

test_that("slide() secondary checking blocks", {
  # primary checking blocks of the backbone: NULL, empty non NULL, NA-only arguments
  expect_error(slide(data = 1:10, window_size = NULL, step = 2, FUN = length)) # window_size cannot be NULL
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, boundary = NULL)) # boundary cannot be NULL
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, parall = NULL)) # parall cannot be NULL
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, print_count = NULL)) # print_count cannot be NULL
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, verbose = NULL)) # verbose cannot be NULL
  # empty non NULL arguments (character(0) or numeric(0) provided by the user)
  expect_error(slide(data = numeric(0), window_size = 5, step = 2, FUN = length)) # data empty non NULL
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, res_path = character(0))) # res_path empty non NULL
  # NA-only arguments
  expect_error(slide(data = NA, window_size = 5, step = 2, FUN = length)) # data made of NA only
  expect_error(slide(data = 1:10, window_size = NA_real_, step = 2, FUN = length)) # window_size made of NA only
  # safer_check argument checking
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, safer_check = "TRUE"))
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, safer_check = c(TRUE, FALSE)))
  # lib_path checking
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, lib_path = 123))
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, lib_path = "path/that/does/not/exist"))
  # data checks of the second round of checking
  expect_error(slide(data = c(1, Inf), window_size = 5, step = 2, FUN = length)) # Inf values
  expect_error(slide(data = c(1, NA), window_size = 5, step = 2, FUN = length)) # NA not allowed either (is.finite() guard)
  # management of "" in character arguments (res_path, boundary)
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, res_path = ""))
  # res_path not existing directory
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, res_path = "path/that/does/not/exist"))
  # arg_check error combinations
  expect_error(slide(data = 1:10, window_size = -5, step = 2, FUN = length)) # window_size negative
  expect_error(slide(data = 1:10, window_size = 5, step = -2, FUN = length)) # step negative
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, from = "a")) # from not numeric
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, to = "a")) # to not numeric
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, args = 123)) # args not character
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, print_count = -1)) # print_count negative
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, thread_nb = "not_numeric", parall = TRUE)) # thread_nb not numeric
  # safer_check given as a function: deparse1 branch of the safer_check argument checking
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, safer_check = mean))
  # lib_path given as a function: deparse1 branch of the lib_path checking
  expect_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, lib_path = mean))
  # lib_path with an existing directory: .libPaths() branch
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = length, lib_path = tempdir()))
  # print_count = Inf: allowed (no loop message desired), with parall = FALSE
  expect_no_error(slide(data = 1:10, window_size = 5, step = 2, FUN = mean, print_count = Inf, verbose = FALSE))
})

test_that("slide() parallelization branches with verbose output", {
  # parallel run with verbose = TRUE: PARALLELIZATION INITIATED / NUMBER OF THREADS / SPLIT OF TEST NUMBERS messages
  res_par <- slide(data = 1:10, window_size = 5, step = 2, FUN = length, parall = TRUE, thread_nb = 2, print_count = 1, verbose = TRUE)
  testthat::expect_equal(object = base::names(x = res_par), expected = base::c("left", "right", "center", "value"))
  testthat::expect_equal(object = res_par$value, expected = base::c(5, 5, 5, 4)) # same result as the non parallel run
  # parallel run with res_path set: the parallel log file is written in res_path
  res_dir <- base::tempdir()
  res_par2 <- slide(data = 1:10, window_size = 5, step = 2, FUN = length, parall = TRUE, thread_nb = 2, print_count = 1, res_path = res_dir, verbose = FALSE)
  testthat::expect_equal(object = res_par2$value, expected = base::c(5, 5, 5, 4))
  testthat::expect_true(object = base::file.exists(base::paste0(res_dir, "/fun_slide_parall_log.txt")))
  # thread_nb above the number of available threads: capped at detectCores() with the ONLY: message (verbose = TRUE)
  # data long enough (996 windows) so that clusterSplit() gives no empty chunk to any worker (an empty chunk triggers the pre-existing 1:length(x) loop bug of the worker on empty input, see CODE INCONSISTENCY 4)
  res_par3 <- slide(data = 1:1000, window_size = 5, step = 1, FUN = length, parall = TRUE, thread_nb = 10000, print_count = 1, verbose = TRUE)
  testthat::expect_equal(object = base::nrow(x = res_par3), expected = 996)
  # thread_nb = NULL with parall = TRUE: thread_nb set to the number of detected threads (verbose = FALSE to limit output)
  res_par4 <- slide(data = 1:1000, window_size = 5, step = 1, FUN = length, parall = TRUE, thread_nb = NULL, print_count = 1, verbose = FALSE)
  testthat::expect_equal(object = base::nrow(x = res_par4), expected = 996)
})

test_that("slide() values of the returned data frame", {
  # boundary = "left": windows are [left, right[ ; boundary = "right": windows are ]left, right]
  res_left <- slide(data = 1:10, window_size = 5, step = 2, FUN = length, verbose = FALSE, safer_check = FALSE)
  testthat::expect_equal(object = base::names(x = res_left), expected = base::c("left", "right", "center", "value"))
  testthat::expect_equal(object = res_left$value, expected = base::c(5, 5, 5, 4))
  testthat::expect_equal(object = res_left$left, expected = base::c(1, 3, 5, 7))
  testthat::expect_equal(object = res_left$right, expected = base::c(6, 8, 10, 12))
  res_right <- slide(data = 1:10, window_size = 5, step = 2, FUN = length, boundary = "right", verbose = FALSE, safer_check = FALSE)
  testthat::expect_equal(object = res_right$value, expected = base::c(5, 5, 5))
  testthat::expect_equal(object = res_right$left, expected = base::c(1, 3, 5))
  testthat::expect_equal(object = res_right$right, expected = base::c(6, 8, 10))
  # from/to arguments: window over the sub range [2, 9]
  res_ft <- slide(data = 1:10, window_size = 5, step = 2, from = 2, to = 9, FUN = length, verbose = FALSE, safer_check = FALSE)
  testthat::expect_equal(object = res_ft$left, expected = base::c(2, 4, 6))
  testthat::expect_equal(object = res_ft$right, expected = base::c(7, 9, 11))
  # FUN as a character string of a function name
  res_char <- slide(data = 1:10, window_size = 5, step = 2, FUN = "mean", verbose = FALSE, safer_check = FALSE)
  testthat::expect_equal(object = res_char$value, expected = base::c(3, 5, 7, 8.5))
  # args argument: extra argument passed to FUN
  res_args <- slide(data = 1:10, window_size = 5, step = 2, FUN = mean, args = "trim = 0.2", verbose = FALSE, safer_check = FALSE)
  testthat::expect_equal(object = res_args$value, expected = base::c(3, 5, 7, 8.5))
})

test_that("slide() data given as a matrix (documented accepted type)", {
  # matrix input: converted to vector internally (as.vector), documented accepted type not tested until now
  res_mat <- slide(data = base::matrix(data = 1:12, nrow = 3), window_size = 3, step = 2, FUN = mean, verbose = FALSE, safer_check = FALSE)
  testthat::expect_equal(object = base::names(x = res_mat), expected = base::c("left", "right", "center", "value"))
  testthat::expect_equal(object = res_mat$left, expected = base::c(1, 3, 5, 7, 9, 11))
  testthat::expect_equal(object = res_mat$right, expected = base::c(4, 6, 8, 10, 12, 14))
  testthat::expect_equal(object = res_mat$center, expected = base::c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5))
  testthat::expect_equal(object = res_mat$value, expected = base::c(2, 4, 6, 8, 10, 11.5)) # 11.5 because 12 is duplicated as boundary padding in the last window
})

test_that("slide() non-safer errors of FUN and seq() values", {
  # FUN given as a string that is not a function: non-safer match.fun() error
  testthat::expect_error(object = slide(data = 1:10, window_size = 3, step = 2, FUN = "not_a_function", verbose = FALSE, safer_check = FALSE), regexp = "object 'not_a_function' of mode 'function' was not found", fixed = TRUE)
  # FUN returning a vector of length 2 (e.g. range): internal consistency guard of the function
  testthat::expect_error(object = slide(data = 1:10, window_size = 3, step = 2, FUN = range, verbose = FALSE, safer_check = FALSE), regexp = "CODE INCONSISTENCY 3", fixed = TRUE)
  # step = 0: passes arg_check (0 is a non-negative number) but crashes in seq() (non-safer error)
  testthat::expect_error(object = slide(data = 1:10, window_size = 3, step = 0, FUN = mean, verbose = FALSE, safer_check = FALSE), regexp = "invalid '(to - from)/by'", fixed = TRUE)
  # from > to: passes arg_check but crashes in seq() (non-safer error)
  testthat::expect_error(object = slide(data = 1:10, window_size = 3, step = 2, FUN = mean, from = 8, to = 2, verbose = FALSE, safer_check = FALSE), regexp = "wrong sign in 'by' argument", fixed = TRUE)
  # to = Inf: passes arg_check (numeric) but crashes in seq() (non-safer error)
  testthat::expect_error(object = slide(data = 1:10, window_size = 3, step = 2, FUN = mean, to = Inf, verbose = FALSE, safer_check = FALSE), regexp = "'to' must be a finite number", fixed = TRUE)
})
