test_that("codon_finder returns correct codons within specified range", {
  # Define datasets
  pos_set1 <- c(3, 7, 9, 12, 15) 
  pos_set2 <- c(1, 4, 8, 11, 14)  
  pos_set3 <- c(2, 5, 10, 13, 16)  
  pos_set4 <- c(6, 13, 17, 19, 20) 
  pos_set5 <- c(1, 2, 4, 8, 9)  
  begin_value <- -2
  end_value <- 12
  begin_value2 <- -4
  end_value2 <- 15
  
  # Test cases with specified range 5 to 10
  expect_error(codon_finder(pos_set1, begin = 5, end = 10))
  expect_error(codon_finder(pos_set2, begin = 5, end = 10))
  expect_error(codon_finder(pos_set3, begin = 5, end = 10))
  expect_error(codon_finder(pos_set4, begin = 5, end = 10))
  expect_error(codon_finder(pos_set5, begin = 5, end = 10))
  
  # Test cases with specified begin and end values
  expect_error(codon_finder(pos_set1, begin = begin_value, end = end_value, safer_check = TRUE))
  expect_error(codon_finder(pos_set2, begin = begin_value, end = end_value, safer_check = TRUE))
  expect_error(codon_finder(pos_set3, begin = begin_value, end = end_value, safer_check = TRUE))
  expect_error(codon_finder(pos_set4, begin = begin_value, end = end_value, safer_check = TRUE))
  # expect_error(codon_finder(pos_set5, begin = begin_value, end = end_value))
  
  # Test cases with different begin and end values
  expect_error(codon_finder(pos_set1, begin = begin_value2, end = end_value2, safer_check = TRUE))
  expect_error(codon_finder(pos_set2, begin = begin_value2, end = end_value2, safer_check = TRUE))
  expect_error(codon_finder(pos_set3, begin = begin_value2, end = end_value2, safer_check = TRUE))
  expect_error(codon_finder(pos_set4, begin = begin_value2, end = end_value2, safer_check = TRUE))
  expect_error(codon_finder(pos_set5, begin = begin_value2, end = end_value2, safer_check = TRUE))

# No-error tests for each argument (expected value(s) of each argument tested with no error as result)
  # pos argument: vector of integers between begin and end
  expect_no_error(codon_finder(pos = c(5, 6, 8, 10), begin = 5, end = 10))
  # pos argument with negatives (position 0 allowed)
  expect_no_error(codon_finder(pos = c(0, 5, 6, 8, 10), begin = -2, end = 12))
  # begin argument
  expect_no_error(codon_finder(pos = c(5, 6, 8, 10), begin = 5, end = 10))
  # end argument
  expect_no_error(codon_finder(pos = c(5, 6, 8, 10), begin = 5, end = 10))
  # safer_check argument
  expect_no_error(codon_finder(pos = c(5, 6, 8, 10), begin = 5, end = 10, safer_check = TRUE))
  # lib_path argument
  expect_no_error(codon_finder(pos = c(5, 6, 8, 10), begin = 5, end = 10, lib_path = NULL))
  # error_text argument
  expect_no_error(codon_finder(pos = c(5, 6, 8, 10), begin = 5, end = 10, error_text = " IN TEST."))
})

test_that("codon_finder returns correct values in the output data frame", {
  # full value assertion of the returned data frame (codon_nb, codon_pos, codon_begin, codon_end)
  res <- codon_finder(pos = c(5, 6, 8, 10), begin = 5, end = 10)
  testthat::expect_s3_class(object = res, class = "data.frame")
  testthat::expect_equal(object = base::names(x = res), expected = base::c("pos", "codon_nb", "codon_pos", "codon_begin", "codon_end"))
  testthat::expect_identical(object = res$pos, expected = base::c(5L, 6L, 8L, 10L))
  testthat::expect_identical(object = res$codon_nb, expected = base::c(1L, 1L, 2L, 2L))
  testthat::expect_identical(object = res$codon_pos, expected = base::c(1L, 2L, 1L, 3L))
  testthat::expect_identical(object = res$codon_begin, expected = base::c(5L, 5L, 8L, 8L))
  testthat::expect_identical(object = res$codon_end, expected = base::c(7L, 7L, 10L, 10L))
  # single position inside a codon, with negative begin (position 0 allowed)
  res2 <- codon_finder(pos = 0, begin = -2, end = 12)
  testthat::expect_identical(object = res2$codon_nb, expected = 1L)
  testthat::expect_identical(object = res2$codon_pos, expected = 3L)
  testthat::expect_identical(object = res2$codon_begin, expected = -2L)
  testthat::expect_identical(object = res2$codon_end, expected = 0L)
})

test_that("codon_finder primary checking and other checkings", {
  # function name block: pkg:: call and bare zero-arg call
  expect_no_error(saferTool2::codon_finder(pos = c(5, 6, 8, 10), begin = 5, end = 10)) # function_name[3] line of the pkg:: call
  expect_error(codon_finder()) # zero-arg call still covered up to the no-default-values block
  # primary checking: NULL arguments
  expect_error(codon_finder(pos = NULL, begin = 5, end = 10)) # pos cannot be NULL
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 10, safer_check = NULL)) # safer_check cannot be NULL
  # primary checking: empty non NULL arguments
  expect_error(codon_finder(pos = integer(0), begin = 5, end = 10)) # pos empty non NULL
  expect_error(codon_finder(pos = c(5, 6), begin = integer(0), end = 10)) # begin empty non NULL
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = integer(0))) # end empty non NULL
  # primary checking: NA-only arguments
  expect_error(codon_finder(pos = NA_integer_, begin = 5, end = 10)) # pos made of NA only
  expect_error(codon_finder(pos = c(5, 6), begin = NA_integer_, end = 10)) # begin made of NA only
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = NA_integer_)) # end made of NA only
  # safer_check argument checking
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 10, safer_check = "TRUE"))
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 10, safer_check = c(TRUE, FALSE)))
  # lib_path checking
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 10, lib_path = 123)) # not a character vector
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 10, lib_path = "path/that/does/not/exist")) # not an existing directory
  # arg_check combinations: pos, begin, end
  expect_error(codon_finder(pos = c("5", "6"), begin = 5, end = 10)) # pos not integer-like
  expect_error(codon_finder(pos = c(5.5, 6), begin = 5, end = 10)) # pos not integer-like
  expect_error(codon_finder(pos = c(5, 6), begin = 5.5, end = 10)) # begin not integer-like
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 10.5)) # end not integer-like
  # other checkings of the second round of checking
  expect_error(codon_finder(pos = c(5, 6), begin = 10, end = 5)) # begin >= end
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 6)) # (end - begin + 1) not a multiple of 3
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 20)) # pos values not between begin and end
  # safer_check given as a function: deparse1 branch of the safer_check argument checking
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 10, safer_check = mean))
  # lib_path given as a function: deparse1 branch of the lib_path checking
  expect_error(codon_finder(pos = c(5, 6), begin = 5, end = 10, lib_path = mean))
  # lib_path with an existing directory: .libPaths() branch
  expect_no_error(codon_finder(pos = c(5, 6), begin = 5, end = 10, lib_path = tempdir()))
})
