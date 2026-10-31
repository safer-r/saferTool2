test_that("codon2aa returns correct amino acid", {
  # Define datasets
  codon1 <- "atg"
  codon2 <- "tga"
  codon3 <- "ctt"
  codon4 <- "aga"
  codon5 <- "tat"
  codon6 <- "gcc"
  codon7 <- "cgg"
  codon8 <- "agg"
  codon9 <- "ttt"
  codon10 <- "aaa"
  codon11 <- "tct"
  codon12 <- "ccc"
  codon13 <- "ggg"
  codon14 <- "tag"
  codon15 <- "xyz"
  codon16 <- "tcj"
  
  # Test cases for valid codons
  expect_equal(codon2aa(data = codon1), "M")
  expect_equal(codon2aa(data = codon2), "stop")
  expect_equal(codon2aa(data = codon3), "L")
  expect_equal(codon2aa(data = codon4), "R")
  expect_equal(codon2aa(data = codon5), "Y")
  expect_equal(codon2aa(data = codon6), "A")
  expect_equal(codon2aa(data = codon7), "R")
  expect_equal(codon2aa(data = codon8), "R")
  expect_equal(codon2aa(data = codon9), "F")
  expect_equal(codon2aa(data = codon10), "K")
  expect_equal(codon2aa(data = codon11), "S")
  expect_equal(codon2aa(data = codon12), "P")
  expect_equal(codon2aa(data = codon13), "G")
  expect_equal(codon2aa(data = codon14), "stop") 
  # Test cases for display argument
  expect_no_error(codon2aa(data = codon1, display = TRUE))
  expect_equal(codon2aa(data = codon2, display = FALSE), "stop")
  expect_no_error(codon2aa(data = codon3, display = TRUE))
  expect_equal(codon2aa(data = codon4, display = FALSE), "R")
  expect_no_error(codon2aa(data = codon5, display = TRUE))
  expect_equal(codon2aa(data = codon6, display = FALSE), "A")
  expect_no_error(codon2aa(data = codon7, display = TRUE))
  expect_equal(codon2aa(data = codon8, display = FALSE), "R")
  expect_no_error(codon2aa(data = codon9, display = TRUE))
  expect_equal(codon2aa(data = codon10, display = FALSE), "K")
  expect_no_error(codon2aa(data = codon11, display = TRUE))
  expect_equal(codon2aa(data = codon12, display = FALSE), "P")
  expect_no_error(codon2aa(data = codon13, display = TRUE))
  expect_equal(codon2aa(data = codon14, display = FALSE), "stop")
  
  # Test cases for invalid codon
  expect_error(codon2aa(data = codon15, display = FALSE))
  expect_error(codon2aa(data = codon15, display = TRUE))
  expect_error(codon2aa(data = codon16, display = FALSE))
  expect_error(codon2aa(data = codon16, display = TRUE))

  # Test cases for all argument 
  expect_no_error(codon2aa(data = codon1, display = TRUE, safer_check = TRUE))
  expect_no_error(codon2aa(data = codon2, display = TRUE, safer_check = TRUE))
  expect_error(codon2aa(data = codon15, display = TRUE, safer_check = TRUE))
  expect_error(codon2aa(data = codon16, display = TRUE, safer_check = TRUE))

  # data argument as a single string of three characters (uppercase)
  expect_no_error(codon2aa(data = "ATC", display = FALSE))
  # lib_path argument
  expect_no_error(codon2aa(data = codon1, display = FALSE, lib_path = NULL))
  # error_text argument
  expect_no_error(codon2aa(data = codon1, display = FALSE, error_text = " IN TEST."))
})

test_that("codon2aa primary checking and other checkings", {
  # function name block: pkg:: call and bare zero-arg call
  expect_no_error(saferTool2::codon2aa(data = "atg")) # function_name[3] line of the pkg:: call
  expect_error(codon2aa()) # zero-arg call still covered up to the no-default-values block
  # primary checking: NULL arguments
  expect_error(codon2aa(data = "atg", display = NULL)) # display cannot be NULL
  # primary checking: NA-only arguments
  expect_error(codon2aa(data = NA_character_)) # data made of NA only
  # safer_check argument checking
  expect_error(codon2aa(data = "atg", safer_check = "TRUE"))
  expect_error(codon2aa(data = "atg", safer_check = c(TRUE, FALSE)))
  # lib_path checking
  expect_error(codon2aa(data = "atg", lib_path = 123)) # not a character vector
  expect_error(codon2aa(data = "atg", lib_path = "path/that/does/not/exist")) # not an existing directory
  # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
  expect_error(codon2aa(data = "atg", lib_path = "path/that/does/not/exist", safer_check = FALSE)) # not an existing directory
  # arg_check combinations: data and display
  expect_error(codon2aa(data = 123)) # data not character
  expect_error(codon2aa(data = "atg", display = "TRUE")) # display not logical
  # other checkings of the second round of checking
  expect_error(codon2aa(data = "at")) # wrong length
  expect_error(codon2aa(data = c("a", "t", "g", "c"))) # wrong length (4)
  expect_error(codon2aa(data = "aXg")) # wrong letters
  expect_error(codon2aa(data = c("a", "X", "g"))) # wrong letters in vector form
  # empty non NULL argument management
  expect_error(codon2aa(data = character(0))) # empty non NULL object
  # safer_check given as a function: deparse1 branch of the safer_check argument checking
  expect_error(codon2aa(data = "atg", safer_check = mean))
  # lib_path given as a function: deparse1 branch of the lib_path checking
  expect_error(codon2aa(data = "atg", lib_path = mean))
  # lib_path with an existing directory: .libPaths() branch
  expect_no_error(codon2aa(data = "atg", display = FALSE, lib_path = tempdir()))
})

test_that("codon2aa data argument values: success forms and edge values", {
  # data as a 3-element character vector (equivalent to the single string form)
  testthat::expect_identical(object = codon2aa(data = c("a", "t", "g"), display = FALSE), expected = "M")
  testthat::expect_identical(object = codon2aa(data = c("T", "G", "A"), display = FALSE), expected = "stop") # uppercase also accepted (TGA -> stop codon)
  testthat::expect_identical(object = codon2aa(data = c("A", "T", "C"), display = FALSE), expected = "I")
  # mixed case also accepted (case-insensitive via toupper)
  testthat::expect_identical(object = codon2aa(data = c("a", "T", "g"), display = FALSE), expected = "M")
  # display = TRUE: the full standard genetic code table is returned (4x4x4 array with T/C/A/G dimnames)
  sgc <- codon2aa(data = "ATC", display = TRUE)
  testthat::expect_equal(object = base::dim(x = sgc), expected = base::c(4, 4, 4))
  testthat::expect_identical(object = base::names(x = base::dimnames(x = sgc)), expected = base::c("first", "second", "third"))
  testthat::expect_identical(object = base::unname(base::unlist(base::dimnames(x = sgc))), expected = base::rep(x = base::c("T", "C", "A", "G"), times = 3))
  testthat::expect_identical(object = sgc[["T", "C", "A"]], expected = "S")
  testthat::expect_identical(object = sgc[["A", "T", "G"]], expected = "M")
  # data = "": passes every checking (single string, no bad letters since all(logical(0)) is TRUE) and returns NA_character_
  testthat::expect_identical(object = codon2aa(data = "", display = FALSE), expected = NA_character_)
})
