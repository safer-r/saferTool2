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
  codon11 <- "xyz"
  
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
  
  # Test cases for invalid codon
  expect_error(codon2aa(data = codon11, display = FALSE))
  expect_error(codon2aa(data = codon11, display = TRUE))
})
