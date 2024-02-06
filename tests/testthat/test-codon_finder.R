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
  expect_no_error(codon_finder(pos_set1, begin = 5, end = 10))
  expect_no_error(codon_finder(pos_set2, begin = 5, end = 10))
  expect_no_error(codon_finder(pos_set3, begin = 5, end = 10))
  expect_no_error(codon_finder(pos_set4, begin = 5, end = 10))
  expect_no_error(codon_finder(pos_set5, begin = 5, end = 10))
  
  # Test cases with specified begin and end values
  expect_no_error(codon_finder(pos_set1, begin = begin_value, end = end_value))
  expect_no_error(codon_finder(pos_set2, begin = begin_value, end = end_value))
  expect_no_error(codon_finder(pos_set3, begin = begin_value, end = end_value))
  expect_no_error(codon_finder(pos_set4, begin = begin_value, end = end_value))
  expect_no_error(codon_finder(pos_set5, begin = begin_value, end = end_value))
  
  # Test cases with different begin and end values
  expect_no_error(codon_finder(pos_set1, begin = begin_value2, end = end_value2))
  expect_no_error(codon_finder(pos_set2, begin = begin_value2, end = end_value2))
  expect_no_error(codon_finder(pos_set3, begin = begin_value2, end = end_value2))
  expect_no_error(codon_finder(pos_set4, begin = begin_value2, end = end_value2))
  expect_no_error(codon_finder(pos_set5, begin = begin_value2, end = end_value2))
})