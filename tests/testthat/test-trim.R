test_that("trim() handles displayed.nb argument without error", {
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, displayed.nb = 5))

# Unit test for the argument "single.value.display"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, single.value.display = TRUE))

# Unit test for the argument "trim.method"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, trim.method = "mean.sd"))


# Unit test for the argument "trim.cutoffs"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, trim.cutoffs = c(0.05, 0.975)))


# Unit test for the argument "interval.scale.disp"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, interval.scale.disp = TRUE))

# Unit test for the argument "down.space"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, down.space = 0.2))

# Unit test for the argument "left.space"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, left.space = 0.2))

# Unit test for the argument "up.space"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, up.space = 0.2))

# Unit test for the argument "right.space"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, right.space = 0.2))

# Unit test for the argument "orient"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, orient = 1))

# Unit test for the argument "dist.legend"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, dist.legend = 0.5))

# Unit test for the argument "amplif.label"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, amplif.label = 1.5))

# Unit test for the argument "amplif.axis"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, amplif.axis = 1.5))

# Unit test for the argument "std.x.range"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, std.x.range = FALSE))

# Unit test for the argument "std.y.range"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, std.y.range = FALSE))

# Unit test for the argument "cex.pt"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, cex.pt = 0.5))

# Unit test for the argument "col.box"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, col.box = "red"))

# Unit test for the argument "x.nb.inter.tick"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, x.nb.inter.tick = 6))

# Unit test for the argument "y.nb.inter.tick"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, y.nb.inter.tick = 6))

# Unit test for the argument "tick.length"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, tick.length = 0.5))

# Unit test for the argument "sec.tick.length"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, sec.tick.length = 0.5))

# Unit test for the argument "corner.text"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, corner.text = "Test corner text"))

# Unit test for the argument "amplif.legend"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, amplif.legend = 1.5))

# Unit test for the argument "corner.text.size"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, corner.text.size = 1.5))

# Unit test for the argument "trim.return"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, trim.return = FALSE))

# Unit test for the argument "safer_check = TRUE"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, safer_check = TRUE))

# Unit test for the argument "seed"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, seed = 22))
  expect_no_error(trim(data = data, seed = NULL))

# Unit test for the argument "lib_path"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, lib_path = NULL))

# Unit test for the argument "error_text"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, error_text = " IN TEST."))

# Unit test for all arguments in the function
  # Create a test case
  data <- 1:10
  expect_no_error(trim(
    data = data, 
    displayed.nb = 5, 
    single.value.display = TRUE, 
    trim.method = "mean.sd",
    trim.cutoffs = c(0.05, 0.975), 
    interval.scale.disp = TRUE, 
    down.space = 0.2,
    left.space = 0.2, 
    up.space = 0.2, 
    right.space = 0.2, 
    orient = 1, 
    dist.legend = 0.5,
    amplif.label = 1.5,
    amplif.axis = 1.5, 
    std.x.range = FALSE, 
    std.y.range = FALSE,
    cex.pt = 0.5, 
    col.box = "red", 
    x.nb.inter.tick = 6, 
    y.nb.inter.tick = 6,
    tick.length = 0.5, 
    sec.tick.length = 0.5, 
    corner.text = "Test corner text",
    amplif.legend = 1.5, 
    corner.text.size = 1.5, 
    trim.return = FALSE, 
    box.type = "l", 
    safer_check = TRUE))
})

test_that("trim primary checking and other checkings", {
  # function name block: pkg:: call and bare zero-arg call
  expect_no_error(saferTool2::trim(data = 1:10)) # function_name[3] line of the pkg:: call
  expect_error(trim()) # zero-arg call still covered up to the no-default-values block
  # primary checking: NULL arguments
  expect_error(trim(data = 1:10, single.value.display = NULL)) # single.value.display cannot be NULL
  expect_error(trim(data = 1:10, trim.method = NULL)) # trim.method cannot be NULL
  expect_error(trim(data = 1:10, safer_check = NULL)) # safer_check cannot be NULL
  # primary checking: NA-only arguments
  expect_error(trim(data = NA_real_)) # data made of NA only
  expect_error(trim(data = 1:10, trim.method = NA_character_)) # trim.method made of NA only
  # safer_check argument checking
  expect_error(trim(data = 1:10, safer_check = "TRUE"))
  expect_error(trim(data = 1:10, safer_check = c(TRUE, FALSE)))
  # lib_path checking
  expect_error(trim(data = 1:10, lib_path = 123)) # not a character vector
  expect_error(trim(data = 1:10, lib_path = "path/that/does/not/exist")) # not an existing directory
  # arg_check combinations
  expect_error(trim(data = 1:10, displayed.nb = 5.5)) # displayed.nb not integer-like
  expect_error(trim(data = 1:10, single.value.display = "TRUE")) # not logical
  expect_error(trim(data = 1:10, trim.method = "invalid")) # trim.method not among options
  expect_error(trim(data = 1:10, trim.cutoffs = c(0.05, 0.975, 0.999))) # trim.cutoffs length must be 2
  expect_error(trim(data = 1:10, trim.cutoffs = 0.05)) # trim.cutoffs length must be 2
  expect_error(trim(data = 1:10, down.space = -1)) # down.space negative not allowed
  expect_error(trim(data = 1:10, orient = -1)) # orient negative not allowed
  expect_error(trim(data = 1:10, box.type = "invalid")) # box.type not among options
  expect_error(trim(data = 1:10, box.type = "")) # box.type cannot be ""
  expect_error(trim(data = 1:10, col.box = 123)) # col.box not character
  expect_error(trim(data = 1:10, x.nb.inter.tick = -1)) # x.nb.inter.tick negative not allowed
  expect_error(trim(data = 1:10, x.nb.inter.tick = 4.5)) # x.nb.inter.tick not integer-like
  expect_error(trim(data = 1:10, y.nb.inter.tick = -1)) # y.nb.inter.tick negative not allowed
  expect_error(trim(data = 1:10, corner.text = 123)) # corner.text not character
  expect_error(trim(data = 1:10, trim.return = "TRUE")) # trim.return not logical
  expect_error(trim(data = 1:10, seed = 22.5)) # seed not integer-like
  expect_error(trim(data = 1:10, seed = -1)) # seed negative not allowed
  # remaining arg_check invalid-value tests
  expect_error(trim(data = 1:10, interval.scale.disp = "TRUE")) # interval.scale.disp not logical
  expect_error(trim(data = 1:10, left.space = -1)) # left.space negative not allowed
  expect_error(trim(data = 1:10, up.space = -1)) # up.space negative not allowed
  expect_error(trim(data = 1:10, right.space = -1)) # right.space negative not allowed
  expect_error(trim(data = 1:10, dist.legend = -1)) # dist.legend negative not allowed
  expect_error(trim(data = 1:10, amplif.label = -1)) # amplif.label negative not allowed
  expect_error(trim(data = 1:10, amplif.axis = -1)) # amplif.axis negative not allowed
  expect_error(trim(data = 1:10, std.x.range = "TRUE")) # std.x.range not logical
  expect_error(trim(data = 1:10, std.y.range = "TRUE")) # std.y.range not logical
  expect_error(trim(data = 1:10, cex.pt = -1)) # cex.pt negative not allowed
  expect_error(trim(data = 1:10, tick.length = 1.5)) # tick.length not a proportion
  expect_error(trim(data = 1:10, sec.tick.length = 1.5)) # sec.tick.length not a proportion
  expect_error(trim(data = 1:10, trim.cutoffs = c(1.5, 0.975))) # trim.cutoffs values not between 0 and 1
  expect_error(trim(data = 1:10, amplif.legend = -1)) # amplif.legend negative not allowed
  expect_error(trim(data = 1:10, corner.text.size = -1)) # corner.text.size negative not allowed
  # other checkings of the second round of checking
  expect_error(trim(data = c(NA, Inf))) # data cannot be made of NA and Inf only
  expect_error(trim(data = c("a", "b"))) # data must be numeric
  expect_error(trim(data = factor(c("a", "b")))) # data must be numeric
  expect_error(trim(data = matrix(c("a", "b"), nrow = 2))) # data must be numeric (matrix branch)
  expect_error(trim(data = 1:10, displayed.nb = 1)) # displayed.nb must be > 1
  # empty non NULL argument management
  expect_error(trim(data = integer(0))) # data empty non NULL
  expect_error(trim(data = 1:10, displayed.nb = integer(0))) # displayed.nb empty non NULL
  # safer_check given as a function: deparse1 branch of the safer_check argument checking
  expect_error(trim(data = 1:10, safer_check = mean))
  # lib_path given as a function: deparse1 branch of the lib_path checking
  expect_error(trim(data = 1:10, lib_path = mean))
  # lib_path with an existing directory: .libPaths() branch
  expect_no_error(trim(data = 1:10, lib_path = tempdir(), trim.return = FALSE))
})

test_that("trim main code branches", {
  # matrix input converted to vector
  res_matrix <- trim(data = matrix(data = 1:10, nrow = 2), trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_matrix))
  # NA values in data: warning text written (na.nb > 0 branch) + trim.return = TRUE returns the output
  res_na <- trim(data = c(1:10, NA), trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_na))
  # NA in data with a corner text already present: corner.text concatenation branch
  res_na2 <- trim(data = c(1:10, NA), corner.text = "Test corner text", trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_na2))
  # single value in data: "No graphic displayed" branch
  res_single <- trim(data = base::rep(x = 5, times = 10), single.value.display = FALSE, trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = base::is.null(x = res_single$trim.method)) # the single-value output
  # displayed.nb above the number of values: warning text written (both corner.text == "" and not "")
  res_above <- trim(data = 1:10, displayed.nb = 20, trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_above))
  res_above2 <- trim(data = 1:10, displayed.nb = 20, corner.text = "Test corner text", trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_above2))
  # displayed.nb set: sampling branch (displayed.nb < number of values)
  res_samp <- trim(data = c(1:100, 1:10), displayed.nb = 50, trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_samp))
  # interval.scale.disp = TRUE and trim.method != "": quantile branch of fun.add.cut() and legend with interval
  res_quant <- trim(data = c(1:100, 1:10), trim.method = "quantile", trim.cutoffs = c(0.05, 0.975), interval.scale.disp = TRUE, trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_quant$real.trim.cutoffs))
  # trim.method != "" and interval.scale.disp = FALSE: legend without interval.scale
  res_noleg <- trim(data = 1:10, trim.method = "mean.sd", interval.scale.disp = FALSE, trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_noleg))
  # trim.method == "": mean.sd legend branch (interval.scale.disp = FALSE) and no real.trim.cutoffs
  res_nometh <- trim(data = 1:10, trim.method = "", interval.scale.disp = FALSE, trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = base::is.null(x = res_nometh$real.trim.cutoffs))
  # trim.method == "" and interval.scale.disp = TRUE: mean.sd legend with interval.scale
  res_nometh2 <- trim(data = 1:10, trim.method = "", interval.scale.disp = TRUE, trim.return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = base::is.null(x = res_nometh2$real.trim.cutoffs))
  # trim.return = FALSE: no output returned
  testthat::expect_null(object = trim(data = 1:10, trim.return = FALSE, safer_check = FALSE))
})

test_that("trim corner.text branches", {
  # NA in data with corner.text == "": the "WARNING: NUMBER OF NA REMOVED IS" branch
  res_na <- trim(data = c(1:10, NA), trim.return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_na) # runs the graphics and NA corner text branch
  # interval.scale.disp = TRUE with corner.text == "" and not "": the MULTIPLYING FACTOR branches
  res_is1 <- trim(data = 1:10, interval.scale.disp = TRUE, trim.return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_is1)
  res_is2 <- trim(data = 1:10, interval.scale.disp = TRUE, corner.text = "Test corner text", trim.return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_is2)
  # trim.method != "" with corner.text == "" and not "": the SELECTED CUT-OFFS branches
  res_tm1 <- trim(data = 1:10, trim.method = "mean.sd", trim.return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_tm1)
  res_tm2 <- trim(data = 1:10, trim.method = "mean.sd", corner.text = "Test corner text", trim.return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_tm2)
  # data given as a non numeric non matrix object: the combined arg_check error of the data argument
  expect_error(trim(data = "abc"))
})
