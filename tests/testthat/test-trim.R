test_that("trim() handles displayed_nb argument without error", {
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, displayed_nb = 5))

# Unit test for the argument "single_value_display"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, single_value_display = TRUE))

# Unit test for the argument "trim_method"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, trim_method = "mean.sd"))


# Unit test for the argument "trim_cutoffs"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, trim_cutoffs = c(0.05, 0.975)))


# Unit test for the argument "interval_scale_disp"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, interval_scale_disp = TRUE))

# Unit test for the argument "down_space"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, down_space = 0.2))

# Unit test for the argument "left_space"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, left_space = 0.2))

# Unit test for the argument "up_space"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, up_space = 0.2))

# Unit test for the argument "right_space"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, right_space = 0.2))

# Unit test for the argument "orient"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, orient = 1))

# Unit test for the argument "dist_legend"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, dist_legend = 0.5))

# Unit test for the argument "amplif_label"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, amplif_label = 1.5))

# Unit test for the argument "amplif_axis"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, amplif_axis = 1.5))

# Unit test for the argument "std_x_range"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, std_x_range = FALSE))

# Unit test for the argument "std_y_range"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, std_y_range = FALSE))

# Unit test for the argument "cex_pt"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, cex_pt = 0.5))

# Unit test for the argument "col_box"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, col_box = "red"))

# Unit test for the argument "x_nb_inter_tick"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, x_nb_inter_tick = 6))

# Unit test for the argument "y_nb_inter_tick"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, y_nb_inter_tick = 6))

# Unit test for the argument "tick_length"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, tick_length = 0.5))

# Unit test for the argument "sec_tick_length"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, sec_tick_length = 0.5))

# Unit test for the argument "corner_text"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, corner_text = "Test corner text"))

# Unit test for the argument "amplif_legend"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, amplif_legend = 1.5))

# Unit test for the argument "corner_text_size"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, corner_text_size = 1.5))

# Unit test for the argument "trim_return"
  # Create a test case
  data <- 1:10
  expect_no_error(trim(data = data, trim_return = FALSE))

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
    displayed_nb = 5, 
    single_value_display = TRUE, 
    trim_method = "mean.sd",
    trim_cutoffs = c(0.05, 0.975), 
    interval_scale_disp = TRUE, 
    down_space = 0.2,
    left_space = 0.2, 
    up_space = 0.2, 
    right_space = 0.2, 
    orient = 1, 
    dist_legend = 0.5,
    amplif_label = 1.5,
    amplif_axis = 1.5, 
    std_x_range = FALSE, 
    std_y_range = FALSE,
    cex_pt = 0.5, 
    col_box = "red", 
    x_nb_inter_tick = 6, 
    y_nb_inter_tick = 6,
    tick_length = 0.5, 
    sec_tick_length = 0.5, 
    corner_text = "Test corner text",
    amplif_legend = 1.5, 
    corner_text_size = 1.5, 
    trim_return = FALSE, 
    box_type = "l", 
    safer_check = TRUE))
})

test_that("trim primary checking and other checkings", {
  # function name block: pkg:: call and bare zero-arg call
  expect_no_error(saferTool2::trim(data = 1:10)) # function_name[3] line of the pkg:: call
  expect_error(trim()) # zero-arg call still covered up to the no-default-values block
  # primary checking: NULL arguments
  expect_error(trim(data = 1:10, single_value_display = NULL)) # single_value_display cannot be NULL
  expect_error(trim(data = 1:10, trim_method = NULL)) # trim_method cannot be NULL
  expect_error(trim(data = 1:10, safer_check = NULL)) # safer_check cannot be NULL
  # primary checking: NA-only arguments
  expect_error(trim(data = NA_real_)) # data made of NA only
  expect_error(trim(data = 1:10, trim_method = NA_character_)) # trim_method made of NA only
  # safer_check argument checking
  expect_error(trim(data = 1:10, safer_check = "TRUE"))
  expect_error(trim(data = 1:10, safer_check = c(TRUE, FALSE)))
  # lib_path checking
  expect_error(trim(data = 1:10, lib_path = 123)) # not a character vector
  expect_error(trim(data = 1:10, lib_path = "path/that/does/not/exist")) # not an existing directory
  # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
  expect_error(trim(data = 1:10, lib_path = "path/that/does/not/exist", safer_check = FALSE)) # not an existing directory
  # arg_check combinations
  expect_error(trim(data = 1:10, displayed_nb = 5.5)) # displayed_nb not integer-like
  expect_error(trim(data = 1:10, single_value_display = "TRUE")) # not logical
  expect_error(trim(data = 1:10, trim_method = "invalid")) # trim_method not among options
  expect_error(trim(data = 1:10, trim_cutoffs = c(0.05, 0.975, 0.999))) # trim_cutoffs length must be 2
  expect_error(trim(data = 1:10, trim_cutoffs = 0.05)) # trim_cutoffs length must be 2
  expect_error(trim(data = 1:10, down_space = -1)) # down_space negative not allowed
  expect_error(trim(data = 1:10, orient = -1)) # orient negative not allowed
  expect_error(trim(data = 1:10, box_type = "invalid")) # box_type not among options
  expect_error(trim(data = 1:10, box_type = "")) # box_type cannot be ""
  expect_error(trim(data = 1:10, col_box = 123)) # col_box not character
  expect_error(trim(data = 1:10, x_nb_inter_tick = -1)) # x_nb_inter_tick negative not allowed
  expect_error(trim(data = 1:10, x_nb_inter_tick = 4.5)) # x_nb_inter_tick not integer-like
  expect_error(trim(data = 1:10, y_nb_inter_tick = -1)) # y_nb_inter_tick negative not allowed
  expect_error(trim(data = 1:10, corner_text = 123)) # corner_text not character
  expect_error(trim(data = 1:10, trim_return = "TRUE")) # trim_return not logical
  expect_error(trim(data = 1:10, seed = 22.5)) # seed not integer-like
  expect_error(trim(data = 1:10, seed = -1)) # seed negative not allowed
  # remaining arg_check invalid-value tests
  expect_error(trim(data = 1:10, interval_scale_disp = "TRUE")) # interval_scale_disp not logical
  expect_error(trim(data = 1:10, left_space = -1)) # left_space negative not allowed
  expect_error(trim(data = 1:10, up_space = -1)) # up_space negative not allowed
  expect_error(trim(data = 1:10, right_space = -1)) # right_space negative not allowed
  expect_error(trim(data = 1:10, dist_legend = -1)) # dist_legend negative not allowed
  expect_error(trim(data = 1:10, amplif_label = -1)) # amplif_label negative not allowed
  expect_error(trim(data = 1:10, amplif_axis = -1)) # amplif_axis negative not allowed
  expect_error(trim(data = 1:10, std_x_range = "TRUE")) # std_x_range not logical
  expect_error(trim(data = 1:10, std_y_range = "TRUE")) # std_y_range not logical
  expect_error(trim(data = 1:10, cex_pt = -1)) # cex_pt negative not allowed
  expect_error(trim(data = 1:10, tick_length = 1.5)) # tick_length not a proportion
  expect_error(trim(data = 1:10, sec_tick_length = 1.5)) # sec_tick_length not a proportion
  expect_error(trim(data = 1:10, trim_cutoffs = c(1.5, 0.975))) # trim_cutoffs values not between 0 and 1
  expect_error(trim(data = 1:10, amplif_legend = -1)) # amplif_legend negative not allowed
  expect_error(trim(data = 1:10, corner_text_size = -1)) # corner_text_size negative not allowed
  # other checkings of the second round of checking
  expect_error(trim(data = c(NA, Inf))) # data cannot be made of NA and Inf only
  expect_error(trim(data = c("a", "b"))) # data must be numeric
  expect_error(trim(data = factor(c("a", "b")))) # data must be numeric
  expect_error(trim(data = matrix(c("a", "b"), nrow = 2))) # data must be numeric (matrix branch)
  expect_error(trim(data = 1:10, displayed_nb = 1)) # displayed_nb must be > 1
  # empty non NULL argument management
  expect_error(trim(data = integer(0))) # data empty non NULL
  expect_error(trim(data = 1:10, displayed_nb = integer(0))) # displayed_nb empty non NULL
  # safer_check given as a function: deparse1 branch of the safer_check argument checking
  expect_error(trim(data = 1:10, safer_check = mean))
  # lib_path given as a function: deparse1 branch of the lib_path checking
  expect_error(trim(data = 1:10, lib_path = mean))
  # lib_path with an existing directory: .libPaths() branch
  expect_no_error(trim(data = 1:10, lib_path = tempdir(), trim_return = FALSE))
})

test_that("trim main code branches", {
  # matrix input converted to vector
  res_matrix <- trim(data = matrix(data = 1:10, nrow = 2), trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_matrix))
  # NA values in data: warning text written (na.nb > 0 branch) + trim_return = TRUE returns the output
  res_na <- trim(data = c(1:10, NA), trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_na))
  # NA in data with a corner text already present: corner_text concatenation branch
  res_na2 <- trim(data = c(1:10, NA), corner_text = "Test corner text", trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_na2))
  # single value in data: "No graphic displayed" branch
  res_single <- trim(data = base::rep(x = 5, times = 10), single_value_display = FALSE, trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = base::is.null(x = res_single$trim_method)) # the single-value output
  # displayed_nb above the number of values: warning text written (both corner_text == "" and not "")
  res_above <- trim(data = 1:10, displayed_nb = 20, trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_above))
  res_above2 <- trim(data = 1:10, displayed_nb = 20, corner_text = "Test corner text", trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_above2))
  # displayed_nb set: sampling branch (displayed_nb < number of values)
  res_samp <- trim(data = c(1:100, 1:10), displayed_nb = 50, trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_samp))
  # interval_scale_disp = TRUE and trim_method != "": quantile branch of fun_add_cut() and legend with interval
  res_quant <- trim(data = c(1:100, 1:10), trim_method = "quantile", trim_cutoffs = c(0.05, 0.975), interval_scale_disp = TRUE, trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_quant$real.trim.cutoffs))
  # trim_method != "" and interval_scale_disp = FALSE: legend without interval.scale
  res_noleg <- trim(data = 1:10, trim_method = "mean.sd", interval_scale_disp = FALSE, trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_noleg))
  # trim_method == "": mean.sd legend branch (interval_scale_disp = FALSE) and no real.trim.cutoffs
  res_nometh <- trim(data = 1:10, trim_method = "", interval_scale_disp = FALSE, trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = base::is.null(x = res_nometh$real.trim.cutoffs))
  # trim_method == "" and interval_scale_disp = TRUE: mean.sd legend with interval.scale
  res_nometh2 <- trim(data = 1:10, trim_method = "", interval_scale_disp = TRUE, trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = base::is.null(x = res_nometh2$real.trim.cutoffs))
  # trim_return = FALSE: no output returned
  testthat::expect_null(object = trim(data = 1:10, trim_return = FALSE, safer_check = FALSE))
})

test_that("trim corner_text branches", {
  # NA in data with corner_text == "": the "WARNING: NUMBER OF NA REMOVED IS" branch
  res_na <- trim(data = c(1:10, NA), trim_return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_na) # runs the graphics and NA corner text branch
  # interval_scale_disp = TRUE with corner_text == "" and not "": the MULTIPLYING FACTOR branches
  res_is1 <- trim(data = 1:10, interval_scale_disp = TRUE, trim_return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_is1)
  res_is2 <- trim(data = 1:10, interval_scale_disp = TRUE, corner_text = "Test corner text", trim_return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_is2)
  # trim_method != "" with corner_text == "" and not "": the SELECTED CUT-OFFS branches
  res_tm1 <- trim(data = 1:10, trim_method = "mean.sd", trim_return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_tm1)
  res_tm2 <- trim(data = 1:10, trim_method = "mean.sd", corner_text = "Test corner text", trim_return = FALSE, safer_check = FALSE)
  testthat::expect_null(object = res_tm2)
  # data given as a non numeric non matrix object: the combined arg_check error of the data argument
  expect_error(trim(data = "abc"))
})

test_that("trim displayed_nb exact-equality and single_value_display = TRUE branches", {
  # single_value_display = TRUE with single-value data: the normal-graph branch (distinct from the FALSE -> NULL branch)
  res_single_true <- trim(data = base::rep(x = 5, times = 3), single_value_display = TRUE, trim_method = "mean.sd", trim_return = TRUE, safer_check = FALSE)
  testthat::expect_true(object = ! base::is.null(x = res_single_true$trim_method)) # normal graph computed
  testthat::expect_identical(object = res_single_true$trim_method, expected = "mean.sd")
  testthat::expect_equal(object = res_single_true$real.trim.cutoffs, expected = base::c(5, 5)) # degenerate interval around the single value
  # displayed_nb exactly equal to the number of values: no sampling (sampling branch only if length > displayed_nb)
  res_eq <- trim(data = base::c(10, 1, 5, 8, 3, 9, 4, 7, 2, 6), displayed_nb = 10, trim_method = "quantile", trim_cutoffs = c(0.2, 0.8), trim_return = TRUE, safer_check = FALSE)
  testthat::expect_identical(object = res_eq$kept.values, expected = base::c(3, 4, 5, 6, 7, 8)) # kept in sampled_data order (no sampling: initial order)
  testthat::expect_identical(object = res_eq$trimmed.values, expected = base::c(1, 2, 9, 10))
  testthat::expect_equal(object = base::unname(res_eq$real.trim.cutoffs), expected = base::c(2.8, 8.2))
  # displayed_nb below the number of values: sampling occurs (with seed for determinism)
  res_samp <- trim(data = base::c(10, 1, 5, 8, 3, 9, 4, 7, 2, 6), displayed_nb = 5, trim_method = "quantile", trim_cutoffs = c(0.2, 0.8), trim_return = TRUE, safer_check = FALSE, seed = 22)
  testthat::expect_identical(object = res_samp$kept.values, expected = base::c(3, 4, 5, 6, 7, 8)) # sampling of the graph does not change the trim computation (computed on the full data)
  # trim_return with non-empty kept/trimmed values for a full quantile trim (values assertions)
  res_q <- trim(data = 1:10, trim_method = "quantile", trim_cutoffs = c(0.2, 0.8), trim_return = TRUE, safer_check = FALSE, seed = 22)
  testthat::expect_equal(object = res_q$kept.values, expected = 3:8)
  testthat::expect_equal(object = res_q$trimmed.values, expected = base::c(1, 2, 9, 10))
  testthat::expect_equal(object = base::unname(res_q$real.trim.cutoffs), expected = base::c(2.8, 8.2))
})
