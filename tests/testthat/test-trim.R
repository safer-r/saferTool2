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