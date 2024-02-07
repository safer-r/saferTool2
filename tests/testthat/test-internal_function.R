test_that(".pack_and_function_check works well", {
  # .pack_and_function_check should throw an error when fun argument doesn't contain '::'
  expect_error(.pack_and_function_check(fun = "geom_point", lib.path = "/some/path", external.function.name = "fun1"))


  # pack_and_function_check should throw an error when required package is not installed
  expect_error(.pack_and_function_check(fun = "ggplot2::geom_point", lib.path = "/nonexistent/path", external.function.name = "fun1"))


  # .pack_and_function_check should throw an error when required function is missing
  expect_error(.pack_and_function_check(fun = "ggplot2::nonexistent_function", lib.path = "/some/path", external.function.name = "fun1"))
})
