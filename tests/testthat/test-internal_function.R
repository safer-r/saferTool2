test_that("test internal functions of saferDev used by saferTool2", {

    # .pack_and_function_check() and .base_op_check() (internals of saferDev, used by saferTool2 via getFromNamespace)
    .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = , envir = )
    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = , envir = )

    fun_wrong1 <- "geom_point"  # incorrect input
    fun_wrong2 <- "ggplot2::non_existent_function"  # incorrect function name
    fun_good <- "ggplot2::geom_point" # correct input
    path_wrong <- "path/to/library"  # incorrect input
    path_good <- NULL  # incorrect input

    testthat::expect_no_error(.pack_and_function_check(
        fun = fun_good,
        lib_path = path_good,
        error_text = ""
    ))
    testthat::expect_error(.pack_and_function_check(
        fun = fun_wrong1,
        lib_path = path_good,
        error_text = ""
    ))
    testthat::expect_error(.pack_and_function_check(
        fun = fun_wrong2,
        lib_path = path_good,
        error_text = ""
    ))
    testthat::expect_error(.pack_and_function_check(
        fun = fun_good,
        lib_path = path_wrong,
        error_text = ""
    ))

    # .base_op_check()
    testthat::expect_no_error(.base_op_check(
        error_text = ""
    ))

})
