test_that("permut works correctly", {
    data1 <- c(1, 2, 3, 4, 5)
    data2 <- c(6, 7, 8, 9, 10)
    n <- 100
    seed <- 12345
    print.count <- 10
    text.print <- "Test"
    cor.method <- "spearman"
    cor.limit <- 0.2
    warn.print <- FALSE
    lib.path <- "."
    
    expect_error(permut())
    expect_error(permut(data1 = 1))

    expect_error(permut(data1 = data1, data2 = "not_a_vector"))

    expect_error(permut(data1 = data1))
    expect_error(permut(data1 = data1, n = "not_an_integer"))

    expect_error(permut(data1 = data1, seed = "not_an_integer"))

    expect_error(permut(data1 = data1, print.count = "not_an_integer"))
    expect_error(permut(data1 = data1, text.print = 123))

    expect_error(permut(data1 = data1, cor.method = "invalid_method"))
    expect_error(permut(data1 = data1, cor.limit = "not_a_numeric"))
    
    expect_error(permut(data1 = data1, warn.print = "not_a_logical"))
    expect_error(permut(data1 = data1, lib.path = 123))
    
    expect_no_error(permut(data1 = data1,
                           data2 = data2,
                           n = n,
                           seed = seed,
                           print.count = print.count,
                           text.print = text.print,
                           cor.method = cor.method,
                           cor.limit = cor.limit,
                           warn.print = warn.print,
                           lib.path = lib.path))
})