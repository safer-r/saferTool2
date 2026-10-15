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
    lib_path <- NULL

    result1 <- permut(
        data1 = data1,
        data2 = NULL,
        n = n,
        seed = seed,
        print.count = 10,
        text.print = "",
        cor.method = "spearman",
        cor.limit = 0.2,
        warn.print = FALSE,
        lib_path = lib_path,
        safer_check = TRUE
    )
    expected1 <- list(data = c(1, 4, 2, 3, 5), warn = NULL, cor = 0.7, count = 100)
    expect_equal(result1, expected1)
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
    expect_error(permut(data1 = data1, lib_path = 123))
    expect_error(permut(data1 = data1, safer_check = TRUE))

    expect_error(permut(
                        data1 = data1,
                        data2 = data2,
                        n = n,
                        seed = seed,
                        print.count = print.count,
                        text.print = text.print,
                        cor.method = cor.method,
                        cor.limit = cor.limit,
                        warn.print = warn.print,
                        lib_path = lib_path,
                        safer_check = "not_a_logical"))

# No-error tests for each argument (expected value(s) of each argument tested with no error as result)
    # data1 argument: numeric vector
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print.count = 100, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE))
    # data1 argument: character vector (only valid if data2 is NULL)
    expect_no_error(permut(data1 = LETTERS[1:5], data2 = NULL, n = 100, seed = 12345, print.count = 100, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE))
    # data2 argument: numeric vector of same length as data1
    expect_no_error(permut(data1 = data1, data2 = data2, n = 100, seed = 12345, print.count = 100, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE))
    # n argument: single positive integer
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 10, seed = 12345, print.count = 10, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE))
    # seed argument: single integer (NULL also valid)
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = seed, print.count = 100, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE))
    # print.count argument: single positive integer
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print.count = print.count, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE))
    # text.print argument: character string ("" allowed)
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print.count = 100, text.print = "Test", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE))
    # cor.method argument: "pearson", "kendall" or "spearman"
    expect_no_error(permut(data1 = data1, data2 = data2, n = 100, seed = 12345, print.count = 100, text.print = "", cor.method = "pearson", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE))
    expect_no_error(permut(data1 = data1, data2 = data2, n = 100, seed = 12345, print.count = 100, text.print = "", cor.method = "kendall", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE))
    # cor.limit argument: single proportion between 0 and 1
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print.count = 100, text.print = "", cor.method = "spearman", cor.limit = cor.limit, warn.print = FALSE, safer_check = FALSE))
    # warn.print argument: single logical
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print.count = 100, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = TRUE, safer_check = FALSE))
    # lib_path argument: NULL or vector of characters
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print.count = 100, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, lib_path = NULL, safer_check = FALSE))
    # error_text argument: single character string
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print.count = 100, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, error_text = " IN TEST.", safer_check = FALSE))
    # safer_check argument: single logical
    expect_no_error(permut(data1 = c(0,0,0,0,0), data2 = NULL, n = 1, seed = 12345, print.count = 1, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, safer_check = TRUE))
})

test_that("permut primary checking and other checkings", {
    data1 <- c(1, 2, 3, 4, 5)
    data2 <- c(6, 7, 8, 9, 10)
    # function name block: pkg:: call and bare zero-arg call
    expect_no_error(saferTool2::permut(data1 = c(0,0,0,0,0), data2 = NULL, n = 1, seed = 1, print.count = 1, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE, safer_check = FALSE)) # function_name[3] line of the pkg:: call
    # primary checking: NULL arguments
    expect_error(permut(data1 = NULL, n = 10, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # data1 cannot be NULL
    expect_error(permut(data1 = data1, data2 = NULL, n = NULL, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # n cannot be NULL (data2 NULL)
    expect_error(permut(data1 = data1, print.count = NULL, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # print.count cannot be NULL
    expect_error(permut(data1 = data1, print.count = 10, cor.method = NULL, cor.limit = 0.5, warn.print = FALSE)) # cor.method cannot be NULL
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = NULL, warn.print = FALSE)) # cor.limit cannot be NULL
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = NULL)) # warn.print cannot be NULL
    # primary checking: empty non NULL arguments
    expect_error(permut(data1 = character(0), n = 10, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # data1 empty non NULL
    expect_error(permut(data1 = data1, print.count = numeric(0), cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # print.count empty non NULL
    # primary checking: NA-only arguments
    expect_error(permut(data1 = NA_character_, n = 10, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # data1 made of NA only
    expect_error(permut(data1 = data1, n = NA_integer_, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # n made of NA only
    expect_error(permut(data1 = data1, print.count = NA_integer_, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # print.count made of NA only
    expect_error(permut(data1 = data1, print.count = 10, cor.method = NA_character_, cor.limit = 0.5, warn.print = FALSE)) # cor.method made of NA only
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = NA_real_, warn.print = FALSE)) # cor.limit made of NA only
    # safer_check argument checking
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE, safer_check = "TRUE"))
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE, safer_check = c(TRUE, FALSE)))
    # lib_path checking
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE, lib_path = 123)) # not a character vector
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE, lib_path = "path/that/does/not/exist")) # not an existing directory
    # arg_check combinations
    expect_error(permut(data1 = data1, data2 = "not_a_vector", n = 10, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # data2 not numeric
    expect_error(permut(data1 = data1, data2 = c("a", "b"), n = 10, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # data2 not numeric
    expect_error(permut(data1 = data1, seed = "not_a_number", print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # seed not numeric
    expect_error(permut(data1 = data1, print.count = "not_a_number", cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # print.count not numeric
    expect_error(permut(data1 = data1, print.count = 10, text.print = 123, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # text.print not character
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = 1.5, warn.print = FALSE)) # cor.limit not a proportion
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = "not_a_logical")) # warn.print not logical
    expect_error(permut(data1 = data1, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE, error_text = 123)) # error_text not character
    # other checkings of the second round of checking
    expect_error(permut(data1 = 1, n = 10, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # data1 length < 2
    expect_error(permut(data1 = data1, data2 = 1:3, n = 10, print.count = 10, cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE)) # data1 and data2 not same length
    expect_error(permut(data1 = data1, n = 10, print.count = 10, cor.method = "invalid", cor.limit = 0.5, warn.print = FALSE)) # cor.method not among options
})

test_that("permut management of empty string in character arguments", {
    data1 <- c(1, 2, 3, 4, 5)
    # cor.method cannot contain "" (arg_check options already reject "", checked here through options)
    expect_error(permut(data1 = data1, data2 = NULL, n = 10, seed = 1, print.count = 10, text.print = "", cor.method = "", cor.limit = 0.5, warn.print = FALSE))
})

test_that("permut identical data warnings and special branches", {
    # data1 identical with data2 specified: no permutation, tempo.cor = 1, warn written
    res_id1 <- permut(data1 = c(1,1,1,1,1), data2 = 1:5, n = 10, seed = 1, print.count = 10, text.print = "", cor.method = "pearson", cor.limit = 0.5, warn.print = FALSE)
    testthat::expect_equal(object = res_id1$cor, expected = 1)
    testthat::expect_equal(object = res_id1$count, expected = 0)
    testthat::expect_true(object = base::grepl(x = res_id1$warn, pattern = "NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS", fixed = TRUE))
    # data2 identical: no permutation, tempo.cor = 1, warn written
    res_id2 <- permut(data1 = 1:5, data2 = c(6,6,6,6,6), n = 10, seed = 1, print.count = 10, text.print = "", cor.method = "pearson", cor.limit = 0.5, warn.print = FALSE)
    testthat::expect_equal(object = res_id2$cor, expected = 1)
    testthat::expect_equal(object = res_id2$count, expected = 0)
    testthat::expect_true(object = base::grepl(x = res_id2$warn, pattern = "NO PERMUTATION PERFORMED BECAUSE data2 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS", fixed = TRUE))
    # initial correlation below cor.limit: complete randomization (count of 5 samples)
    res_rand <- permut(data1 = c(1,3,2,5,4), data2 = 1:5, n = 10, seed = 42, print.count = 10, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = FALSE)
    testthat::expect_equal(object = res_rand$count, expected = 5)
    testthat::expect_true(object = base::grepl(x = res_rand$warn, pattern = "HAS BEEN DETECTED AS BELOW THE CORRELATION LIMIT PARAMETER", fixed = TRUE))
    # negative initial correlation: warn written and final correlation negative
    res_neg <- permut(data1 = 1:5, data2 = 5:1, n = 10, seed = 42, print.count = 10, text.print = "", cor.method = "pearson", cor.limit = 0.9, warn.print = FALSE)
    testthat::expect_equal(object = res_neg$cor, expected = -0.9)
    testthat::expect_true(object = base::grepl(x = res_neg$warn, pattern = "HAS BEEN DETECTED AS NEGATIVE", fixed = TRUE))
    # print.count > n: corrected to n (same permutation result as print.count == n with the same seed)
    res_pcn <- permut(data1 = 1:5, data2 = NULL, n = 10, seed = 1, print.count = 100, text.print = "", cor.method = "pearson", cor.limit = 0.5, warn.print = FALSE)
    testthat::expect_equal(object = res_pcn$count, expected = 10)
    res_pc0 <- permut(data1 = 1:5, data2 = NULL, n = 10, seed = 1, print.count = 10, text.print = "", cor.method = "pearson", cor.limit = 0.5, warn.print = FALSE)
    testthat::expect_equal(object = res_pcn$data, expected = res_pc0$data) # print.count correction does not change the permutation result
    # warn.print = TRUE: warning emitted on exit through the on.exit() call
    expect_warning(object = permut(data1 = c(0,0,0,0,0), data2 = NULL, n = 1, seed = 1, print.count = 1, text.print = "", cor.method = "spearman", cor.limit = 0.9, warn.print = TRUE), regexp = "NO PERMUTATION PERFORMED")
})

test_that("permut correlated permutation loops (for/while rounds and GOBACK)", {
    # round 1 with loop.nb.est > 1e4: for loop path, GOBACK TRUE then FALSE rounds, final while loop
    res <- suppressWarnings(permut(data1 = c(1,1,1,2,3), data2 = 1:5, n = 10, seed = 1, print.count = 1000, text.print = "", cor.method = "pearson", cor.limit = 0.5, warn.print = FALSE))
    testthat::expect_equal(object = base::round(x = res$cor, digits = 3), expected = 0.354)
    testthat::expect_true(object = res$count >= 15) # at least the first while loop step, the for loop round and the final while loop step
    # count as numeric with decimals: the for loop path adds fractional counts (verified against the local run)
    testthat::expect_true(object = base::is.numeric(x = res$count))
})
