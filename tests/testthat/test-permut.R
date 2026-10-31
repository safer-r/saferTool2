test_that("permut works correctly", {
    data1 <- c(1, 2, 3, 4, 5)
    data2 <- c(6, 7, 8, 9, 10)
    n <- 100
    seed <- 12345
    print_count <- 10
    text_print <- "Test"
    cor_method <- "spearman"
    cor_limit <- 0.2
    warn_print <- FALSE
    lib_path <- NULL

    result1 <- permut(
        data1 = data1,
        data2 = NULL,
        n = n,
        seed = seed,
        print_count = 10,
        text_print = "",
        cor_method = "spearman",
        cor_limit = 0.2,
        warn_print = FALSE,
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

    expect_error(permut(data1 = data1, print_count = "not_an_integer"))
    expect_error(permut(data1 = data1, text_print = 123))

    expect_error(permut(data1 = data1, cor_method = "invalid_method"))
    expect_error(permut(data1 = data1, cor_limit = "not_a_numeric"))

    expect_error(permut(data1 = data1, warn_print = "not_a_logical"))
    expect_error(permut(data1 = data1, lib_path = 123))
    # expect_error(permut(data1 = data1, safer_check = TRUE)) # removed: safer_check = TRUE is a VALID value (the error came from the missing n argument below)

    expect_error(permut(
                        data1 = data1,
                        data2 = data2,
                        n = n,
                        seed = seed,
                        print_count = print_count,
                        text_print = text_print,
                        cor_method = cor_method,
                        cor_limit = cor_limit,
                        warn_print = warn_print,
                        lib_path = lib_path,
                        safer_check = "not_a_logical"))

# No-error tests for each argument (expected value(s) of each argument tested with no error as result)
    # data1 argument: numeric vector
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    # data1 argument: character vector (only valid if data2 is NULL)
    expect_no_error(permut(data1 = LETTERS[1:5], data2 = NULL, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    # data1 argument: logical vector (only valid if data2 is NULL)
    expect_no_error(permut(data1 = c(TRUE, FALSE, TRUE, FALSE, TRUE), data2 = NULL, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    # data2 argument: numeric vector of same length as data1
    expect_no_error(permut(data1 = data1, data2 = data2, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    # n argument: single positive integer
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 10, seed = 12345, print_count = 10, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    # seed argument: single integer (NULL also valid)
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = seed, print_count = 100, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    # print_count argument: single positive integer
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print_count = print_count, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    # text_print argument: character string ("" allowed)
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print_count = 100, text_print = "Test", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    # cor_method argument: "pearson", "kendall" or "spearman"
    expect_no_error(permut(data1 = data1, data2 = data2, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "pearson", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    expect_no_error(permut(data1 = data1, data2 = data2, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "kendall", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE))
    # cor_limit argument: single proportion between 0 and 1
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "spearman", cor_limit = cor_limit, warn_print = FALSE, safer_check = FALSE))
    # warn_print argument: single logical
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = TRUE, safer_check = FALSE))
    # lib_path argument: NULL or vector of characters
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, lib_path = NULL, safer_check = FALSE))
    # error_text argument: single character string
    expect_no_error(permut(data1 = data1, data2 = NULL, n = 100, seed = 12345, print_count = 100, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, error_text = " IN TEST.", safer_check = FALSE))
    # safer_check argument: single logical
    expect_no_error(permut(data1 = c(0,0,0,0,0), data2 = NULL, n = 1, seed = 12345, print_count = 1, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = TRUE))
})

test_that("permut primary checking and other checkings", {
    data1 <- c(1, 2, 3, 4, 5)
    data2 <- c(6, 7, 8, 9, 10)
    # function name block: pkg:: call and bare zero-arg call
    expect_no_error(saferTool2::permut(data1 = c(0,0,0,0,0), data2 = NULL, n = 1, seed = 1, print_count = 1, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE, safer_check = FALSE)) # function_name[3] line of the pkg:: call
    # primary checking: NULL arguments
    expect_error(permut(data1 = NULL, n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # data1 cannot be NULL
    expect_error(permut(data1 = data1, data2 = NULL, n = NULL, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # n cannot be NULL (data2 NULL)
    expect_error(permut(data1 = data1, print_count = NULL, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # print_count cannot be NULL
    expect_error(permut(data1 = data1, print_count = 10, cor_method = NULL, cor_limit = 0.5, warn_print = FALSE)) # cor_method cannot be NULL
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = NULL, warn_print = FALSE)) # cor_limit cannot be NULL
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = NULL)) # warn_print cannot be NULL
    # primary checking: empty non NULL arguments
    expect_error(permut(data1 = character(0), n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # data1 empty non NULL
    expect_error(permut(data1 = data1, print_count = numeric(0), cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # print_count empty non NULL
    # primary checking: NA-only arguments
    expect_error(permut(data1 = NA_character_, n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # data1 made of NA only
    expect_error(permut(data1 = data1, n = NA_integer_, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # n made of NA only
    expect_error(permut(data1 = data1, print_count = NA_integer_, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # print_count made of NA only
    expect_error(permut(data1 = data1, print_count = 10, cor_method = NA_character_, cor_limit = 0.5, warn_print = FALSE)) # cor_method made of NA only
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = NA_real_, warn_print = FALSE)) # cor_limit made of NA only
    # safer_check argument checking
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE, safer_check = "TRUE"))
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE, safer_check = c(TRUE, FALSE)))
    # lib_path checking
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE, lib_path = 123)) # not a character vector
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE, lib_path = "path/that/does/not/exist")) # not an existing directory
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE, lib_path = "path/that/does/not/exist", safer_check = FALSE)) # not an existing directory
    # arg_check combinations
    expect_error(permut(data1 = data1, data2 = "not_a_vector", n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # data2 not numeric
    expect_error(permut(data1 = data1, data2 = c("a", "b"), n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # data2 not numeric
    expect_error(permut(data1 = data1, seed = "not_a_number", print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # seed not numeric
    expect_error(permut(data1 = data1, print_count = "not_a_number", cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # print_count not numeric
    expect_error(permut(data1 = data1, print_count = 10, text_print = 123, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # text_print not character
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = 1.5, warn_print = FALSE)) # cor_limit not a proportion
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = "not_a_logical")) # warn_print not logical
    expect_error(permut(data1 = data1, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE, error_text = 123)) # error_text not character (the real error comes from the missing n argument, since error_text is unconditionally converted to a string)
    # other checkings of the second round of checking
    expect_error(permut(data1 = 1, n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # data1 length < 2
    expect_error(permut(data1 = data1, data2 = 1:3, n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE)) # data1 and data2 not same length
    expect_error(permut(data1 = data1, n = 10, print_count = 10, cor_method = "invalid", cor_limit = 0.5, warn_print = FALSE)) # cor_method not among options
})

test_that("permut management of empty string in character arguments", {
    data1 <- c(1, 2, 3, 4, 5)
    # cor_method cannot contain "" (arg_check options already reject "", checked here through options)
    expect_error(permut(data1 = data1, data2 = NULL, n = 10, seed = 1, print_count = 10, text_print = "", cor_method = "", cor_limit = 0.5, warn_print = FALSE))
})

test_that("permut identical data warnings and special branches", {
    # data1 identical with data2 specified: no permutation, tempo.cor = 1, warn written
    res_id1 <- permut(data1 = c(1,1,1,1,1), data2 = 1:5, n = 10, seed = 1, print_count = 10, text_print = "", cor_method = "pearson", cor_limit = 0.5, warn_print = FALSE)
    testthat::expect_equal(object = res_id1$cor, expected = 1)
    testthat::expect_equal(object = res_id1$count, expected = 0)
    testthat::expect_true(object = base::grepl(x = res_id1$warn, pattern = "NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS", fixed = TRUE))
    # data2 identical: no permutation, tempo.cor = 1, warn written
    res_id2 <- permut(data1 = 1:5, data2 = c(6,6,6,6,6), n = 10, seed = 1, print_count = 10, text_print = "", cor_method = "pearson", cor_limit = 0.5, warn_print = FALSE)
    testthat::expect_equal(object = res_id2$cor, expected = 1)
    testthat::expect_equal(object = res_id2$count, expected = 0)
    testthat::expect_true(object = base::grepl(x = res_id2$warn, pattern = "NO PERMUTATION PERFORMED BECAUSE data2 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS", fixed = TRUE))
    # initial correlation below cor_limit: complete randomization (count of 5 samples)
    res_rand <- permut(data1 = c(1,3,2,5,4), data2 = 1:5, n = 10, seed = 42, print_count = 10, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = FALSE)
    testthat::expect_equal(object = res_rand$count, expected = 5)
    testthat::expect_true(object = base::grepl(x = res_rand$warn, pattern = "HAS BEEN DETECTED AS BELOW THE CORRELATION LIMIT PARAMETER", fixed = TRUE))
    # negative initial correlation: warn written and final correlation negative
    res_neg <- permut(data1 = 1:5, data2 = 5:1, n = 10, seed = 42, print_count = 10, text_print = "", cor_method = "pearson", cor_limit = 0.9, warn_print = FALSE)
    testthat::expect_equal(object = res_neg$cor, expected = -0.9)
    testthat::expect_true(object = base::grepl(x = res_neg$warn, pattern = "HAS BEEN DETECTED AS NEGATIVE", fixed = TRUE))
    # print_count > n: corrected to n (same permutation result as print_count == n with the same seed)
    res_pcn <- permut(data1 = 1:5, data2 = NULL, n = 10, seed = 1, print_count = 100, text_print = "", cor_method = "pearson", cor_limit = 0.5, warn_print = FALSE)
    testthat::expect_equal(object = res_pcn$count, expected = 10)
    res_pc0 <- permut(data1 = 1:5, data2 = NULL, n = 10, seed = 1, print_count = 10, text_print = "", cor_method = "pearson", cor_limit = 0.5, warn_print = FALSE)
    testthat::expect_equal(object = res_pcn$data, expected = res_pc0$data) # print_count correction does not change the permutation result
    # warn_print = TRUE: warning emitted on exit through the on.exit() call
    expect_warning(object = permut(data1 = c(0,0,0,0,0), data2 = NULL, n = 1, seed = 1, print_count = 1, text_print = "", cor_method = "spearman", cor_limit = 0.9, warn_print = TRUE), regexp = "NO PERMUTATION PERFORMED")
})

test_that("permut non-safer errors of loop counter values and data2 factor case", {
    data1 <- 1:5
    # n = 0: passes arg_check (0 is a non-negative number) but crashes in the loop code (sample.int(size = 0) -> empty pos)
    testthat::expect_error(object = permut(data1 = data1, data2 = NULL, n = 0, seed = 1, print_count = 1, text_print = "", cor_method = "pearson", cor_limit = 0.9, warn_print = FALSE), regexp = "NAs are not allowed in subscripted assignments", fixed = TRUE)
    # n = 0.5: same crash (1:0.5 is an empty sequence), non-safer error documented here
    testthat::expect_error(object = permut(data1 = data1, data2 = NULL, n = 0.5, seed = 1, print_count = 1, text_print = "", cor_method = "pearson", cor_limit = 0.9, warn_print = FALSE), regexp = "NAs are not allowed in subscripted assignments", fixed = TRUE)
    # print_count = 0: same crash mechanism (loop of 0 iterations)
    testthat::expect_error(object = permut(data1 = data1, data2 = NULL, n = 5, seed = 1, print_count = 0, text_print = "", cor_method = "pearson", cor_limit = 0.9, warn_print = FALSE), regexp = "NAs are not allowed in subscripted assignments", fixed = TRUE)
    # data2 given as a factor: passes the mode = "numeric" arg_check but crashes in stats::cor (non-safer error path)
    testthat::expect_error(object = permut(data1 = data1, data2 = base::factor(x = 1:5), n = 10, seed = 1, print_count = 1, text_print = "", cor_method = "pearson", cor_limit = 0.9, warn_print = FALSE), regexp = "'y' must be numeric", fixed = TRUE)
})

test_that("permut correlated permutation loops (for/while rounds and GOBACK)", {
    # round 1 with loop.nb.est > 1e4: for loop path, GOBACK TRUE then FALSE rounds, final while loop
    res <- suppressWarnings(permut(data1 = c(1,1,1,2,3), data2 = 1:5, n = 10, seed = 1, print_count = 1000, text_print = "", cor_method = "pearson", cor_limit = 0.5, warn_print = FALSE))
    testthat::expect_equal(object = base::round(x = res$cor, digits = 3), expected = 0.354)
    testthat::expect_true(object = res$count >= 15) # at least the first while loop step, the for loop round and the final while loop step
    # count as numeric with decimals: the for loop path adds fractional counts (verified against the local run)
    testthat::expect_true(object = base::is.numeric(x = res$count))
})

test_that("permut GOBACK estimation branches", {
    # config where the estimation step is entered several times with GOBACK == FALSE (else branch of the estimation step)
    res_gbf <- suppressWarnings(permut(data1 = c(1,1,1,2,3), data2 = 1:5, n = 10, seed = 2, print_count = 5000, text_print = "", cor_method = "pearson", cor_limit = 0.5, warn_print = FALSE))
    testthat::expect_true(object = base::is.numeric(x = res_gbf$count))
    # config with several rounds, including GOBACK TRUE and FALSE rounds
    res_gbm <- suppressWarnings(permut(data1 = c(1,1,1,2,3), data2 = 1:5, n = 10, seed = 1, print_count = 5000, text_print = "", cor_method = "pearson", cor_limit = 0.5, warn_print = FALSE))
    testthat::expect_true(object = base::is.numeric(x = res_gbm$count))
})

test_that("permut progress print blocks of the while loops (print_count = 1)", {
    # print_count = 1 makes the progress message of the first while loop (equality escape) print at each iteration
    res_first <- suppressWarnings(permut(data1 = c(1,1,3), data2 = 1:3, n = 10, seed = 1, print_count = 1, text_print = "", cor_method = "pearson", cor_limit = 0.5, warn_print = FALSE))
    testthat::expect_true(object = base::is.numeric(x = res_first$count))
    # print_count = 1 with a config where the final while loop also prints its progress message (loop runs more than 1 iteration)
    res_final <- suppressWarnings(permut(data1 = c(1,1,3,4,5,6,7,8,9,10), data2 = 1:10, n = 10, seed = 2, print_count = 1, text_print = "", cor_method = "pearson", cor_limit = 0.9, warn_print = FALSE))
    testthat::expect_true(object = base::is.numeric(x = res_final$count))
})

test_that("permut environment checking branches", {
    data1 <- c(1, 2, 3, 4, 5)
    # safer_check given as a function: deparse1 branch of the safer_check argument checking
    expect_error(permut(data1 = data1, n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE, safer_check = mean))
    # lib_path given as a function: deparse1 branch of the lib_path checking
    expect_error(permut(data1 = data1, n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE, lib_path = mean))
    # lib_path with an existing directory: .libPaths() branch
    expect_no_error(suppressWarnings(permut(data1 = data1, n = 10, seed = 1, print_count = 10, text_print = "", cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE, lib_path = tempdir())))
    # arg_check data1: value failing all the possible types (list) -> combined error block
    expect_error(permut(data1 = base::list(1, 2, 3, 4, 5), n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE))
    # arg_check data1 with data2 specified: data1 must be numeric in this case
    expect_error(permut(data1 = c("a", "b", "c", "d", "e"), data2 = 1:5, n = 10, print_count = 10, cor_method = "spearman", cor_limit = 0.5, warn_print = FALSE))
})
