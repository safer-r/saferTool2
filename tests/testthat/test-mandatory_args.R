# This file tests the "mandatory arg of safer-r functions" checking block present in every saferTool2 function
# This block fires only if the function is written without one of the mandatory formals (lib_path, safer_check, error_text for public functions)
# It cannot be triggered by user values: the test uses a temporary function built from the same body but with the mandatory formal removed from formals()
# No # nocov markers are used, and no R source files are modified: the block is actually executed by the tests

testthat::test_that("mandatory arg block of saferTool2 functions (public functions)", {

    ## data argument values
    # full calls with all formals except the mandatory one to drop (lib_path for all these functions)
    full_calls <- base::list(
        codon2aa = base::quote(codon2aa(data = "atg", display = FALSE, safer_check = FALSE, error_text = "")),
        codon_finder = base::quote(codon_finder(pos = 1:3, begin = 1L, end = 3L, safer_check = FALSE, error_text = "")),
        permut = base::quote(permut(data1 = c(0,0,0,0,0), data2 = NULL, n = 1, seed = 1, print.count = 1, text.print = "", cor.method = "spearman", cor.limit = 0.5, warn.print = FALSE, safer_check = FALSE, error_text = "")),
        slide = base::quote(slide(data = 1:10, window.size = 5, step = 2, from = NULL, to = NULL, FUN = length, args = NULL, boundary = "left", parall = FALSE, thread.nb = NULL, print.count = Inf, res.path = NULL, verbose = FALSE, safer_check = FALSE, error_text = "")),
        trim = base::quote(trim(data = 1:10, displayed.nb = 5, single.value.display = FALSE, trim.method = "mean.sd", trim.cutoffs = c(0.05, 0.95), interval.scale.disp = FALSE, down.space = 1, left.space = 1, up.space = 1, right.space = 1, orient = 1, dist.legend = 0.5, box.type = "n", amplif.label = 1, amplif.axis = 1, std.x.range = TRUE, std.y.range = TRUE, cex.pt = 1, col.box = "grey", x.nb.inter.tick = 1, y.nb.inter.tick = 1, tick.length = 0.5, sec.tick.length = 0.3, corner.text = "", amplif.legend = 1, corner.text.size = 1, trim.return = FALSE, seed = 1, safer_check = FALSE, error_text = ""))
    )
    ## end data argument values

    ## initialization of tests
    # none
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    ######## mandatory arg of safer-r functions
    for(fn in base::names(x = full_calls)){
        f <- base::get(x = fn, envir = base::asNamespace("saferTool2"))
        fml <- base::formals(fun = f)
        fml_dropped <- fml[base::setdiff(x = base::names(x = fml), y = "lib_path")] # drop lib_path: the block must fire
        tempo_env <- base::new.env(parent = base::environment(fun = f))
        tempo_env[[fn]] <- base::as.function(base::c(fml_dropped, base::list(base::body(f))), envir = tempo_env)
        cl <- full_calls[[fn]]
        res <- base::tryCatch(
            expr = base::eval(expr = cl, envir = tempo_env),
            error = function(e){base::conditionMessage(e)}
        )
        testthat::expect_true(object = base::grepl(x = res, pattern = "FOLLOWING ARGUMENT IS MANDATORY IN SAFER-R FUNCTIONS:", fixed = TRUE), label = base::paste0(fn, ": mandatory-args block message"))
        testthat::expect_true(object = base::grepl(x = res, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path reported as missing mandatory arg"))
        testthat::expect_false(object = base::grepl(x = res, pattern = "object '", fixed = TRUE), label = base::paste0(fn, ": no error before the mandatory-args block"))
    }
    ######## end mandatory arg of safer-r functions

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # restore check: with all formals present, the block must NOT fire
    testthat::expect_no_error(object = codon2aa(data = "atg", display = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(object = codon_finder(pos = 1:3, begin = 1L, end = 3L, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(object = trim(data = 1:10, trim.return = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
