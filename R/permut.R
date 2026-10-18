#' @title permut
#' @description
#' Reorder the elements of the data1 vector by flipping 2 randomly selected consecutive positions either:
#'
#' - n times (when n is precised) or.
#'
#' - until the correlation between data1 and data2 decreases down to the cor_limit (0.2 by default). See cor_limit below to deal with negative correlations.
#'
#' Example of consecutive position flipping: ABCD -> BACD -> BADC, etc.
#'
#' Designed for discrete values, but works also for continuous values.
#' @param data1 A vector of at least 2 elements. Must be numeric if data2 is specified.
#' @param data2 A numeric vector of same length as data1.
#' @param n Single numeric value of times "flipping 2 randomly selected consecutive positions" (integers expected in practice). Ignored if data2 is specified.
#' @param seed Single numeric number used by set.seed() (integers expected in practice). Write NULL if random result is required, a number otherwise. BEWARE: if not NULL, permut() will systematically return the same result when the other parameters keep the same settings.
#' @param print_count Single positive numeric value (integers expected in practice). Print a working progress message every print_count during loops. BEWARE: can increase substentially the time to complete the process using a small value, like 10 for instance. Use Inf is no loop message desired (Inf only allowed when data2 is NULL).
#' @param text_print Single character string indicating the optional message to add to the working progress message every print_count loop.
#' @param cor_method Correlation method. Either "pearson", "kendall" or "spearman". Ignored if data2 is not specified.
#' @param cor_limit Single positive numeric proportion fixing the correlation limit. Ignored if data2 is not specified. Compute the correlation between data1 and data2, permute the data1 values, and stop the permutation process when the correlation between data1 and data2 decreases down below the cor limit value (0.2 by default). If cor(data1, data2) is negative, then -cor_limit is used and the process stops until the correlation between data1 and data2 increases up over cor_limit (-0.2 by default). BEWARE: write a positive cor_limit even if cor(data1, data2) is known to be negative. The function will automatically uses -cor_limit. If the initial correlation is already below cor_limit (positive correlation) or over -cor_limit (negative correlation), then the data1 value positions are completely randomized (correlation between data1 and data2 is expected to be 0).
#' @param warn_print Single logical value. Print warnings at the end of the execution? No print if no warning messages
#' @param safer_check Single logical value. Perform some "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns
#' A list containing:
#'
#' - $data: the modified vector.
#'
#' - $warn: potential warning messages (in case of negative correlation when data2 is specified). NULL if non warning message.
#'
#' - $cor: a spearman correlation between the initial positions (1:length(data1) and the final positions if data2 is not specified and the final correlation between data1 and data2 otherwise, according to cor_method.
#'
#' - $count: the number of loops used.
#' @details
#' WARNINGS
#'
#' see # https://www.r-bloggers.com/strategies-to-speedup-r-code/ for code speedup
#'
#' The random switch of non consecutive positions (ABCD -> DBCA for instance) does not work very well as the correlation is quickly obtained but the initial vector structure is mainly kept (no much order).
#'
#' Ths code would be: pos <- ini.pos[1:2] ; pos <- sample.int(n = n , size = 2, replace = FALSE) ; tempo.pos[pos] <- tempo.pos[rev(pos)]
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' permut(data1 = 1:10, data2 = 10:1, seed = 1, print_count = 1e4, text_print = "", cor_method = "spearman", cor_limit = 0.7)
#'
#' permut(data1 = c(0,0,0,0,0), n = 5, data2 = NULL, seed = 1, print_count = 1e4, cor_limit = 0.5)
#' @importFrom saferDev arg_check
#' @importFrom saferTool round2
#' @importFrom lubridate seconds_to_period
#' @export
permut <- function(
    data1,
    data2 = NULL,
    n = NULL,
    seed = NULL,
    print_count = 10,
    text_print = "",
    cor_method = "spearman",
    cor_limit = 0.2,
    warn_print = FALSE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
){

    # DEBUGGING
    # data1 = LETTERS[1:5] ; data2 = NULL ; n = 1e6 ; seed = NULL ; print_count = 1e3 ; text_print = "" ; cor_method = "spearman" ; cor_limit = 0.2 ; warn_print = TRUE ; safer_check = TRUE ; lib_path = NULL ; error_text = ""
    # data1 = LETTERS[1:5] ; data2 = NULL ; n = 10 ; seed = 22 ; print_count = 10 ; text_print = "" ; cor_method = "spearman" ; cor_limit = 0.2 ; warn_print = TRUE ; safer_check = TRUE ; lib_path = NULL ; error_text = ""
    # data1 = 101:110 ; data2 = 21:30 ; n = 10 ; seed = 22 ; print_count = 10 ; text_print = "" ; cor_method = "spearman" ; cor_limit = 0.2 ; warn_print = TRUE ; safer_check = TRUE ; lib_path = NULL ; error_text = ""
    # data1 = 1:1e3 ; data2 = 1e3:1 ; n = 20 ; seed = 22 ; print_count = 1e6 ; text_print = "" ; cor_method = "spearman" ; cor_limit = 0.5 ; warn_print = TRUE ; safer_check = TRUE ; lib_path = NULL ; error_text = ""

    #### package name
    package_name <- "saferTool2" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE)
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names,
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of values if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 0)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)),
        function_name,
        collapse = NULL,
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name,
        base::ifelse(test = error_text == "", yes = ".", no = error_text),
        "\n\n",
        collapse = NULL,
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    intern_error_text_start <- base::paste0(
        package_function_name,
        base::ifelse(test = error_text == "", yes = ".", no = error_text),
        "\n\n",
        collapse = NULL,
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present in your main code
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start,
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(x = tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start,
            "FOLLOWING ARGUMENT",
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"),
            " MANDATORY IN SAFER-R FUNCTIONS:\n",
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

    ######## arg with no default values
    # optional section: remove the code if none of your arguments has no default value
    no_def_args <- base::c(
        "data1"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(no_def_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start,
            "FOLLOWING ARGUMENT",
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"),
            " NO DEFAULT VALUE AND REQUIRE ONE:\n",
            base::paste0(no_def_args[tempo], collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1",
        # "data2", # inactivated because can be null
        # "n", # inactivated because can be null
        # "seed", # inactivated because can be null
        "print_count",
        "text_print",
        "cor_method",
        "cor_limit",
        "warn_print",
        "safer_check"
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is permut's frame.
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start,
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"),
            " CANNOT BE NULL:\n",
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1",
        "data2",
        "n",
        "seed",
        "print_count",
        # "text_print", # inactivated because can be ""
        "cor_method",
        "cor_limit",
        "warn_print",
        "safer_check",
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval,
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                },
                simplify = TRUE,
                USE.NAMES = TRUE
            ),
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start,
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"),
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n",
                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # Mandatory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval,
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ),
                FUN = function(x){
                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                },
                simplify = TRUE,
                USE.NAMES = TRUE
            ),
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start,
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"),
                " CANNOT BE MADE OF NA ONLY:\n",
                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        if(base::all(base::mode(x = safer_check) == "function", na.rm = TRUE)){
            safer_check <- base::deparse1(expr = safer_check, collapse = "", width.cutoff = 500L)
        }
        tempo_cat <- base::paste0(
            error_text_start,
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n",
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    # always executed: lib_path is always checked, whatever the value of safer_check
    if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
            if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
            }
            tempo_cat <- base::paste0(
                error_text_start,
                "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n",
                base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
            tempo_log <- ! base::dir.exists(paths = lib_path)
            tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
            tempo_cat_b[tempo_cat_b == ""] <- "\"\""
            tempo_cat <- base::paste0(
                error_text_start,
                "THE DIRECTORY PATH",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""),
                " INDICATED IN THE lib_path ARGUMENT DO",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"),
                " NOT EXIST:\n",
                base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
        }
    }else{
        lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = , envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check", # write each function preceeded by their package name
                "saferTool::round2",
                "lubridate::seconds_to_period"
                # end functions required in this code
                # internal functions required in this code
                ,
                "saferDev:::.base_op_check"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = , envir = )
    # add here in the internal functions that are used in your main code (copy-paste the line above and replace .base_op_check by the name of the internal function
    # not mandatory if your function is not designed for submission to the CRAN

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # data1 can be a vector of any of the main atomic types (character, numeric, logical). Since "vector" cannot be used as a class/typeof/mode value (see saferDev::arg_check() doc), several arg_check() calls are performed and the error is triggered only if all of them fail
    tempo1 <- saferDev::arg_check(data = data1, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    tempo2 <- saferDev::arg_check(data = data1, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    tempo3 <- saferDev::arg_check(data = data1, class = NULL, typeof = "logical", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE){
        tempo.cat <- base::paste0(
            arg_check_error_text,
            "data1 ARGUMENT MUST BE A VECTOR MADE OF CHARACTER OR NUMERIC OR LOGICAL VALUES",
            collapse = NULL,
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo.cat)
        argum_check <- base::c(argum_check, TRUE)
        checked_arg_names <- base::c(checked_arg_names, tempo1$object.name)
    }
    if(base::is.null(x = data2) == FALSE){ # data1 must be numeric if data2 specified
        tempo5 <- saferDev::arg_check(data = data1, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo5$problem == TRUE){
            tempo.cat <- base::paste0(
                arg_check_error_text,
                "data1 ARGUMENT MUST BE A VECTOR. IF THE data2 ARGUMENT IS SPECIFIED, data1 MUST BE A VECTOR OF NUMERIC VALUES",
                collapse = NULL,
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo.cat)
            argum_check <- base::c(argum_check, TRUE)
            checked_arg_names <- base::c(checked_arg_names, tempo1$object.name)
        }
    }
    if(base::is.null(x = data2) == FALSE){
        tempo <- saferDev::arg_check(data = data2, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if(base::is.null(x = n) == FALSE){
        tempo <- saferDev::arg_check(data = n, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if(base::is.null(x = seed) == FALSE){
        tempo <- saferDev::arg_check(data = seed, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = print_count, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = text_print, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = cor_method, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, options = base::c("pearson", "kendall", "spearman"), double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = cor_limit, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = TRUE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = warn_print, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    tempo_arg <- base::c(
        # "text_print" # inactivated because can be ""
        "cor_method" # Ok if the argument is NULL, tempo_log will be FALSE
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is permut's frame.
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ",
            intern_error_text_start,
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n",
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"),
            " NOT CLASS \"character\":\n",
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
            "\nIf saferMake::saferMake() HAS BEEN USED TO MODIFY YOUR FUNCTION, PLEASE RESTART WITH YOUR MAIN CODE AND SELECT PROPER CLASS, TYPE AND/OR MODE FOR YOUR ARGUMENTS\n.HERE, ARGUMENTS ARE TYPE:\n",
            base::paste0(base::sapply(X = tempo_arg, FUN = function(x){base::paste0(x, ': \"', base::typeof(x = base::get(x = x, pos = -1L, envir = base::parent.frame(n = 3), mode = "any", inherits = FALSE)), '"', collapse = NULL, recycle0 = FALSE)}), collapse = "\n", recycle0 = FALSE), # parent.frame(n = 3) because the get() is evaluated inside the FUN closure of sapply. But sapply(FUN) calls lapply(FUN) internally !! So parent.frame(n = 1) is the internal lapply() frame called by sapply, parent.frame(n = 2) is sapply's frame, parent.frame(n = 3) is permut's frame. NB: paste0() and typeof() are primitives and add no frame.
            intern_error_text_end,
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is permut's frame. # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            # nocov start
            # codecov inactivated because cor_method is the only argument of tempo_arg and it is already checked by arg_check() options ("" cannot reach this block).
            tempo_cat <- base::paste0(
                error_text_start,
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"),
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    # Warning: seeding is always at the .GlobalEnv level, whenever the seeding is applied inside a function, another envir, etc.
    if (base::exists(x = ".Random.seed", where = -1, envir = .GlobalEnv, frame = , mode = "any", inherits = TRUE)) {
        # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo.random.seed <- .Random.seed
        base::on.exit(expr = base::assign(x = ".Random.seed", value = tempo.random.seed, pos = -1, envir = .GlobalEnv, inherits = FALSE, immediate = TRUE), add = TRUE, after = TRUE)
    }else{
        # nocov start
        # codecov inactivated because .Random.seed always exists in the testing environment (testthat sets it): this branch can only be reached in a fresh R session before any random operation.
        base::on.exit(expr = base::set.seed(seed = NULL, kind = NULL, normal.kind = NULL, sample.kind = NULL), add = TRUE, after = TRUE) # inactivate seeding -> return to complete randomness
        # nocov end
    }
    base::set.seed(seed = seed, kind = NULL, normal.kind = NULL, sample.kind = NULL) # seed value is the seed argument of the function
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ini_warning_length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE) # registered here so that warning.length is restored even if an error occurs before the warning-output on.exit() registrations below
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    if(base::length(x = data1) < 2){
        tempo_cat <- base::paste0(
            error_text_start,
            "data1 ARGUMENT MUST BE A VECTOR OF MINIMUM LENGTH 2. HERE IT IS: ",
            base::length(x = data1),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::is.null(x = data2) == FALSE){
        if(base::length(x = data1) != base::length(x = data2)){
            tempo_cat <- base::paste0(
                error_text_start,
                "data1 AND data2 MUST BE VECTOR OF SAME LENGTH. HERE IT IS ",
                base::length(x = data1),
                " AND ",
                base::length(x = data2),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }else if(base::is.null(x = n)){
        tempo_cat <- base::paste0(
            error_text_start,
            "n ARGUMENT CANNOT BE NULL IF data2 ARGUMENT IS NULL",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code

    ini_date <- base::Sys.time() # time of process begin, converted into seconds
    ini_time <- base::as.numeric(ini_date) # time of process begin, converted into seconds
    ini.pos <- 1:base::length(data1) # positions of data1 before permutation loops
    tempo.pos <- ini.pos # positions of data1 that will be modified during loops
    # pos.selec.seq <- ini.pos[-length(data1)] # selection of 1 position in initial position, without the last because always up permutation (pos -> pos+1 & pos+1 -> pos)
    pos.selec.seq.max <- base::length(ini.pos) - 1 # max position (used by base::sample.int() function). See  below for - 1
    count <- 0
    round <- 0
    BREAK <- FALSE
    tempo.cor <- 0
    if(base::is.null(data2)){
        if(base::length(base::table(data1)) == 1L){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", base::names(base::table(data1)), collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }else{
            if(print_count > n){
                print_count <- n
            }
           base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "FOR LOOP OF ", n, " LOOPS INITIATED | LOOP COUNT: ", base::format(count, big.mark=","), collapse = NULL, recycle0 = FALSE))
            print.count.loop <- base::logical(length = print_count)
            print.count.loop[base::length(print.count.loop)] <- TRUE # not this to avoid long vector, but not forget to reset during printing: print.count.loop[(1: trunctrunc(n / print_count) * print_count)] <- TRUE # counter to speedup
            count.loop <- 0
            pos <- base::sample.int(n = pos.selec.seq.max , size = print_count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
            tempo.date.loop <- base::Sys.time()
            tempo.time.loop <- base::as.numeric(tempo.date.loop)
            for(i3 in 1:n){
                count.loop <- count.loop + 1
                pos2 <- pos[count.loop] # selection of 1 position
                tempo.pos[base::c(pos2 + 1, pos2)] <- tempo.pos[base::c(pos2, pos2 + 1)]
                if(print.count.loop[count.loop]){
                    count.loop <- 0
                    pos <- base::sample.int(n = pos.selec.seq.max , size = print_count, replace = TRUE) # BEWARE: never forget to resample here
                    tempo.time <- base::as.numeric(base::Sys.time())
                    tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(x = tempo.time - tempo.time.loop)), safer_check = FALSE)
                    final.loop <- (tempo.time - tempo.time.loop) / i3 * n # expected duration in seconds
                    final.exp <- base::as.POSIXct(final.loop, origin = tempo.date.loop)
                    base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "FOR LOOP ", i3, " / ", n, " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp, collapse = NULL, recycle0 = FALSE))
                }
            }
            count <- count + n # out of the loop to speedup
            base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "FOR LOOP ENDED | LOOP COUNT: ", base::format(count, big.mark=","), collapse = NULL, recycle0 = FALSE))
            base::cat("\n\n")
        }
    }else{
        if(base::length(base::table(data1)) == 1L){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", base::names(base::table(data1)), collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            tempo.cor <- 1
        }else if(base::length(base::table(data2)) == 1L){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NO PERMUTATION PERFORMED BECAUSE data2 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", base::names(base::table(data2)), collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            tempo.cor <- 1
        }else{
            cor.ini <- stats::cor(x = data1, y = data2, use = "pairwise.complete.obs", method = cor_method)
            tempo.cor <- cor.ini # correlation that will be modified during loops
            neg.cor <- FALSE
            if(tempo.cor < 0){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") INITIAL ", base::toupper(cor_method), " CORRELATION BETWEEN data1 AND data2 HAS BEEN DETECTED AS NEGATIVE: ", tempo.cor, ". THE LOOP STEPS WILL BE PERFORMED USING POSITIVE CORRELATIONS BUT THE FINAL CORRELATION WILL BE NEGATIVE", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                neg.cor <- TRUE
                tempo.cor <- base::abs(tempo.cor)
                cor.ini <- base::abs(cor.ini)
            }
            if(tempo.cor < cor_limit){ # randomize directly all the position to be close to correlation zero
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") INITIAL ABSOLUTE VALUE OF THE ", base::toupper(cor_method), " CORRELATION ", saferTool::round2(tempo.cor, safer_check = FALSE), " BETWEEN data1 AND data2 HAS BEEN DETECTED AS BELOW THE CORRELATION LIMIT PARAMETER ", cor_limit, "\nTHE data1 SEQUENCE HAS BEEN COMPLETELY RANDOMIZED TO CORRESPOND TO CORRELATION ZERO", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                for(i4 in 1:5){ # done 5 times to be sure of the complete randomness
                    tempo.pos <- base::sample(x = tempo.pos, size = base::length(tempo.pos), replace = FALSE)
                }
                count <- count + 5 # out of the loop to speedup
            }else{
                # smallest correlation decrease
                count <- count + 1 # 1 and not 0 because already 1 performed just below
                pos <- base::sample.int(n = pos.selec.seq.max , size = 1, replace = TRUE) # selection of 1 position # pos.selec.seq.max  because selection of 1 position in initial position, without the last because always up permutation (pos -> pos+1 & pos+1 -> pos)
                tempo.pos[base::c(pos + 1, pos)] <- tempo.pos[base::c(pos, pos + 1)]
                tempo.cor <- base::abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor_method))
                smallest.cor.dec <- cor.ini - tempo.cor
                # end smallest correlation decrease
                # going out of tempo.cor == cor.ini
                base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "CORRELATION DECREASE AFTER A SINGLE PERMUTATION: ", saferTool::round2(smallest.cor.dec, 4, safer_check = FALSE), collapse = NULL, recycle0 = FALSE))
                base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "FIRST WHILE LOOP STEP -> GOING OUT FROM EQUALITY | LOOP COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor_limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), collapse = NULL, recycle0 = FALSE))
                print.count.loop <- base::logical(length = print_count)
                print.count.loop[base::length(print.count.loop)] <- TRUE # counter to speedup
                count.loop <- 0 #
                pos <- base::sample.int(n = pos.selec.seq.max , size = print_count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                tempo.date.loop <- base::Sys.time()
                tempo.time.loop <- base::as.numeric(tempo.date.loop)
                while(tempo.cor == cor.ini){ # to be out of equality between tempo.cor and cor.ini at the beginning (only valid for very long vector)
                    count <- count + 1
                    count.loop <- count.loop + 1
                    pos2 <- pos[count.loop]
                    tempo.pos[base::c(pos2 + 1, pos2)] <- tempo.pos[base::c(pos2, pos2 + 1)]
                    tempo.cor <- base::abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor_method))
                    if(print.count.loop[count.loop]){
                        count.loop <- 0
                        pos <- base::sample.int(n = pos.selec.seq.max , size = print_count, replace = TRUE) # BEWARE: never forget to resample here
                        tempo.time <- base::as.numeric(base::Sys.time())
                        tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(x = tempo.time - tempo.time.loop)), safer_check = FALSE)
                        base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "FIRST WHILE LOOP STEP", base::format(count.loop, big.mark=","), " / ? | COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor_limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), " | TIME SPENT: ", tempo.lapse, collapse = NULL, recycle0 = FALSE))
                    }
                }
                tempo.time <- base::as.numeric(base::Sys.time())
                tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(x = tempo.time - ini_time)), safer_check = FALSE)
                base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "FIRST WHILE LOOP STEP END | LOOP COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor_limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), " | TOTAL SPENT TIME: ", tempo.lapse, collapse = NULL, recycle0 = FALSE))
                if(tempo.cor < cor_limit){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE FIRST FOR & WHILE LOOP STEPS HAVE BEEN TOO FAR AND SUBSEQUENT LOOP STEPS WILL NOT RUN", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
                # end going out of tempo.cor == cor.ini
                # estimation of the average correlation decrease per loop on x loops and for loop execution
                base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "WHILE/FOR LOOPS INITIATION | LOOP COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor_limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), collapse = NULL, recycle0 = FALSE))
                count.est <- 1e5
                first.round <- TRUE
                GOBACK <- FALSE
                while(tempo.cor > cor_limit){
                    round <- round + 1
                    # estimation step
                    if(first.round == TRUE){
                        first.round <- FALSE
                        cor.dec.per.loop <- base::numeric(length = 5)
                        loop.nb.est <- Inf
                        cor.est.ini <- tempo.cor
                        cor.est <- base::numeric(length = 5)
                        for(i6 in 1:5){ # connected to cor.dec.per.loop
                            tempo.pos.est <- tempo.pos
                            pos <- base::sample.int(n = pos.selec.seq.max , size = count.est, replace = TRUE) # selection of n position
                            for(i7 in 1:count.est){
                                pos2 <- pos[i7] # selection of 1 position
                                tempo.pos.est[base::c(pos2 + 1, pos2)] <- tempo.pos.est[base::c(pos2, pos2 + 1)]
                            }
                            tempo.cor.est <- base::abs(stats::cor(x = data1[tempo.pos.est], y = data2, use = "pairwise.complete.obs", method = cor_method))
                            cor.est[i6] <- tempo.cor.est
                            tempo.cor.dec.per.loop <- (cor.est.ini - tempo.cor.est) / count.est # correlation decrease per loop
                            if(base::is.na(tempo.cor.dec.per.loop) | ! base::is.finite(tempo.cor.dec.per.loop)){
                                # nocov start
                                # codecov inactivated because it is an internal error, impossible to cover with argument values.
                                tempo.cat <- base::paste0(
                                    "INTERNAL ERROR IN THE MAIN CODE OF ",
                                    intern_error_text_start,
                                    "CODE INCONSISTENCY 2\ncor.est.ini: ",
                                    cor.est.ini,
                                    "\ntempo.cor.est: ",
                                    tempo.cor.est,
                                    intern_error_text_end,
                                    collapse = NULL,
                                    recycle0 = FALSE
                                )
                                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                                # nocov end
                            }
                            cor.dec.per.loop[i6] <- tempo.cor.dec.per.loop
                        }
                        cor.est <- cor.est[base::which.max(cor.dec.per.loop)] # max to avoid to go to far with for loop (tempo.cor below tempo.limit)
                        cor.dec.per.loop <- base::max(cor.dec.per.loop, na.rm = TRUE) # max to avoid to go to far with for loop (tempo.cor below tempo.limit)
                        loop.nb.est <- saferTool::round2((tempo.cor - cor_limit) / cor.dec.per.loop, safer_check = FALSE)
                    }else{
                        if(GOBACK == TRUE){
                            loop.nb.est <- saferTool::round2(loop.nb.est / 2, safer_check = FALSE)
                        }else{
                            cor.dec.per.loop <- (cor.ini - tempo.cor) / count
                            loop.nb.est <- saferTool::round2((tempo.cor - cor_limit) / cor.dec.per.loop, safer_check = FALSE)
                        }
                    }
                    # end estimation step
                    # loop step
                    if(base::is.na(loop.nb.est) | ! base::is.finite(loop.nb.est)){
                        # nocov start
                        # codecov inactivated because it is an internal error, impossible to cover with argument values.
                        tempo.cat <- base::paste0(
                            "INTERNAL ERROR IN THE MAIN CODE OF ",
                            intern_error_text_start,
                            "CODE INCONSISTENCY 1\nloop.nb.est: ",
                            loop.nb.est,
                            "\ncor.ini: ",
                            cor.ini,
                            "\ntempo.cor: ",
                            tempo.cor,
                            "\ncor.limit: ",
                            cor_limit,
                            "\ncor.dec.per.loop: ",
                            cor.dec.per.loop,
                            intern_error_text_end,
                            collapse = NULL,
                            recycle0 = FALSE
                        )
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                        # nocov end
                    }else if(loop.nb.est > 1e4){ # below -> leave the while loop
                        tempo.pos.secu <- tempo.pos
                        count.secu <- count
                        tempo.cor.secu <- tempo.cor
                        base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "INITIAL SETTINGS BEFORE ROUND: ", round, " | LOOP COUNT: ", base::format(count, big.mark=","), " | GO BACK: ", GOBACK, " | LOOP NUMBER ESTIMATION: ", base::format(loop.nb.est, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor_limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), collapse = NULL, recycle0 = FALSE))
                        print.count.loop <- base::logical(length = print_count)
                        print.count.loop[base::length(print.count.loop)] <- TRUE # not this to avoid long vector, but not forget to reset during printing: print.count.loop[(1:base::trunc(n / print_count) * print_count)] <- TRUE # counter to speedup
                        count.loop <- 0
                        pos <- base::sample.int(n = pos.selec.seq.max , size = print_count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                        tempo.date.loop <- base::Sys.time()
                        tempo.time.loop <- base::as.numeric(tempo.date.loop)
                        for(i6 in 1:loop.nb.est){
                            count.loop <- count.loop + 1
                            pos2 <- pos[count.loop] # selection of 1 position
                            tempo.pos[base::c(pos2 + 1, pos2)] <- tempo.pos[base::c(pos2, pos2 + 1)]
                            if(print.count.loop[count.loop]){
                                count.loop <- 0
                                pos <- base::sample.int(n = pos.selec.seq.max , size = print_count, replace = TRUE) # BEWARE: never forget to resample here
                                tempo.time <- base::as.numeric(base::Sys.time())
                                tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(x = tempo.time - tempo.time.loop)), safer_check = FALSE)
                                final.loop <- (tempo.time - tempo.time.loop) / i6 * loop.nb.est # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
                                final.exp <- base::as.POSIXct(final.loop, origin = tempo.date.loop)
                                base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "FOR LOOP | ROUND ", round, " | LOOP: ", base::format(i6, big.mark=","), " / ", base::format(loop.nb.est, big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp, collapse = NULL, recycle0 = FALSE))
                            }
                        }
                        count <- count + loop.nb.est # out of the loop to speedup
                        tempo.cor <- base::abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor_method))
                        if(tempo.cor > tempo.cor.secu | ((tempo.cor - cor_limit) < 0 & base::abs(tempo.cor - cor_limit) > smallest.cor.dec * saferTool::round2(base::log10(base::max(ini.pos, na.rm = TRUE)), safer_check = FALSE))){
                            GOBACK <- TRUE
                            tempo.pos <- tempo.pos.secu
                            count <- count.secu
                            tempo.cor <- tempo.cor.secu
                        }else{
                            GOBACK <- FALSE
                        }
                    }else{
                        base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "FINAL WHILE LOOP | LOOP COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor_limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), collapse = NULL, recycle0 = FALSE))
                        print.count.loop <- base::logical(length = print_count)
                        print.count.loop[base::length(print.count.loop)] <- TRUE # counter to speedup
                        count.loop <- 0 #
                        pos <- base::sample.int(n = pos.selec.seq.max , size = print_count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                        tempo.cor.loop <- tempo.cor
                        tempo.date.loop <- base::Sys.time()
                        tempo.time.loop <- base::as.numeric(tempo.date.loop)
                        while(tempo.cor > cor_limit){
                            count <- count + 1
                            count.loop <- count.loop + 1
                            pos2 <- pos[count.loop]
                            tempo.pos[base::c(pos2 + 1, pos2)] <- tempo.pos[base::c(pos2, pos2 + 1)]
                            tempo.cor <- base::abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor_method))
                            if(print.count.loop[count.loop]){
                                count.loop <- 0
                                pos <- base::sample.int(n = pos.selec.seq.max , size = print_count, replace = TRUE) # BEWARE: never forget to resample here
                                tempo.time <- base::as.numeric(base::Sys.time())
                                tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(x = tempo.time - tempo.time.loop)), safer_check = FALSE)
                                final.loop <- (tempo.time - tempo.time.loop) / (tempo.cor.loop - tempo.cor) * (tempo.cor - cor_limit) # expected duration in seconds # tempo.cor.loop - tempo.cor always positive and tempo.cor decreases progressively starting from tempo.cor.loop
                                final.exp <- base::as.POSIXct(final.loop, origin = tempo.date.loop)
                                base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "WHILE LOOP | LOOP NB: ", base::format(count.loop, big.mark=","), " | COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor_limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp, collapse = NULL, recycle0 = FALSE))
                            }
                        }
                    }
                }
                tempo.time <- base::as.numeric(base::Sys.time())
                tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(x = tempo.time - ini_time)), safer_check = FALSE)
                base::cat(base::paste0("\n", base::ifelse(test = text_print == "", yes = "", no = base::paste0(text_print, " | ", collapse = NULL, recycle0 = FALSE)), "WHILE/FOR LOOPS END | LOOP COUNT: ", base::format(count, big.mark=","), " | NB OF ROUNDS: ", round, " | CORRELATION LIMIT: ", saferTool::round2(cor_limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), " | TOTAL SPENT TIME: ", tempo.lapse, collapse = NULL, recycle0 = FALSE))
            }
            tempo.cor <- base::ifelse(test = neg.cor == TRUE, yes = -tempo.cor, no = tempo.cor)
        }
    }
    base::cat("\n\n")

    #### end main code

    #### warning output
    # must be before return()
    if(warn_print == TRUE & ! base::is.null(x = warn)){
        base::on.exit(
            expr = base::warning(
                base::paste0(
                    base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
                    warn,
                    collapse = NULL,
                    recycle0 = FALSE
                ), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL
            ), add = TRUE, after = TRUE
        )
    }
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
    #### end warning output

    #### output
    output <- base::list(data = data1[tempo.pos], warn = warn, cor = if(base::is.null(data2)){stats::cor(ini.pos, tempo.pos, method = "spearman")}else{tempo.cor}, count = count)
    base::return(output)
    #### end output

}
