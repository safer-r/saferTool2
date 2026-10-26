#' @title slide
#' @description
#' Return a computation made on a vector using a sliding window.
#' @param data Vector, matrix, table or array of numeric values (mode must be numeric). Inf and NA not allowed.
#' @param window_size Single numeric value indicating the width of the window sliding across data (in the same unit as data value).
#' @param step Single numeric value indicating the step between each window (in the same unit as data value). Cannot be larger than window_size.
#' @param from Single numeric value of the left boundary of the first sliding window. If NULL, min(data) is used. The first window will strictly have from or min(data) as left boundary.
#' @param to Single numeric value of the right boundary of the last sliding window. If NULL, max(data) is used. Warning: (1) the final last window will not necessary have to|max(data) as right boundary. In fact the last window will be the one that contains to|max(data) for the first time, i.e., min[from|min(data) + window_size + n * step >= to|max(data)]; (2) In fact, the >= in min[from|min(data) + window_size + n * step >= to|max(data)] depends on the boundary argument (>= for "right" and > for "left"); (3) to have the rule (1) but for the center of the last window, use to argument as to = to|max(data) + window_size / 2.
#' @param FUN Function or single character string indicating the name of the function to apply in each window. Example of function: FUN = mean. Example of character string: FUN = "mean".
#' @param args Single character string of additional arguments of FUN (separated by a comma between the quotes). Example args = "na.rm = TRUE" for FUN = mean. Ignored if NULL.
#' @param boundary Either "left" or "right". Indicates if the sliding window includes values equal to left boundary and exclude values equal to right boundary ("left") or the opposite ("right").
#' @param parall Single logical value. Force parallelization ?
#' @param thread_nb Single numeric value indicating the number of threads to use if ever parallelization is required. If NULL, all the available threads will be used. Ignored if parall is FALSE.
#' @param print_count Single numeric value (integers expected in practice). Print a working progress message every print_count during loops. BEWARE: can increase substantially the time to complete the process using a small value, like 10 for instance. Use Inf is no loop message desired.
#' @param res_path Character string indicating the absolute pathway where the parallelization log file will be created if parallelization is used. If NULL, will be created in the R current directory.
#' @param verbose Single logical value. Display messages?
#' @param safer_check Single logical value. Perform some "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns
#' A data frame containing :
#'
#' - $left : the left boundary of each window (in the unit of the data argument).
#'
#' - $right : the right boundary of each window (in the unit of data argument).
#'
#' - $center : the center of each window (in the unit of data argument).
#'
#' - $value : the computed value by the fun argument in each window).
#' @details
#' WARNINGS
#'
#' The function uses two strategies, depending on the amout of memory required which depends on the data, window_size and step arguments. The first one uses lapply(), is generally fast but requires lots of memory. The second one uses a parallelized loop. The choice between the two strategies is automatic if parall argument is FALSE, and is forced toward parallelization if parall argument is TRUE.
#'
#' The parall argument forces the parallelization, which is convenient when the data argument is big, because the lapply() function is sometimes slower than the parallelization.
#'
#' Always use the env argument when slide() is used inside functions.
#' @seealso \code{\link[slider]{slide}}
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' slide(data = c(1:10, 100:110, 500), window_size = 5, step = 2, FUN = length, boundary = "left")
#'
#' slide(data = c(1:10, 100:110, 500), window_size = 5, step = 2, FUN = length, boundary = "right") # effect of boundary argument
#'
#' \dontrun{
#' slide(data = c(1:10, 100:110, 500), window_size = 5, step = 2, FUN = length, boundary = "left", parall = TRUE, thread_nb = 2) # effect of parall argument
#' }
#' @importFrom saferDev arg_check
#' @importFrom saferDev get_message
#' @importFrom saferTool round2
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterSplit
#' @importFrom parallel clusterApply
#' @importFrom parallel stopCluster
#' @importFrom lubridate seconds_to_period
#' @export
slide <- function(
    data,
    window_size,
    step,
    from = NULL,
    to = NULL,
    FUN,
    args = NULL,
    boundary = "left",
    parall = FALSE,
    thread_nb = NULL,
    print_count = 100,
    res_path = NULL,
    verbose = TRUE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
){

    # DEBUGGING
    # data = c(1:10, 100:110, 500) ; window_size = 5 ; step = 2 ; from = NULL ; to = NULL ; FUN = length ; args = NULL ; boundary = "left" ; parall = FALSE ; thread_nb = NULL ; print_count = 100 ; res_path = NULL ; verbose = TRUE ; safer_check = TRUE ; lib_path = NULL ; error_text = ""
    # data = lag.pos; window_size = window_size; step = step; FUN = length; from = min(a$pos); to = max(a$pos) ; safer_check = TRUE

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
    if(base::any(tempo_log, na.rm = TRUE)) {
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
        "data",
        "window_size",
        "step",
        "FUN"
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
        "data",
        "window_size",
        "step",
        # "from", # inactivated because can be null
        # "to", # inactivated because can be null
        "FUN",
        # "args", # inactivated because can be null
        "boundary",
        "parall",
        # "thread_nb", # inactivated because can be null
        "print_count",
        # "res_path", # inactivated because can be null
        "verbose",
        "safer_check"
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is slide's frame.
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
        "data",
        "window_size",
        "step",
        "from",
        "to",
        "FUN",
        "args",
        "boundary",
        "thread_nb",
        "print_count",
        "res_path",
        "verbose",
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
                    base::all(x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
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
                "saferDev::get_message",
                "saferTool::round2",
                "parallel::detectCores",
                "parallel::makeCluster",
                "parallel::clusterSplit",
                "parallel::clusterApply",
                "parallel::stopCluster",
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
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object_name))
    tempo <- saferDev::arg_check(data = data, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = window_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = step, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::is.null(x = from) == FALSE){
        tempo <- saferDev::arg_check(data = from, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if(base::is.null(x = to) == FALSE){
        tempo <- saferDev::arg_check(data = to, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo1 <- saferDev::arg_check(data = FUN, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    tempo2 <- saferDev::arg_check(data = FUN, class = "function", typeof = NULL, mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    if(tempo1$problem == TRUE & tempo2$problem == TRUE){
        tempo_cat <- base::paste0(
            arg_check_error_text,
            "FUN ARGUMENT MUST BE A FUNCTION OR A CHARACTER STRING OF THE NAME OF A FUNCTION",
            collapse = NULL,
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo_cat)
        argum_check <- base::c(argum_check, TRUE)
        checked_arg_names <- base::c(checked_arg_names, tempo1$object_name)
    }
    if(base::is.null(x = args) == FALSE){
        tempo <- saferDev::arg_check(data = args, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = boundary, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, options = base::c("left", "right"), double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = parall, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(parall == TRUE){
        if(base::is.null(x = thread_nb) == FALSE){
            tempo <- saferDev::arg_check(data = thread_nb, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        }
    }
    tempo <- saferDev::arg_check(data = print_count, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::is.null(x = res_path) == FALSE){
        tempo <- saferDev::arg_check(data = res_path, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = verbose, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        "boundary", # Ok if the argument is NULL, tempo_log will be FALSE
        "res_path" # Ok if the argument is NULL, tempo_log will be FALSE
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is slide's frame.
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
            base::paste0(base::sapply(X = tempo_arg, FUN = function(x){base::paste0(x, ': \"', base::typeof(x = base::get(x = x, pos = -1L, envir = base::parent.frame(n = 3), mode = "any", inherits = FALSE)), '"', collapse = NULL, recycle0 = FALSE)}), collapse = "\n", recycle0 = FALSE), # parent.frame(n = 3) because the get() is evaluated inside the FUN closure of sapply. But sapply(FUN) calls lapply(FUN) internally !! So parent.frame(n = 1) is the internal lapply() frame called by sapply, parent.frame(n = 2) is sapply's frame, parent.frame(n = 3) is slide's frame. NB: paste0() and typeof() are primitives and add no frame.
            intern_error_text_end,
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is slide's frame. # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start,
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"),
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    if(base::length(x = data) == 0){
        # nocov start
        # codecov inactivated because data length 0 cannot reach this block: empty non NULL arguments are rejected by the primary checking above.
        tempo_cat <- base::paste0(
            error_text_start,
            "data ARGUMENT CANNOT BE LENGTH 0",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }
    if(base::any( ! base::is.finite(data), na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start,
            "data ARGUMENT CANNOT CONTAIN Inf VALUES",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(step > window_size){
        tempo_cat <- base::paste0(
            error_text_start,
            "step ARGUMENT MUST BE LOWER THAN window_size ARGUMENT\nstep: ",
            base::paste(step, collapse = " ", recycle0 = FALSE),
            "\nwindow_size: ",
            base::paste(window_size, collapse = " ", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::is.null(x = res_path) == FALSE){
        if( ! base::all(base::dir.exists(paths = res_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and res_path == NA
            tempo_log <- ! base::dir.exists(paths = res_path)
            tempo_cat_b <- res_path[tempo_log] # here res_path is character string
            tempo_cat_b[tempo_cat_b == ""] <- "\"\""
            tempo_cat <- base::paste0(
                error_text_start,
                "THE DIRECTORY PATH",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""),
                " INDICATED IN THE res_path ARGUMENT DO",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"),
                " NOT EXIST:\n",
                base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }else{
        res_path <- base::getwd() # working directory
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    if(verbose == TRUE){
        base::cat("\nslide JOB IGNITION\n")
    }
    ini_date <- base::Sys.time()
    ini_time <- base::as.numeric(ini_date) # time of process begin, converted into seconds
    FUN <- base::match.fun(FUN) # make FUN <- base::get(FUN) if FUN is a function name written as character string of length 1
    if(boundary == "left"){
        left <- ">="
        right <- "<"
        right_last_wind <- ">"
    }else if(boundary == "right"){
        left <- ">"
        right <- "<="
        right_last_wind <- ">="
    }else{
        # nocov start
        # codecov inactivated because it is an internal error, impossible to cover with argument values (boundary already checked by arg_check() options above).
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE MAIN CODE OF ",
            intern_error_text_start,
            "CODE INCONSISTENCY 1.",
            intern_error_text_end,
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        # nocov end
    }
    data <- base::as.vector(data)
    data <- base::sort(data, na.last = NA) # NA removed
    wind <- base::data.frame(left = base::seq(from = if(base::is.null(from)){base::min(data, na.rm = TRUE)}else{from}, to = if(base::is.null(to)){base::max(data, na.rm = TRUE)}else{to}, by = step), stringsAsFactors = FALSE)
    wind <- base::data.frame(wind, right = wind$left + window_size, stringsAsFactors = FALSE)
    wind <- base::data.frame(wind, center = (wind$left + wind$right) / 2, stringsAsFactors = FALSE)
    if(base::all(wind$right < if(base::is.null(to)){base::max(data, na.rm = TRUE)}else{to})){
        # nocov start
        # codecov inactivated because it is an internal error, impossible to cover with argument values.
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE MAIN CODE OF ",
            intern_error_text_start,
            "CODE INCONSISTENCY 2.",
            intern_error_text_end,
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        # nocov end
    }
    # The 3 next lines is for the rule of to argument with center (see to argument description)
    # if(any(wind$center > max(data, na.rm = TRUE))){
    # wind <- wind[ ! wind$center > max(data, na.rm = TRUE),]
    # }
    if(base::sum(base::get(right_last_wind)(wind$right, if(base::is.null(to)){base::max(data, na.rm = TRUE)}else{to}), na.rm = TRUE) > 1){  # no env = base::sys.nframe(), inherit = FALSE in base::get() because look for function in the classical scope
        tempo_log <- base::get(right_last_wind)(wind$right, if(base::is.null(to)){base::max(data, na.rm = TRUE)}else{to}) # no env = base::sys.nframe(), inherit = FALSE in base::get() because look for function in the classical scope
        tempo_log[base::min(base::which(tempo_log), na.rm = TRUE)] <- FALSE # convert the first left boundary that goes above base::max(data, na.rm = TRUE) to FALSE to keep it (the next ones will be removed)
        wind <- wind[ ! tempo_log,]
    }

    # test if lapply can be used
    if(parall == FALSE){
        env_name <- base::paste0("env", base::as.numeric(base::Sys.time())) # new environment for the get_message() test
        if(base::exists(x = env_name, where = -1)){ # verify if still ok when info() is inside a function
            # nocov start
            # codecov inactivated because env_name is randomly generated, so it cannot already exist (internal error, impossible to cover with argument values).
            tempo_cat <- base::paste0(
                error_text_start,
                "ENVIRONMENT env_name ALREADY EXISTS. PLEASE RERUN ONCE",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            # nocov end
        }else{
            base::assign(x = env_name, value = base::new.env())
        }
        # end new environment
        base::assign("wind", wind, envir = base::get(env_name, envir = base::sys.nframe(), inherits = FALSE)) # wind assigned in a new envir for test
        base::assign("data", data, envir = base::get(env_name, envir = base::sys.nframe(), inherits = FALSE)) # data assigned in a new envir for test
        tempo_message <- saferDev::get_message(data="base::lapply(X = wind$left, Y = data, FUN = function(X, Y){res <- base::get(left)(Y, X) ; base::return(res)})", kind = "error", header = FALSE, print_no = FALSE, text = NULL, env = base::get(env_name, envir = base::sys.nframe(), inherits = FALSE), safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # no env = base::sys.nframe(), inherit = FALSE in base::get(left) because look for function in the classical scope
        # rm(env_name) # optional, because should disappear at the end of the function execution
    }else{
        tempo_message <- "ERROR" # with this, force the parallelization by default
    }
    # end test if lapply can be used
    if( ! base::any(base::grepl(x = tempo_message, pattern = "ERROR.*"), na.rm = TRUE)){
        left_log <- base::lapply(X = wind$left, Y = data, FUN = function(X, Y){
            res <- base::get(left)(Y, X) # no env = base::sys.nframe(), inherit = FALSE in base::get() because look for function in the classical scope
            base::return(res)
        })
        right_log <- base::lapply(X = wind$right, Y = data, FUN = function(X, Y){
            res <- base::get(right)(Y, X) # no env = base::sys.nframe(), inherit = FALSE in base::get() because look for function in the classical scope
            base::return(res)
        })
        log <-base::mapply(FUN = "&", left_log, right_log, SIMPLIFY = FALSE)
        output <- base::eval(base::parse(text = base::paste0("base::sapply(base::lapply(log, FUN = function(X){(data[X])}), FUN = FUN", if( ! base::is.null(args)){base::paste0(", ", args)}, ")")))
        if(base::length(output) != base::nrow(wind)){
            # nocov start
            # codecov inactivated because it is an internal error, impossible to cover with argument values.
            tempo_cat <- base::paste0(
                "INTERNAL ERROR IN THE MAIN CODE OF ",
                intern_error_text_start,
                "CODE INCONSISTENCY 3.",
                intern_error_text_end,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            # nocov end
        }else{
            output <- base::data.frame(wind, value = output, stringsAsFactors = FALSE)
        }
    }else{
        if(verbose == TRUE){
            tempo_cat <- base::paste0("PARALLELIZATION INITIATED AT: ", ini_date)
            base::cat(base::paste0("\n", tempo_cat, "\n"))
        }
        tempo_thread_nb = parallel::detectCores(all.tests = FALSE, logical = TRUE) # detect the number of threads
        if( ! base::is.null(thread_nb)){
            if(tempo_thread_nb < thread_nb){
                thread_nb <- tempo_thread_nb
                if(verbose == TRUE){
                    tempo_cat <- base::paste0("ONLY: ", tempo_thread_nb, " THREADS AVAILABLE")
                    base::cat(base::paste0("\n", tempo_cat, "\n"))
                }
            }
        }else{
            thread_nb <- tempo_thread_nb
        }
        if(verbose == TRUE){
            tempo_cat <- base::paste0("NUMBER OF THREADS USED: ", thread_nb)
            base::cat(base::paste0("\n    ", tempo_cat, "\n"))
        }
        Clust <- parallel::makeCluster(spec = thread_nb, outfile = base::paste0(res_path, "/fun_slide_parall_log.txt")) # outfile to print or cat during parallelization (only possible in a file, outfile = "" do not work on windows)
        cluster_list <- parallel::clusterSplit(cl = Clust, seq = 1:base::nrow(wind)) # split according to the number of cluster
        if(verbose == TRUE){
            tempo_cat <- base::paste0("SPLIT OF TEST NUMBERS IN PARALLELISATION:")
            base::cat(base::paste0("\n    ", tempo_cat, "\n"))
            utils::str(cluster_list) # using base::print(utils::str()) add a NULL below the result
            base::cat("\n")
        }
        paral_output_list <- parallel::clusterApply( #
            cl = Clust,
            x = cluster_list,
            function_name = function_name,
            data = data,
            FUN = FUN,
            args = args,
            thread_nb = thread_nb,
            print_count = print_count,
            wind = wind,
            left = left,
            right = right,
            res_path = res_path,
            verbose = verbose,
            fun = function(
                x,
                function_name,
                data,
                FUN,
                args,
                thread_nb,
                print_count,
                wind,
                left,
                right,
                res_path,
                verbose
            ){
                # check again: very important because another R
                process_id <- base::Sys.getpid()
                base::cat(base::paste0("\nPROCESS ID ", process_id, " -> TESTS ", x[1], " TO ", x[base::length(x)], "\n"))
                # fun_pack(req.package = "lubridate", lib.path = lib.path, load = TRUE) # load = TRUE to be sure that functions are present in the environment. And this prevent to use R.lib.path argument of python_pack()
                # end check again: very important because another R
                # nocov start
                # codecov inactivated because this function is executed in child processes launched by parallel::clusterApply(), which are not tracked by covr.
                ini_date <- base::Sys.time()
                ini_time <- base::as.numeric(ini_date) # time of process begin, converted into
                output <- NULL
                print_count_loop <- 0
                for(i4 in 1:base::length(x)){
                    print_count_loop <- print_count_loop + 1
                    log <- base::get(left)(data, wind$left[x[i4]]) & base::get(right)(data, wind$right[x[i4]]) # no env = base::sys.nframe(), inherit = FALSE in base::get() because look for function in the classical scope
                    output <- base::c(output, base::eval(base::parse(text = base::paste0("FUN(data[log]", if( ! base::is.null(args)){base::paste0(", ", args)}, ")"))))
                    if(verbose == TRUE){
                        if(print_count_loop == print_count){
                            print_count_loop <- 0
                            tempo_time <- base::as.numeric(base::Sys.time())
                            tempo_lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(x = tempo_time - ini_time)), safer_check = FALSE)
                            final_loop <- (tempo_time - ini_time) / i4 * base::length(x) # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
                            final_exp <- base::as.POSIXct(final_loop, origin = ini_date)
                            base::cat(base::paste0("\nIN PROCESS ", process_id, " | LOOP ", base::format(i4, big.mark=","), " / ", base::format(base::length(x), big.mark=","), " | TIME SPENT: ", tempo_lapse, " | EXPECTED END: ", final_exp))
                        }
                        if(i4 == base::length(x)){
                            tempo_time <- base::as.numeric(base::Sys.time())
                            tempo_lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(x = tempo_time - ini_time)), safer_check = FALSE)
                            base::cat(base::paste0("\nPROCESS ", process_id, " ENDED | LOOP ", base::format(i4, big.mark=","), " / ", base::format(base::length(x), big.mark=","), " | TIME SPENT: ", tempo_lapse, "\n\n"))
                        }
                    }
                }
                # nocov end
                wind <- wind[x, ]
                # nocov start
                # codecov inactivated because this is executed in child processes launched by parallel::clusterApply(), which are not tracked by covr (internal error, impossible to cover with argument values).
                if(base::length(output) != base::nrow(wind)){
                    tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 4")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
                }else{
                    output <- base::data.frame(wind, value = output, stringsAsFactors = FALSE)
                    base::return(output)
                }
                # nocov end
            }
        )
        parallel::stopCluster(cl = Clust)
        # result assembly
        output <- base::data.frame()
        for(i2 in 1:base::length(paral_output_list)){ # compartment relatives to each parallelization
            output <- base::rbind(output, paral_output_list[[i2]], stringsAsFactors = FALSE)
        }
        # end result assembly
        if(base::nrow(output) != base::nrow(wind)){
            # nocov start
            # codecov inactivated because it is an internal error, impossible to cover with argument values.
            tempo_cat <- base::paste0(
                "INTERNAL ERROR IN THE MAIN CODE OF ",
                intern_error_text_start,
                "CODE INCONSISTENCY 5\nbase::length(output): ",
                base::length(output),
                "\nbase::nrow(wind): ",
                base::nrow(wind),
                intern_error_text_end,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            # nocov end
        }else{
            output <- output[base::order(output$left), ]
        }
    }
    if(verbose == TRUE){
        end_date <- base::Sys.time()
        end_time <- base::as.numeric(end_date)
        total_lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(x = end_time - ini_time)), safer_check = FALSE)
        base::cat(base::paste0("\nslide JOB END\n\nTIME: ", end_date, "\n\nTOTAL TIME LAPSE: ", total_lapse, "\n\n\n"))
    }

    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output

}
