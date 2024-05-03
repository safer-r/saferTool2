#' @title slide
#' @description
#' Return a computation made on a vector using a sliding window.
#' @param data Vector, matrix, table or array of numeric values (mode must be numeric). Inf not allowed. NA will be removed before computation.
#' @param window.size Single numeric value indicating the width of the window sliding across data (in the same unit as data value).
#' @param step Single numeric value indicating the step between each window (in the same unit as data value). Cannot be larger than window.size.
#' @param from Single numeric value of the left boundary of the first sliding window. If NULL, min(data) is used. The first window will strictly have from or min(data) as left boundary.
#' @param to Single numeric value of the right boundary of the last sliding window. If NULL, max(data) is used. Warning: (1) the final last window will not necessary have to|max(data) as right boundary. In fact the last window will be the one that contains to|max(data) for the first time, i.e., min[from|min(data) + window.size + n * step >= to|max(data)]; (2) In fact, the >= in min[from|min(data) + window.size + n * step >= to|max(data)] depends on the boundary argument (>= for "right" and > for "left"); (3) to have the rule (1) but for the center of the last window, use to argument as to = to|max(data) + window.size / 2.
#' @param FUN Function or single character string indicating the name of the function to apply in each window. Example of function: FUN = mean. Example of character string: FUN = "mean".
#' @param args Single character string of additional arguments of FUN (separated by a comma between the quotes). Example args = "na.rm = TRUE" for FUN = mean. Ignored if NULL.
#' @param boundary Either "left" or "right". Indicates if the sliding window includes values equal to left boundary and exclude values equal to right boundary ("left") or the opposite ("right").
#' @param parall Single logical value. Force parallelization ?
#' @param thread.nb Single numeric value indicating the number of threads to use if ever parallelization is required. If NULL, all the available threads will be used. Ignored if parall is FALSE.
#' @param print.count Single integer value. Print a working progress message every print.count during loops. BEWARE: can increase substantially the time to complete the process using a small value, like 10 for instance. Use Inf is no loop message desired.
#' @param res.path Character string indicating the absolute pathway where the parallelization log file will be created if parallelization is used. If NULL, will be created in the R current directory.
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @param verbose Single logical value. Display messages?
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
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
#' The function uses two strategies, depending on the amout of memory required which depends on the data, window.size and step arguments. The first one uses lapply(), is generally fast but requires lots of memory. The second one uses a parallelized loop. The choice between the two strategies is automatic if parall argument is FALSE, and is forced toward parallelization if parall argument is TRUE.
#' 
#' The parall argument forces the parallelization, which is convenient when the data argument is big, because the lapply() function is sometimes slower than the parallelization.
#' 
#' Always use the env argument when slide() is used inside functions.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' slide(data = c(1:10, 100:110, 500), window.size = 5, step = 2, FUN = length, boundary = "left")
#' 
#' slide(data = c(1:10, 100:110, 500), window.size = 5, step = 2, FUN = length, boundary = "right") # effect of boundary argument
#' 
#' \dontrun{
#' slide(data = c(1:10, 100:110, 500), window.size = 5, step = 2, FUN = length, boundary = "left", parall = TRUE, thread.nb = 2) # effect of parall argument
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
        window.size, 
        step, 
        from = NULL, 
        to = NULL, 
        FUN,
        args = NULL, 
        boundary = "left", 
        parall = FALSE, 
        thread.nb = NULL, 
        print.count = 100, 
        res.path = NULL, 
        lib.path = NULL, 
        verbose = TRUE, 
        safer_check = TRUE
){
    # DEBUGGING
    # data = c(1:10, 100:110, 500) ; window.size = 5 ; step = 2 ; from = NULL ; to = NULL ; FUN = length ; args = NULL ; boundary = "left" ; parall = FALSE ; thread.nb = NULL ; print.count = 100 ; res.path = NULL ; lib.path = NULL ; verbose = TRUE ; safer_check = TRUE
    # data = lag.pos; window.size = window.size; step = step; FUN = length; from = min(a$pos); to = max(a$pos) ; safer_check = TRUE
    # package name
    package.name <- "saferTool2"
    # end package name
    # function name
    ini <- base::match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            external.function.name = function.name, 
            external.package.name = package.name
        )
    }
    # end critical operator checking

    
    # package checking
    # check of lib.path
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
           base:: stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            base::.libPaths(new = base::sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- base::.libPaths()
        }
    }else{
        lib.path <- base::.libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }
    # end check of lib.path
    
    # check of the required function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
            fun = base::c(
                "saferDev::arg_check",
                "saferDev::get_message",
                "saferTool::round2",
                "parallel::detectCores",
                "parallel::makeCluster",
                "parallel::clusterSplit",
                "parallel::clusterApply",
                "parallel::stopCluster",
                "lubridate::seconds_to_period"
        ),
        lib.path = lib.path,
        external.function.name = function.name,
        external.package.name = package.name
    )
    }
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "data", 
        "window.size", 
        "step", 
        "FUN"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "),base::missing("), "))")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = data, mode = "numeric", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = window.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = step, class = "vector", mode = "numeric", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(from)){
        tempo <- saferDev::arg_check(data = from, class = "vector", mode = "numeric", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(to)){
        tempo <- saferDev::arg_check(data = to, class = "vector", mode = "numeric", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    tempo1 <- saferDev::arg_check(data = FUN, class = "vector", mode = "character", length = 1, fun.name = function.name, safer_check = FALSE)
    tempo2 <- saferDev::arg_check(data = FUN, class = "function", length = 1, fun.name = function.name, safer_check = FALSE)
    if(tempo1$problem == TRUE & tempo2$problem == TRUE){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: FUN ARGUMENT MUST BE A FUNCTION OR A CHARACTER STRING OF THE NAME OF A FUNCTION")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }
    if( ! base::is.null(args)){
        tempo <- saferDev::arg_check(data = args, class = "vector", mode = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = boundary, options = base::c("left", "right"), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = parall, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if(parall == TRUE){
        if( ! base::is.null(thread.nb)){
            tempo <- saferDev::arg_check(data = thread.nb, typeof = "integer", double.as.integer.allowed = TRUE, neg.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            if(tempo$problem == FALSE & thread.nb < 1){
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: thread.nb PARAMETER MUST EQUAL OR GREATER THAN 1: ", thread.nb)
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    tempo <- saferDev::arg_check(data = print.count, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(res.path)){
        tempo <- saferDev::arg_check(data = res.path, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(lib.path)){
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = verbose, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R")  ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # new environment
    env.name <- base::paste0("env", base::as.numeric(base::Sys.time()))
    if(base::exists(env.name, where = -1)){ # verify if still ok when info() is inside a function
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        base::assign(env.name, base::new.env())
    }
    # end new environment
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <- base::c(
        "data", 
        "window.size", 
        "step", 
        # "from", # inactivated because can be null
        # "to", # inactivated because can be null
        "FUN", 
        # "args", # inactivated because can be null
        "boundary", 
        # "parall", 
        # "thread.nb", # inactivated because can be null
        "print.count", 
        # "res.path", # inactivated because can be null
        # "lib.path", # inactivated because can be null
        "verbose", 
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    if(base::length(data) == 0){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data ARGUMENT CANNOT BE LENGTH 0")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
    }
    if(base::any( ! base::is.finite(data), na.rm = TRUE)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data ARGUMENT CANNOT CONTAIN Inf VALUES")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
    }
    if(step > window.size){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: step ARGUMENT MUST BE LOWER THAN window.size ARGUMENT\nstep: ", base::paste(step, collapse = " "), "\nwindow.size: ", base::paste(window.size, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
    }
    if( ! base::is.null(res.path)){
        if( ! base::all(base::dir.exists(res.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and res.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE res.path ARGUMENT DOES NOT EXISTS:\n", base::paste(res.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
        }
    }else{
        res.path <- base::getwd() # working directory
    }
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
        }
    }
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    if(verbose == TRUE){
        base::cat("\nslide JOB IGNITION\n")
    }
    ini.date <- base::Sys.time()
    ini.time <- base::as.numeric(ini.date) # time of process begin, converted into seconds
    FUN <- base::match.fun(FUN) # make FUN <- get(FUN) if FUN is a function name written as character string of length 1
    if(boundary == "left"){
        left <- ">="
        right <- "<"
        right.last.wind <- ">"
    }else if(boundary == "right"){
        left <- ">"
        right <- "<="
        right.last.wind <- ">="
    }else{
        tempo.cat <- base::paste0("INTERNAL CODE ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nCODE INCONSISTENCY 1")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    data <- base::as.vector(data)
    data <- base::sort(data, na.last = NA) # NA removed
    wind <- base::data.frame(left = base::seq(from = if(base::is.null(from)){base::min(data, na.rm = TRUE)}else{from}, to = if(base::is.null(to)){base::max(data, na.rm = TRUE)}else{to}, by = step), stringsAsFactors = TRUE)
    wind <- base::data.frame(wind, right = wind$left + window.size, stringsAsFactors = TRUE)
    wind <- base::data.frame(wind, center = (wind$left + wind$right) / 2, stringsAsFactors = TRUE)
    if(base::all(wind$right < if(base::is.null(to)){base::max(data, na.rm = TRUE)}else{to})){
        tempo.cat <- base::paste0("INTERNAL CODE ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nCODE INCONSISTENCY 2")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # The 3 next lines is for the rule of to argument with center (see to argument description)
    # if(any(wind$center > max(data, na.rm = TRUE))){
    # wind <- wind[ ! wind$center > max(data, na.rm = TRUE),]
    # }
    if(base::sum(base::get(right.last.wind)(wind$right, if(base::is.null(to)){base::max(data, na.rm = TRUE)}else{to}), na.rm = TRUE) > 1){  # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
        tempo.log <- base::get(right.last.wind)(wind$right, if(base::is.null(to)){base::max(data, na.rm = TRUE)}else{to}) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
        tempo.log[base::min(base::which(tempo.log), na.rm = TRUE)] <- FALSE # convert the first left boundary that goes above max(data, na.rm = TRUE) to FALSE to keep it (the next ones will be removed)
        wind <- wind[ ! tempo.log,]
    }
    
    # test if lapply can be used
    if(parall == FALSE){
        base::assign("wind", wind, envir = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE)) # wind assigned in a new envir for test
        base::assign("data", data, envir = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE)) # data assigned in a new envir for test
        tempo.message <- saferDev::get_message(data="base::lapply(X = wind$left, Y = data, FUN = function(X, Y){res <- base::get(left)(Y, X) ; base::return(res)})", kind = "error", header = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), print.no = FALSE, safer_check = FALSE) # no env = sys.nframe(), inherit = FALSE in get(left) because look for function in the classical scope
        # rm(env.name) # optional, because should disappear at the end of the function execution
    }else{
        tempo.message <- "ERROR" # with this, force the parallelization by default
    }
    # end test if lapply can be used
    if( ! base::any(base::grepl(x = tempo.message, pattern = "ERROR.*"), na.rm = TRUE)){
        left.log <- base::lapply(X = wind$left, Y = data, FUN = function(X, Y){
            res <- base::get(left)(Y, X) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            base::return(res)
        })
        right.log <- base::lapply(X = wind$right, Y = data, FUN = function(X, Y){
            res <- base::get(right)(Y, X) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            base::return(res)
        })
        log <-base::mapply(FUN = "&", left.log, right.log, SIMPLIFY = FALSE)
        # output
        # warning output
        # end warning output
        output <- base::eval(base::parse(text = base::paste0("base::sapply(base::lapply(log, FUN = function(X){(data[X])}), FUN = FUN", if( ! base::is.null(args)){base::paste0(", ", args)}, ")"))) # take the values of the data vector according to log (list of logical, each compartment of length(data)) and apply fun with args of fun
        if(base::length(output) != base::nrow(wind)){
            tempo.cat <- base::paste0("INTERNAL CODE ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nCODE INCONSISTENCY 3")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            output <- base::data.frame(wind, value = output, stringsAsFactors = TRUE)
        }
    }else{
        if(verbose == TRUE){
            tempo.cat <- base::paste0("PARALLELIZATION INITIATED AT: ", ini.date)
            base::cat(base::paste0("\n", tempo.cat, "\n"))
        }
        tempo.thread.nb = parallel::detectCores(all.tests = FALSE, logical = TRUE) # detect the number of threads
        if( ! base::is.null(thread.nb)){
            if(tempo.thread.nb < thread.nb){
                thread.nb <- tempo.thread.nb
                if(verbose == TRUE){
                    tempo.cat <- base::paste0("ONLY: ", tempo.thread.nb, " THREADS AVAILABLE")
                    base::cat(base::paste0("\n", tempo.cat, "\n"))
                }
            }
        }else{
            thread.nb <- tempo.thread.nb
        }
        if(verbose == TRUE){
            tempo.cat <- base::paste0("NUMBER OF THREADS USED: ", thread.nb)
            base::cat(base::paste0("\n    ", tempo.cat, "\n"))
        }
        Clust <- parallel::makeCluster(thread.nb, outfile = base::paste0(res.path, "/fun_slide_parall_log.txt")) # outfile to print or cat during parallelization (only possible in a file, outfile = "" do not work on windows)
        cluster.list <- parallel::clusterSplit(Clust, 1:base::nrow(wind)) # split according to the number of cluster
        if(verbose == TRUE){
            tempo.cat <- base::paste0("SPLIT OF TEST NUMBERS IN PARALLELISATION:")
            base::cat(base::paste0("\n    ", tempo.cat, "\n"))
            utils::str(cluster.list) # using print(str()) add a NULL below the result
            base::cat("\n")
        }
        paral.output.list <- parallel::clusterApply( #
            cl = Clust,
            x = cluster.list,
            function.name = function.name, 
            data = data, 
            FUN = FUN,
            args = args, 
            thread.nb = thread.nb, 
            print.count = print.count, 
            wind = wind, 
            left = left, 
            right = right, 
            res.path = res.path, 
            lib.path = lib.path, 
            verbose = verbose, 
            fun = function(
        x, 
        function.name, 
        data, 
        FUN, 
        args, 
        thread.nb, 
        print.count, 
        wind, 
        left, 
        right, 
        res.path, 
        lib.path, 
        verbose
            ){
                # check again: very important because another R
                process.id <- base::Sys.getpid()
                base::cat(base::paste0("\nPROCESS ID ", process.id, " -> TESTS ", x[1], " TO ", x[base::length(x)], "\n"))
                # fun_pack(req.package = "lubridate", lib.path = lib.path, load = TRUE) # load = TRUE to be sure that functions are present in the environment. And this prevent to use R.lib.path argument of python_pack()
                # end check again: very important because another R
                ini.date <- base::Sys.time()
                ini.time <- base::as.numeric(ini.date) # time of process begin, converted into 
                output <- NULL
                print.count.loop <- 0
                for(i4 in 1:base::length(x)){
                    print.count.loop <- print.count.loop + 1
                    log <- base::get(left)(data, wind$left[x[i4]]) & base::get(right)(data, wind$right[x[i4]]) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
                    output <- base::c(output, base::eval(base::parse(text = base::paste0("FUN(data[log]", if( ! base::is.null(args)){base::paste0(", ", args)}, ")"))))
                    if(verbose == TRUE){
                        if(print.count.loop == print.count){
                            print.count.loop <- 0
                            tempo.time <- base::as.numeric(base::Sys.time())
                            tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(tempo.time - ini.time)), safer_check = FALSE)
                            final.loop <- (tempo.time - ini.time) / i4 * base::length(x) # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
                            final.exp <- base::as.POSIXct(final.loop, origin = ini.date)
                            base::cat(base::paste0("\nIN PROCESS ", process.id, " | LOOP ", base::format(i4, big.mark=","), " / ", base::format(base::length(x), big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
                        }
                        if(i4 == base::length(x)){
                            tempo.time <- base::as.numeric(base::Sys.time())
                            tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(tempo.time - ini.time)), safer_check = FALSE)
                            base::cat(base::paste0("\nPROCESS ", process.id, " ENDED | LOOP ", base::format(i4, big.mark=","), " / ", base::format(base::length(x), big.mark=","), " | TIME SPENT: ", tempo.lapse, "\n\n"))
                        }
                    }
                }
                wind <- wind[x, ]
                if(base::length(output) != base::nrow(wind)){
                    tempo.cat <- base::paste0("INTERNAL CODE ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nCODE INCONSISTENCY 4")
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else{
                    output <- base::data.frame(wind, value = output, stringsAsFactors = TRUE)
                    base::return(output)
                }
            }
        )
        parallel::stopCluster(Clust)
        # result assembly
        output <- base::data.frame()
        for(i2 in 1:base::length(paral.output.list)){ # compartment relatives to each parallelization
            output <- base::rbind(output, paral.output.list[[i2]], stringsAsFactors = TRUE)
        }
        # end result assembly
        if(base::nrow(output) != base::nrow(wind)){
            tempo.cat <- base::paste0("INTERNAL CODE ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nCODE INCONSISTENCY 5\nbase::length(output): ", base::length(output), "\nbase::nrow(wind): ", base::nrow(wind))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            output <- output[base::order(output$left), ]
        }
    }
    if(verbose == TRUE){
        end.date <- base::Sys.time()
        end.time <- base::as.numeric(end.date)
        total.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(end.time - ini.time)), safer_check = FALSE)
        base::cat(base::paste0("\nslide JOB END\n\nTIME: ", end.date, "\n\nTOTAL TIME LAPSE: ", total.lapse, "\n\n\n"))
    }
    base::return(output)
    # end output
    # end main code
}
