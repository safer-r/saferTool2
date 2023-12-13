#' @title slide
#' @description
#' Return a computation made on a vector using a sliding window.
#' @param data Vector, matrix, table or array of numeric values (mode must be numeric). Inf not allowed. NA will be removed before computation.
#' @param window.size Single numeric value indicating the width of the window sliding across data (in the same unit as data value).
#' @param step Single numeric value indicating the step between each window (in the same unit as data value). Cannot be larger than window.size.
#' @param from Single numeric value of the left boundary of the first sliding window. If NULL, min(data) is used. The first window will strictly have from or min(data) as left boundary.
#' @param to Single numeric value of the right boundary of the last sliding window. If NULL, max(data) is used. Warning: (1) the final last window will not necessary have to|max(data) as right boundary. In fact the last window will be the one that contains to|max(data) for the first time, i.e., min[from|min(data) + window.size + n * step >= to|max(data)]; (2) In fact, the >= in min[from|min(data) + window.size + n * step >= to|max(data)] depends on the boundary argument (>= for "right" and > for "left"); (3) to have the rule (1) but for the center of the last window, use to argument as to = to|max(data) + window.size / 2.
#' @param fun Function or single character string indicating the name of the function to apply in each window. Example of function: fun = mean.Example of character string: fun = "mean".
#' @param args Single character string of additional arguments of fun (separated by a comma between the quotes). Example args = "na.rm = TRUE" for fun = mean. Ignored if NULL.
#' @param boundary Either "left" or "right". Indicates if the sliding window includes values equal to left boundary and exclude values equal to right boundary ("left") or the opposite ("right").
#' @param parall Single logical value. Force parallelization ?
#' @param thread.nb Single numeric value indicating the number of threads to use if ever parallelization is required. If NULL, all the available threads will be used. Ignored if parall is FALSE.
#' @param print.count Single interger value. Print a working progress message every print.count during loops. BEWARE: can increase substentially the time to complete the process using a small value, like 10 for instance. Use Inf is no loop message desired.
#' @param res.path Character string indicating the absolute pathway where the parallelization log file will be created if parallelization is used. If NULL, will be created in the R current directory.
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @param verbose Single logical value. Display messages?
#' @param cute.path Single character string indicating the absolute path of the cute.R file. Will be remove when cute will be a package. Ignored if parall is FALSE.
#' @returns
#' A data frame containing :
#' - $left : the left boundary of each window (in the unit of the data argument).
#' - $right : the right boundary of each window (in the unit of data argument).
#' - $center : the center of each window (in the unit of data argument).
#' - $value : the computed value by the fun argument in each window).
#' @details 
#' REQUIRED PACKAGES
#' 
#' lubridate
#' 
#' parallel if parall argument is TRUE (included in the R installation packages but not automatically loaded)
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#' 
#' fun_get_message()
#' 
#' fun_pack()
#' 
#' 
#' WARNINGS
#' 
#' The function uses two strategies, depending on the amout of memory required which depends on the data, window.size and step arguments. The first one uses lapply(), is generally fast but requires lots of memory. The second one uses a parallelized loop. The choice between the two strategies is automatic if parall argument is FALSE, and is forced toward parallelization if parall argument is TRUE.
#' 
#' The parall argument forces the parallelization, which is convenient when the data argument is big, because the lapply() function is sometimes slower than the parallelization.
#' 
#' Always use the env argument when fun_slide() is used inside functions.
#' @examples
#' fun_slide(data = c(1:10, 100:110, 500), window.size = 5, step = 2, fun = length, boundary = "left")
#' 
#' fun_slide(data = c(1:10, 100:110, 500), window.size = 5, step = 2, fun = length, boundary = "right") # effect of boundary argument
#' 
#' fun_slide(data = c(1:10, 100:110, 500), window.size = 5, step = 2, fun = length, boundary = "left", parall = TRUE) # effect of parall argument
#' @export
fun_slide <- function(
        data, 
        window.size, 
        step, 
        from = NULL, 
        to = NULL, 
        fun, 
        args = NULL, 
        boundary = "left", 
        parall = FALSE, 
        thread.nb = NULL, 
        print.count = 100, 
        res.path = NULL, 
        lib.path = NULL, 
        verbose = TRUE, 
        cute.path = "C:\\Users\\Gael\\Documents\\Git_projects\\cute_little_R_functions\\cute_little_R_functions.R"
){
    # DEBUGGING
    # data = c(1:10, 100:110, 500) ; window.size = 5 ; step = 2 ; from = NULL ; to = NULL ; fun = length ; args = NULL ; boundary = "left" ; parall = FALSE ; thread.nb = NULL ; print.count = 100 ; res.path = NULL ; lib.path = NULL ; verbose = TRUE ; cute.path = "C:\\Users\\Gael\\Documents\\Git_projects\\cute_little_R_functions\\cute_little_R_functions.R"
    # data = lag.pos; window.size = window.size; step = step; fun = length; from = min(a$pos); to = max(a$pos)
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # required function checking
    req.function <- c(
        "fun_check", 
        "fun_get_message", 
        "fun_pack"
    )
    tempo <- NULL
    for(i1 in req.function){
        if(length(find(i1, mode = "function")) == 0L){
            tempo <- c(tempo, i1)
        }
    }
    if( ! is.null(tempo)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nREQUIRED cute FUNCTION", ifelse(length(tempo) > 1, "S ARE", " IS"), " MISSING IN THE R ENVIRONMENT:\n", paste0(tempo, collapse = "()\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end required function checking
    # reserved words
    # end reserved words
    # arg with no default values
    mandat.args <- c(
        "data", 
        "window.size", 
        "step", 
        "fun"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument primary checking
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = data, mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = window.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = step, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(from)){
        tempo <- fun_check(data = from, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(to)){
        tempo <- fun_check(data = to, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    }
    tempo1 <- fun_check(data = fun, class = "vector", mode = "character", length = 1, fun.name = function.name)
    tempo2 <- fun_check(data = fun, class = "function", length = 1, fun.name = function.name)
    if(tempo1$problem == TRUE & tempo2$problem == TRUE){
        tempo.cat <- paste0("ERROR IN ", function.name, ": fun ARGUMENT MUST BE A FUNCTION OR A CHARACTER STRING OF THE NAME OF A FUNCTION")
        text.check <- c(text.check, tempo.cat)
        arg.check <- c(arg.check, TRUE)
    }
    if( ! is.null(args)){
        tempo <- fun_check(data = args, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    tempo <- fun_check(data = boundary, options = c("left", "right"), length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = parall, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if(parall == TRUE){
        if( ! is.null(thread.nb)){
            tempo <- fun_check(data = thread.nb, typeof = "integer", double.as.integer.allowed = TRUE, neg.values = FALSE, length = 1, fun.name = function.name) ; eval(ee)
            if(tempo$problem == FALSE & thread.nb < 1){
                tempo.cat <- paste0("ERROR IN ", function.name, ": thread.nb PARAMETER MUST EQUAL OR GREATER THAN 1: ", thread.nb)
                text.check <- c(text.check, tempo.cat)
                arg.check <- c(arg.check, TRUE)
            }
        }
    }
    tempo <- fun_check(data = print.count, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    if( ! is.null(res.path)){
        tempo <- fun_check(data = res.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(lib.path)){
        tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    }
    tempo <- fun_check(data = verbose, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = cute.path, class = "vector", typeof = "character", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end using fun_check()
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument primary checking
    # second round of checking and data preparation
    # new environment
    env.name <- paste0("env", as.numeric(Sys.time()))
    if(exists(env.name, where = -1)){ # verify if still ok when fun_info() is inside a function
        tempo.cat <- paste0("ERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        assign(env.name, new.env())
    }
    # end new environment
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-c(
        "data", 
        "window.size", 
        "step", 
        # "from", # inactivated because can be null
        # "to", # inactivated because can be null
        "fun", 
        # "args", # inactivated because can be null
        "boundary", 
        # "parall", 
        # "thread.nb", # inactivated because can be null
        "print.count", 
        # "res.path", # inactivated because can be null
        # "lib.path", # inactivated because can be null
        "verbose", 
        "cute.path"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    if(length(data) == 0){
        tempo.cat <- paste0("ERROR IN ", function.name, ": data ARGUMENT CANNOT BE LENGTH 0")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
    }
    if(any( ! is.finite(data), na.rm = TRUE)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": data ARGUMENT CANNOT CONTAIN Inf VALUES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
    }
    if(step > window.size){
        tempo.cat <- paste0("ERROR IN ", function.name, ": step ARGUMENT MUST BE LOWER THAN window.size ARGUMENT\nstep: ", paste(step, collapse = " "), "\nwindow.size: ", paste(window.size, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
    }
    if( ! is.null(res.path)){
        if( ! all(dir.exists(res.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and res.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE res.path ARGUMENT DOES NOT EXISTS:\n", paste(res.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
        }
    }else{
        res.path <- getwd() # working directory
    }
    if( ! is.null(lib.path)){
        if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
        }
    }
    if(grepl(x = cute.path, pattern = "^http")){
        tempo.error1 <- any(grepl(x = fun_get_message(data = "source(cute.path)", kind = "error", header = FALSE, env = get(env.name, env = sys.nframe(), inherit = FALSE)), pattern = "^[Ee]rror"), na.rm = TRUE)
        tempo.error2 <- FALSE
    }else{
        tempo.error1 <- FALSE
        tempo.error2 <- ! file.exists(cute.path)
    }
    if(tempo.error1 | tempo.error2){
        tempo.cat <- paste0("ERROR IN ", function.name, ": ", ifelse(grepl(x = cute.path, pattern = "^http"), "URL", "FILE"), " PATH INDICATED IN THE cute.path PARAMETER DOES NOT EXISTS:\n", cute.path)
        text.check <- c(text.check, tempo.cat)
        arg.check <- c(arg.check, TRUE)
    }
    # end other checkings
    # reserved word checking
    # end reserved word checking
    # end second round of checking and data preparation
    # package checking
    fun_pack(req.package = c("lubridate"), lib.path = lib.path)
    fun_pack(req.package = c("parallel"), lib.path = lib.path)
    # end package checking
    # main code
    if(verbose == TRUE){
        cat("\nfun_slide JOB IGNITION\n")
    }
    ini.date <- Sys.time()
    ini.time <- as.numeric(ini.date) # time of process begin, converted into seconds
    fun <- match.fun(fun) # make fun <- get(fun) is fun is a function name written as character string of length 1
    if(boundary == "left"){
        left <- ">="
        right <- "<"
        right.last.wind <- ">"
    }else if(boundary == "right"){
        left <- ">"
        right <- "<="
        right.last.wind <- ">="
    }else{
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 1")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    data <- as.vector(data)
    data <- sort(data, na.last = NA) # NA removed
    wind <- data.frame(left = seq(from = if(is.null(from)){min(data, na.rm = TRUE)}else{from}, to = if(is.null(to)){max(data, na.rm = TRUE)}else{to}, by = step), stringsAsFactors = TRUE)
    wind <- data.frame(wind, right = wind$left + window.size, stringsAsFactors = TRUE)
    wind <- data.frame(wind, center = (wind$left + wind$right) / 2, stringsAsFactors = TRUE)
    if(all(wind$right < if(is.null(to)){max(data, na.rm = TRUE)}else{to})){
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 2")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # The 3 next lines is for the rule of to argument with center (see to argument description)
    # if(any(wind$center > max(data, na.rm = TRUE))){
    # wind <- wind[ ! wind$center > max(data, na.rm = TRUE),]
    # }
    if(sum(get(right.last.wind)(wind$right, if(is.null(to)){max(data, na.rm = TRUE)}else{to}), na.rm = TRUE) > 1){  # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
        tempo.log <- get(right.last.wind)(wind$right, if(is.null(to)){max(data, na.rm = TRUE)}else{to}) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
        tempo.log[min(which(tempo.log), na.rm = TRUE)] <- FALSE # convert the first left boundary that goes above max(data, na.rm = TRUE) to FALSE to keep it (the next ones will be removed)
        wind <- wind[ ! tempo.log,]
    }
    
    # test if lapply can be used
    if(parall == FALSE){
        assign("wind", wind, envir = get(env.name, env = sys.nframe(), inherit = FALSE)) # wind assigned in a new envir for test
        assign("data", data, envir = get(env.name, env = sys.nframe(), inherit = FALSE)) # data assigned in a new envir for test
        tempo.message <- fun_get_message(data="lapply(X = wind$left, Y = data, FUN = function(X, Y){res <- get(left)(Y, X) ; return(res)})", kind = "error", header = FALSE, env = get(env.name, env = sys.nframe(), inherit = FALSE), print.no = FALSE) # no env = sys.nframe(), inherit = FALSE in get(left) because look for function in the classical scope
        # rm(env.name) # optional, because should disappear at the end of the function execution
    }else{
        tempo.message <- "ERROR" # with this, force the parallelization by default
    }
    # end test if lapply can be used
    if( ! any(grepl(x = tempo.message, pattern = "ERROR.*"), na.rm = TRUE)){
        left.log <- lapply(X = wind$left, Y = data, FUN = function(X, Y){
            res <- get(left)(Y, X) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            return(res)
        })
        right.log <- lapply(X = wind$right, Y = data, FUN = function(X, Y){
            res <- get(right)(Y, X) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            return(res)
        })
        log <- mapply(FUN = "&", left.log, right.log, SIMPLIFY = FALSE)
        # output
        output <- eval(parse(text = paste0("sapply(lapply(log, FUN = function(X){(data[X])}), FUN = fun", if( ! is.null(args)){paste0(", ", args)}, ")"))) # take the values of the data vector according to log (list of logical, each compartment of length(data)) and apply fun with args of fun
        if(length(output) != nrow(wind)){
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 3")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            output <- data.frame(wind, value = output, stringsAsFactors = TRUE)
        }
    }else{
        if(verbose == TRUE){
            tempo.cat <- paste0("PARALLELIZATION INITIATED AT: ", ini.date)
            cat(paste0("\n", tempo.cat, "\n"))
        }
        tempo.thread.nb = parallel::detectCores(all.tests = FALSE, logical = TRUE) # detect the number of threads
        if( ! is.null(thread.nb)){
            if(tempo.thread.nb < thread.nb){
                thread.nb <- tempo.thread.nb
                if(verbose == TRUE){
                    tempo.cat <- paste0("ONLY: ", tempo.thread.nb, " THREADS AVAILABLE")
                    cat(paste0("\n", tempo.cat, "\n"))
                }
            }
        }else{
            thread.nb <- tempo.thread.nb
        }
        if(verbose == TRUE){
            tempo.cat <- paste0("NUMBER OF THREADS USED: ", thread.nb)
            cat(paste0("\n    ", tempo.cat, "\n"))
        }
        Clust <- parallel::makeCluster(thread.nb, outfile = paste0(res.path, "/fun_slide_parall_log.txt")) # outfile to print or cat during parallelization (only possible in a file, outfile = "" do not work on windows)
        cluster.list <- parallel::clusterSplit(Clust, 1:nrow(wind)) # split according to the number of cluster
        if(verbose == TRUE){
            tempo.cat <- paste0("SPLIT OF TEST NUMBERS IN PARALLELISATION:")
            cat(paste0("\n    ", tempo.cat, "\n"))
            str(cluster.list) # using print(str()) add a NULL below the result
            cat("\n")
        }
        paral.output.list <- parallel::clusterApply( #
            cl = Clust,
            x = cluster.list,
            function.name = function.name, 
            data = data, 
            FUN = fun, # because fun argument of clusterApply
            args = args, 
            thread.nb = thread.nb, 
            print.count = print.count, 
            wind = wind, 
            left = left, 
            right = right, 
            res.path = res.path, 
            lib.path = lib.path, 
            verbose = verbose, 
            cute.path = cute.path, 
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
        verbose, 
        cute.path
            ){
                # check again: very important because another R
                process.id <- Sys.getpid()
                cat(paste0("\nPROCESS ID ", process.id, " -> TESTS ", x[1], " TO ", x[length(x)], "\n"))
                source(cute.path, local = .GlobalEnv)
                fun_pack(req.package = "lubridate", lib.path = lib.path, load = TRUE) # load = TRUE to be sure that functions are present in the environment. And this prevent to use R.lib.path argument of fun_python_pack()
                # end check again: very important because another R
                ini.date <- Sys.time()
                ini.time <- as.numeric(ini.date) # time of process begin, converted into 
                output <- NULL
                print.count.loop <- 0
                for(i4 in 1:length(x)){
                    print.count.loop <- print.count.loop + 1
                    log <- get(left)(data, wind$left[x[i4]]) & get(right)(data, wind$right[x[i4]]) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
                    output <- c(output, eval(parse(text = paste0("FUN(data[log]", if( ! is.null(args)){paste0(", ", args)}, ")"))))
                    if(verbose == TRUE){
                        if(print.count.loop == print.count){
                            print.count.loop <- 0
                            tempo.time <- as.numeric(Sys.time())
                            tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))
                            final.loop <- (tempo.time - ini.time) / i4 * length(x) # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
                            final.exp <- as.POSIXct(final.loop, origin = ini.date)
                            cat(paste0("\nIN PROCESS ", process.id, " | LOOP ", format(i4, big.mark=","), " / ", format(length(x), big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
                        }
                        if(i4 == length(x)){
                            tempo.time <- as.numeric(Sys.time())
                            tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))
                            cat(paste0("\nPROCESS ", process.id, " ENDED | LOOP ", format(i4, big.mark=","), " / ", format(length(x), big.mark=","), " | TIME SPENT: ", tempo.lapse, "\n\n"))
                        }
                    }
                }
                wind <- wind[x, ]
                if(length(output) != nrow(wind)){
                    tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 4")
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else{
                    output <- data.frame(wind, value = output, stringsAsFactors = TRUE)
                    return(output)
                }
            }
        )
        parallel::stopCluster(Clust)
        # result assembly
        output <- data.frame()
        for(i2 in 1:length(paral.output.list)){ # compartment relatives to each parallelization
            output <- rbind(output, paral.output.list[[i2]], stringsAsFactors = TRUE)
        }
        # end result assembly
        if(nrow(output) != nrow(wind)){
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 5\nlength(output): ", length(output), "\nnrow(wind): ", nrow(wind))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            output <- output[order(output$left), ]
        }
    }
    if(verbose == TRUE){
        end.date <- Sys.time()
        end.time <- as.numeric(end.date)
        total.lapse <- round(lubridate::seconds_to_period(end.time - ini.time))
        cat(paste0("\nfun_slide JOB END\n\nTIME: ", end.date, "\n\nTOTAL TIME LAPSE: ", total.lapse, "\n\n\n"))
    }
    return(output)
    # end output
    # end main code
}
