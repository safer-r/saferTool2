#' @title permut
#' @description
#' Reorder the elements of the data1 vector by flipping 2 randomly selected consecutive positions either:
#' 
#' - n times (when n is precised) or.
#' 
#' - until the correlation between data1 and data2 decreases down to the cor.limit (0.2 by default). See cor.limit below to deal with negative correlations.
#' 
#' Example of consecutive position flipping: ABCD -> BACD -> BADC, etc.
#' 
#' Designed for discrete values, but works also for continuous values.
#' @param data1 A vector of at least 2 elements. Must be numeric if data2 is specified.
#' @param data2 A numeric vector of same length as data1.
#' @param n Single numeric value of times "flipping 2 randomly selected consecutive positions". Ignored if data2 is specified.
#' @param seed Single integer number used by set.seed(). Write NULL if random result is required, an integer otherwise. BEWARE: if not NULL, permut() will systematically return the same result when the other parameters keep the same settings.
#' @param print.count Single integer value. Print a working progress message every print.count during loops. BEWARE: can increase substentially the time to complete the process using a small value, like 10 for instance. Use Inf is no loop message desired.
#' @param text.print Single character string indicating the optional message to add to the working progress message every print.count loop.
#' @param cor.method Correlation method. Either "pearson", "kendall" or "spearman". Ignored if data2 is not specified.
#' @param cor.limit Single positive numeric proportion fixing the correlation limit. Ignored if data2 is not specified. Compute the correlation between data1 and data2, permute the data1 values, and stop the permutation process when the correlation between data1 and data2 decreases down below the cor limit value (0.2 by default). If cor(data1, data2) is negative, then -cor.limit is used and the process stops until the correlation between data1 and data2 increases up over cor.limit (-0.2 by default). BEWARE: write a positive cor.limit even if cor(data1, data2) is known to be negative. The function will automatically uses -cor.limit. If the initial correlation is already below cor.limit (positive correlation) or over -cor.limit (negative correlation), then the data1 value positions are completely randomized (correlation between data1 and data2 is expected to be 0).
#' @param warn.print Single logical value. Print warnings at the end of the execution? No print if no warning messages
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @returns
#' A list containing:
#' 
#' - $data: the modified vector.
#' 
#' - $warn: potential warning messages (in case of negative correlation when data2 is specified). NULL if non warning message.
#' 
#' - $cor: a spearman correlation between the initial positions (1:length(data1) and the final positions if data2 is not specified and the final correlation between data1 and data2 otherwise, according to cor.method.
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
#' @examples
#' permut(data1 = 1:10, data2 = 10:1, seed = 1, print.count = 1e4, text.print = "", cor.method = "spearman", cor.limit = 0.7)
#' 
#' permut(data1 = c(0,0,0,0,0), n = 5, data2 = NULL, seed = 1, print.count = 1e4, cor.limit = 0.5)
#' @importFrom saferDev arg_check
#' @importFrom saferTool round2
#' @importFrom lubridate seconds_to_period
#' @importFrom stats cor
#' @export
permut <- function(
        data1, 
        data2 = NULL, 
        n = NULL, 
        seed = NULL, 
        print.count = 10, 
        text.print = "", 
        cor.method = "spearman", 
        cor.limit = 0.2, 
        warn.print = FALSE, 
        lib.path = NULL
){
    # DEBUGGING
    # data1 = LETTERS[1:5] ; data2 = NULL ; n = 1e6 ; seed = NULL ; print.count = 1e3 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.2 ; warn.print = TRUE ; lib.path = NULL
    # data1 = LETTERS[1:5] ; data2 = NULL ; n = 10 ; seed = 22 ; print.count = 10 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.2 ; warn.print = TRUE ; lib.path = NULL
    # data1 = 101:110 ; data2 = 21:30 ; n = 10 ; seed = 22 ; print.count = 10 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.2 ; warn.print = TRUE ; lib.path = NULL
    # data1 = 1:1e3 ; data2 = 1e3:1 ; n = 20 ; seed = 22 ; print.count = 1e6 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.5 ; warn.print = TRUE ; lib.path = NULL
    # package name
    package.name <- "saferTool2"
    # end package name
    # function name
    ini <- match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    
    # package checking
    # check of lib.path
    # end check of lib.path
    
    # check of the required function from the required packages
    .pack_and_function_check(
        fun = c(
            "saferDev::arg_check",
            "saferTool::round2",
            "lubridate::seconds_to_period"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "data1"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check <- c(argum.check, tempo$problem) , text.check = c(text.check, tempo$text) , checked.arg.names = c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = data1, class = "vector", fun.name = function.name) ; eval(ee)
    if(tempo$problem == FALSE & length(data1) < 2){
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data1 ARGUMENT MUST BE A VECTOR OF MINIMUM LENGTH 2. HERE IT IS: ", length(data1))
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    if( ! is.null(data2)){
        tempo <- saferDev::arg_check(data = data1, class = "vector", mode = "numeric", fun.name = function.name) ; eval(ee)
        if(tempo$problem == TRUE){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data1 MUST BE A NUMERIC VECTOR IF data2 ARGUMENT IS SPECIFIED")
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
        tempo <- saferDev::arg_check(data = data2, class = "vector", mode = "numeric", fun.name = function.name) ; eval(ee)
        if(length(data1) != length(data2)){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data1 AND data2 MUST BE VECTOR OF SAME LENGTH. HERE IT IS ", length(data1)," AND ", length(data2))
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
    }else if(is.null(n)){
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: n ARGUMENT CANNOT BE NULL IF data2 ARGUMENT IS NULL")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    if( ! is.null(n)){
        tempo <- saferDev::arg_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(seed)){
        tempo <- saferDev::arg_check(data = seed, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = TRUE, fun.name = function.name) ; eval(ee)
    }
    tempo <- saferDev::arg_check(data = print.count, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = text.print, class = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = cor.method, options = c("pearson", "kendall", "spearman"), length =1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = cor.limit, class = "vector", mode = "numeric", prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = warn.print, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(lib.path)){
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
                tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-c(
        "data1", 
        # "data2", # inactivated because can be null 
        # "n", # inactivated because can be null 
        # "seed", # inactivated because can be null 
        "print.count", 
        "text.print", 
        "cor.method", 
        "cor.limit", 
        "warn.print"
        # lib.path # inactivated because can be null
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    # end warning initiation
    
    # other checkings
    # end other checkings
    
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # end second round of checking and data preparation
    
    # main code
    # code that protects set.seed() in the global environment
    # see also Protocol 100-rev0 Parallelization in R.docx
    if(exists(".Random.seed", envir = .GlobalEnv)){ # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo.random.seed <- .Random.seed
        on.exit(assign(".Random.seed", tempo.random.seed, envir = .GlobalEnv))
    }else{
        on.exit(set.seed(NULL)) # inactivate seeding -> return to complete randomness
    }
    set.seed(seed)
    # end code that protects set.seed() in the global environment
    ini.date <- Sys.time() # time of process begin, converted into seconds
    ini.time <- as.numeric(ini.date) # time of process begin, converted into seconds
    ini.pos <- 1:length(data1) # positions of data1 before permutation loops
    tempo.pos <- ini.pos # positions of data1 that will be modified during loops
    # pos.selec.seq <- ini.pos[-length(data1)] # selection of 1 position in initial position, without the last because always up permutation (pos -> pos+1 & pos+1 -> pos)
    pos.selec.seq.max <- length(ini.pos) - 1 # max position (used by sample.int() function). See  below for - 1
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    count <- 0
    round <- 0
    BREAK <- FALSE
    tempo.cor <- 0
    if(is.null(data2)){
        if(length(table(data1)) == 1L){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", names(table(data1)))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
        }else{
            if(print.count > n){
                print.count <- n
            }
            cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FOR LOOP OF ", n, " LOOPS INITIATED | LOOP COUNT: ", format(count, big.mark=",")))
            print.count.loop <- logical(length = print.count)
            print.count.loop[length(print.count.loop)] <- TRUE # not this to avoid long vector, but not forget to reset during printing: print.count.loop[(1:trunc(n / print.count) * print.count)] <- TRUE # counter to speedup
            count.loop <- 0
            pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
            tempo.date.loop <- Sys.time()
            tempo.time.loop <- as.numeric(tempo.date.loop)
            for(i3 in 1:n){
                count.loop <- count.loop + 1
                pos2 <- pos[count.loop] # selection of 1 position
                tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                if(print.count.loop[count.loop]){
                    count.loop <- 0
                    pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
                    tempo.time <- as.numeric(Sys.time())
                    tempo.lapse <- saferTool::round2(as.numeric(lubridate::seconds_to_period(tempo.time - tempo.time.loop)))
                    final.loop <- (tempo.time - tempo.time.loop) / i3 * n # expected duration in seconds
                    final.exp <- as.POSIXct(final.loop, origin = tempo.date.loop)
                    cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FOR LOOP ", i3, " / ", n, " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
                }
            }
            count <- count + n # out of the loop to speedup
            cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FOR LOOP ENDED | LOOP COUNT: ", format(count, big.mark=",")))
            cat("\n\n")
        }
    }else{
        if(length(table(data1)) == 1L){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", names(table(data1)))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
            tempo.cor <- 1
        }else if(length(table(data2)) == 1L){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") NO PERMUTATION PERFORMED BECAUSE data2 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", names(table(data2)))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
            tempo.cor <- 1
        }else{
            cor.ini <- stats::cor(x = data1, y = data2, use = "pairwise.complete.obs", method = cor.method)
            tempo.cor <- cor.ini # correlation that will be modified during loops
            neg.cor <- FALSE
            if(tempo.cor < 0){
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") INITIAL ", toupper(cor.method), " CORRELATION BETWEEN data1 AND data2 HAS BEEN DETECTED AS NEGATIVE: ", tempo.cor, ". THE LOOP STEPS WILL BE PERFORMED USING POSITIVE CORRELATIONS BUT THE FINAL CORRELATION WILL BE NEGATIVE")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
                neg.cor <- TRUE
                tempo.cor <- abs(tempo.cor)
                cor.ini <- abs(cor.ini)
            }
            if(tempo.cor < cor.limit){ # randomize directly all the position to be close to correlation zero
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") INITIAL ABSOLUTE VALUE OF THE ", toupper(cor.method), " CORRELATION ", saferTool::round2(tempo.cor), " BETWEEN data1 AND data2 HAS BEEN DETECTED AS BELOW THE CORRELATION LIMIT PARAMETER ", cor.limit, "\nTHE data1 SEQUENCE HAS BEEN COMPLETELY RANDOMIZED TO CORRESPOND TO CORRELATION ZERO")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
                for(i4 in 1:5){ # done 5 times to be sure of the complete randomness
                    tempo.pos <- sample(x = tempo.pos, size = length(tempo.pos), replace = FALSE)
                }
                count <- count + 5 # out of the loop to speedup
            }else{
                # smallest correlation decrease
                count <- count + 1 # 1 and not 0 because already 1 performed just below
                pos <- sample.int(n = pos.selec.seq.max , size = 1, replace = TRUE) # selection of 1 position # pos.selec.seq.max  because selection of 1 position in initial position, without the last because always up permutation (pos -> pos+1 & pos+1 -> pos)
                tempo.pos[c(pos + 1, pos)] <- tempo.pos[c(pos, pos + 1)]
                tempo.cor <- abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
                smallest.cor.dec <- cor.ini - tempo.cor
                # end smallest correlation decrease
                # going out of tempo.cor == cor.ini
                cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "CORRELATION DECREASE AFTER A SINGLE PERMUTATION: ", saferTool::round2(smallest.cor.dec, 4)))
                cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FIRST WHILE LOOP STEP -> GOING OUT FROM EQUALITY | LOOP COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4)))
                print.count.loop <- logical(length = print.count)
                print.count.loop[length(print.count.loop)] <- TRUE # counter to speedup
                count.loop <- 0 # 
                pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                tempo.date.loop <- Sys.time()
                tempo.time.loop <- as.numeric(tempo.date.loop)
                while(tempo.cor == cor.ini){ # to be out of equality between tempo.cor and cor.ini at the beginning (only valid for very long vector)
                    count <- count + 1
                    count.loop <- count.loop + 1
                    pos2 <- pos[count.loop]
                    tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                    tempo.cor <- abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
                    if(print.count.loop[count.loop]){
                        count.loop <- 0
                        pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
                        tempo.time <- as.numeric(Sys.time())
                        tempo.lapse <- saferTool::round2(as.numeric(lubridate::seconds_to_period(tempo.time - tempo.time.loop)))
                        cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FIRST WHILE LOOP STEP", format(count.loop, big.mark=","), " / ? | COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4), " | TIME SPENT: ", tempo.lapse))
                    }
                }
                tempo.time <- as.numeric(Sys.time())
                tempo.lapse <- saferTool::round2(as.numeric(lubridate::seconds_to_period(tempo.time - ini.time)))
                cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FIRST WHILE LOOP STEP END | LOOP COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4), " | TOTAL SPENT TIME: ", tempo.lapse))
                if(tempo.cor < cor.limit){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") THE FIRST FOR & WHILE LOOP STEPS HAVE BEEN TOO FAR AND SUBSEQUENT LOOP STEPS WILL NOT RUN")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
                # end going out of tempo.cor == cor.ini
                # estimation of the average correlation decrease per loop on x loops and for loop execution
                cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "WHILE/FOR LOOPS INITIATION | LOOP COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4)))
                count.est <- 1e5
                first.round <- TRUE
                GOBACK <- FALSE
                while(tempo.cor > cor.limit){
                    round <- round + 1
                    # estimation step
                    if(first.round == TRUE){
                        first.round <- FALSE
                        cor.dec.per.loop <- numeric(length = 5)
                        loop.nb.est <- Inf
                        cor.est.ini <- tempo.cor
                        cor.est <- numeric(length = 5)
                        for(i6 in 1:5){ # connected to cor.dec.per.loop
                            tempo.pos.est <- tempo.pos
                            pos <- sample.int(n = pos.selec.seq.max , size = count.est, replace = TRUE) # selection of n position
                            for(i7 in 1:count.est){
                                pos2 <- pos[i7] # selection of 1 position
                                tempo.pos.est[c(pos2 + 1, pos2)] <- tempo.pos.est[c(pos2, pos2 + 1)]
                            }
                            tempo.cor.est <- abs(stats::cor(x = data1[tempo.pos.est], y = data2, use = "pairwise.complete.obs", method = cor.method))
                            cor.est[i6] <- tempo.cor.est
                            tempo.cor.dec.per.loop <- (cor.est.ini - tempo.cor.est) / count.est # correlation decrease per loop
                            if(is.na(tempo.cor.dec.per.loop) | ! is.finite(tempo.cor.dec.per.loop)){
                                tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 2\ncor.est.ini: ", cor.est.ini, "\ntempo.cor.est: ", tempo.cor.est)
                                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                            }
                            cor.dec.per.loop[i6] <- tempo.cor.dec.per.loop
                        }
                        cor.est <- cor.est[which.max(cor.dec.per.loop)] # max to avoid to go to far with for loop (tempo.cor below tempo.limit)
                        cor.dec.per.loop <- max(cor.dec.per.loop, na.rm = TRUE) # max to avoid to go to far with for loop (tempo.cor below tempo.limit)
                        loop.nb.est <- saferTool::round2((tempo.cor - cor.limit) / cor.dec.per.loop)
                    }else{
                        if(GOBACK == TRUE){
                            loop.nb.est <- saferTool::round2(loop.nb.est / 2)
                        }else{
                            cor.dec.per.loop <- (cor.ini - tempo.cor) / count
                            loop.nb.est <- saferTool::round2((tempo.cor - cor.limit) / cor.dec.per.loop)
                        }
                    }
                    # end estimation step
                    # loop step
                    if(is.na(loop.nb.est) | ! is.finite(loop.nb.est)){
                        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 1\nloop.nb.est: ", loop.nb.est, "\ncor.ini: ", cor.ini, "\ntempo.cor: ", tempo.cor, "\ncor.limit: ", cor.limit, "\ncor.dec.per.loop: ", cor.dec.per.loop)
                        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                    }else if(loop.nb.est > 1e4){ # below -> leave the while loop
                        tempo.pos.secu <- tempo.pos
                        count.secu <- count
                        tempo.cor.secu <- tempo.cor
                        cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "INITIAL SETTINGS BEFORE ROUND: ", round, " | LOOP COUNT: ", format(count, big.mark=","), " | GO BACK: ", GOBACK, " | LOOP NUMBER ESTIMATION: ", format(loop.nb.est, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4)))
                        print.count.loop <- logical(length = print.count)
                        print.count.loop[length(print.count.loop)] <- TRUE # not this to avoid long vector, but not forget to reset during printing: print.count.loop[(1:trunc(n / print.count) * print.count)] <- TRUE # counter to speedup
                        count.loop <- 0
                        pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                        tempo.date.loop <- Sys.time()
                        tempo.time.loop <- as.numeric(tempo.date.loop)
                        for(i6 in 1:loop.nb.est){
                            count.loop <- count.loop + 1
                            pos2 <- pos[count.loop] # selection of 1 position
                            tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                            if(print.count.loop[count.loop]){
                                count.loop <- 0
                                pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
                                tempo.time <- as.numeric(Sys.time())
                                tempo.lapse <- saferTool::round2(as.numeric(lubridate::seconds_to_period(tempo.time - tempo.time.loop)))
                                final.loop <- (tempo.time - tempo.time.loop) / i6 * loop.nb.est # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
                                final.exp <- as.POSIXct(final.loop, origin = tempo.date.loop)
                                cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FOR LOOP | ROUND ", round, " | LOOP: ", format(i6, big.mark=","), " / ", format(loop.nb.est, big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
                            }
                        }
                        count <- count + loop.nb.est # out of the loop to speedup
                        tempo.cor <- abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
                        if(tempo.cor > tempo.cor.secu | ((tempo.cor - cor.limit) < 0 & abs(tempo.cor - cor.limit) > smallest.cor.dec * saferTool::round2(log10(max(ini.pos, na.rm = TRUE))))){
                            GOBACK <- TRUE
                            tempo.pos <- tempo.pos.secu
                            count <- count.secu
                            tempo.cor <- tempo.cor.secu
                        }else{
                            GOBACK <- FALSE
                        }
                    }else{
                        cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FINAL WHILE LOOP | LOOP COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4)))
                        print.count.loop <- logical(length = print.count)
                        print.count.loop[length(print.count.loop)] <- TRUE # counter to speedup
                        count.loop <- 0 # 
                        pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                        tempo.cor.loop <- tempo.cor
                        tempo.date.loop <- Sys.time()
                        tempo.time.loop <- as.numeric(tempo.date.loop)
                        while(tempo.cor > cor.limit){
                            count <- count + 1
                            count.loop <- count.loop + 1
                            pos2 <- pos[count.loop]
                            tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                            tempo.cor <- abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
                            if(print.count.loop[count.loop]){
                                count.loop <- 0
                                pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
                                tempo.time <- as.numeric(Sys.time())
                                tempo.lapse <- saferTool::round2(as.numeric(lubridate::seconds_to_period(tempo.time - tempo.time.loop)))
                                final.loop <- (tempo.time - tempo.time.loop) / (tempo.cor.loop - tempo.cor) * (tempo.cor - cor.limit) # expected duration in seconds # tempo.cor.loop - tempo.cor always positive and tempo.cor decreases progressively starting from tempo.cor.loop
                                final.exp <- as.POSIXct(final.loop, origin = tempo.date.loop)
                                cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "WHILE LOOP | LOOP NB: ", format(count.loop, big.mark=","), " | COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
                            }
                        }
                    }
                }
                tempo.time <- as.numeric(Sys.time())
                tempo.lapse <- saferTool::round2(as.numeric(lubridate::seconds_to_period(tempo.time - ini.time)))
                cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "WHILE/FOR LOOPS END | LOOP COUNT: ", format(count, big.mark=","), " | NB OF ROUNDS: ", round, " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4), " | TOTAL SPENT TIME: ", tempo.lapse))
            }
            tempo.cor <- ifelse(neg.cor == TRUE, -tempo.cor, tempo.cor)
        }
    }
    cat("\n\n")
    if(warn.print == TRUE & ! is.null(warn)){
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE), add = TRUE)
    }
    on.exit(expr = options(warning.length = ini.warning.length), add = TRUE)
    # output
    # warning output
    if(warn.print == TRUE & ! is.null(warn)){
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
      }
      on.exit(expr = options(warning.length = ini.warning.length), add = TRUE)
    # end warning output
    output <- list(data = data1[tempo.pos], warn = warn, cor = if(is.null(data2)){cor(ini.pos, tempo.pos, method = "spearman")}else{tempo.cor}, count = count)
    return(output)
    # end output
    # end main code
}
