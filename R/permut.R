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
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
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
        lib.path = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # data1 = LETTERS[1:5] ; data2 = NULL ; n = 1e6 ; seed = NULL ; print.count = 1e3 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.2 ; warn.print = TRUE ; lib.path = NULL ; safer_check = TRUE
    # data1 = LETTERS[1:5] ; data2 = NULL ; n = 10 ; seed = 22 ; print.count = 10 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.2 ; warn.print = TRUE ; lib.path = NULL ; safer_check = TRUE
    # data1 = 101:110 ; data2 = 21:30 ; n = 10 ; seed = 22 ; print.count = 10 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.2 ; warn.print = TRUE ; lib.path = NULL ;safer_check = TRUE
    # data1 = 1:1e3 ; data2 = 1e3:1 ; n = 20 ; seed = 22 ; print.count = 1e6 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.5 ; warn.print = TRUE ; lib.path = NULL ; safer_check = TRUE
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
    # end check of lib.path
    
    # check of the required function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
        fun = base::c(
            "saferDev::arg_check",
            "saferTool::round2",
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
        "data1"
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
    tempo <- saferDev::arg_check(data = data1, class = "vector", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if(tempo$problem == FALSE & base::length(data1) < 2){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data1 ARGUMENT MUST BE A VECTOR OF MINIMUM LENGTH 2. HERE IT IS: ", base::length(data1))
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }
    if( ! base::is.null(data2)){
        tempo <- saferDev::arg_check(data = data1, class = "vector", mode = "numeric", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == TRUE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data1 MUST BE A NUMERIC VECTOR IF data2 ARGUMENT IS SPECIFIED")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
        tempo <- saferDev::arg_check(data = data2, class = "vector", mode = "numeric", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(base::length(data1) != base::length(data2)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data1 AND data2 MUST BE VECTOR OF SAME LENGTH. HERE IT IS ", base::length(data1)," AND ", base::length(data2))
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }else if(base::is.null(n)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: n ARGUMENT CANNOT BE NULL IF data2 ARGUMENT IS NULL")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }
    if( ! base::is.null(n)){
        tempo <- saferDev::arg_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(seed)){
        tempo <- saferDev::arg_check(data = seed, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = print.count, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = text.print, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = cor.method, options = base::c("pearson", "kendall", "spearman"), length =1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = cor.limit, class = "vector", mode = "numeric", prop = TRUE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = warn.print, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(lib.path)){
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE){
            if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-base::c(
        "data1", 
        # "data2", # inactivated because can be null 
        # "n", # inactivated because can be null 
        # "seed", # inactivated because can be null 
        "print.count", 
        "text.print", 
        "cor.method", 
        "cor.limit", 
        "warn.print",
        # lib.path, # inactivated because can be null
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
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    # end warning initiation
    # other checkings
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    # code that protects set.seed() in the global environment
    # see also Protocol 100-rev0 Parallelization in R.docx
    if(base::exists(".Random.seed", envir = .GlobalEnv)){ # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo.random.seed <- .Random.seed
        base::on.exit(base::assign(".Random.seed", tempo.random.seed, envir = .GlobalEnv))
    }else{
        base::on.exit(base::set.seed(NULL)) # inactivate seeding -> return to complete randomness
    }
    base::set.seed(seed)
    # end code that protects set.seed() in the global environment
    ini.date <- base::Sys.time() # time of process begin, converted into seconds
    ini.time <- base::as.numeric(ini.date) # time of process begin, converted into seconds
    ini.pos <- 1:base::length(data1) # positions of data1 before permutation loops
    tempo.pos <- ini.pos # positions of data1 that will be modified during loops
    # pos.selec.seq <- ini.pos[-length(data1)] # selection of 1 position in initial position, without the last because always up permutation (pos -> pos+1 & pos+1 -> pos)
    pos.selec.seq.max <- base::length(ini.pos) - 1 # max position (used by sample.int() function). See  below for - 1
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    count <- 0
    round <- 0
    BREAK <- FALSE
    tempo.cor <- 0
    if(base::is.null(data2)){
        if(base::length(base::table(data1)) == 1L){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", base::names(base::table(data1)))
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn))) #
        }else{
            if(print.count > n){
                print.count <- n
            }
           base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "FOR LOOP OF ", n, " LOOPS INITIATED | LOOP COUNT: ", base::format(count, big.mark=",")))
            print.count.loop <- base::logical(length = print.count)
            print.count.loop[base::length(print.count.loop)] <- TRUE # not this to avoid long vector, but not forget to reset during printing: print.count.loop[(1:trunc(n / print.count) * print.count)] <- TRUE # counter to speedup
            count.loop <- 0
            pos <- base::sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
            tempo.date.loop <- base::Sys.time()
            tempo.time.loop <- base::as.numeric(tempo.date.loop)
            for(i3 in 1:n){
                count.loop <- count.loop + 1
                pos2 <- pos[count.loop] # selection of 1 position
                tempo.pos[base::c(pos2 + 1, pos2)] <- tempo.pos[base::c(pos2, pos2 + 1)]
                if(print.count.loop[count.loop]){
                    count.loop <- 0
                    pos <- base::sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
                    tempo.time <- base::as.numeric(base::Sys.time())
                    tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(tempo.time - tempo.time.loop)), safer_check = FALSE) 
                    final.loop <- (tempo.time - tempo.time.loop) / i3 * n # expected duration in seconds
                    final.exp <- base::as.POSIXct(final.loop, origin = tempo.date.loop)
                    base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "FOR LOOP ", i3, " / ", n, " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
                }
            }
            count <- count + n # out of the loop to speedup
            base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "FOR LOOP ENDED | LOOP COUNT: ", base::format(count, big.mark=",")))
            base::cat("\n\n")
        }
    }else{
        if(base::length(base::table(data1)) == 1L){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", base::names(base::table(data1)))
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn))) #
            tempo.cor <- 1
        }else if(base::length(base::table(data2)) == 1L){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") NO PERMUTATION PERFORMED BECAUSE data2 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", base::names(base::table(data2)))
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn))) #
            tempo.cor <- 1
        }else{
            cor.ini <- stats::cor(x = data1, y = data2, use = "pairwise.complete.obs", method = cor.method)
            tempo.cor <- cor.ini # correlation that will be modified during loops
            neg.cor <- FALSE
            if(tempo.cor < 0){
                warn.count <- warn.count + 1
                tempo.warn <- base::paste0("(", warn.count,") INITIAL ", base::toupper(cor.method), " CORRELATION BETWEEN data1 AND data2 HAS BEEN DETECTED AS NEGATIVE: ", tempo.cor, ". THE LOOP STEPS WILL BE PERFORMED USING POSITIVE CORRELATIONS BUT THE FINAL CORRELATION WILL BE NEGATIVE")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn))) #
                neg.cor <- TRUE
                tempo.cor <- base::abs(tempo.cor)
                cor.ini <- base::abs(cor.ini)
            }
            if(tempo.cor < cor.limit){ # randomize directly all the position to be close to correlation zero
                warn.count <- warn.count + 1
                tempo.warn <- base::paste0("(", warn.count,") INITIAL ABSOLUTE VALUE OF THE ", base::toupper(cor.method), " CORRELATION ", saferTool::round2(tempo.cor, safer_check = FALSE), " BETWEEN data1 AND data2 HAS BEEN DETECTED AS BELOW THE CORRELATION LIMIT PARAMETER ", cor.limit, "\nTHE data1 SEQUENCE HAS BEEN COMPLETELY RANDOMIZED TO CORRESPOND TO CORRELATION ZERO")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn))) #
                for(i4 in 1:5){ # done 5 times to be sure of the complete randomness
                    tempo.pos <- base::sample(x = tempo.pos, size = base::length(tempo.pos), replace = FALSE)
                }
                count <- count + 5 # out of the loop to speedup
            }else{
                # smallest correlation decrease
                count <- count + 1 # 1 and not 0 because already 1 performed just below
                pos <- base::sample.int(n = pos.selec.seq.max , size = 1, replace = TRUE) # selection of 1 position # pos.selec.seq.max  because selection of 1 position in initial position, without the last because always up permutation (pos -> pos+1 & pos+1 -> pos)
                tempo.pos[base::c(pos + 1, pos)] <- tempo.pos[base::c(pos, pos + 1)]
                tempo.cor <- base::abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
                smallest.cor.dec <- cor.ini - tempo.cor
                # end smallest correlation decrease
                # going out of tempo.cor == cor.ini
                base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "CORRELATION DECREASE AFTER A SINGLE PERMUTATION: ", saferTool::round2(smallest.cor.dec, 4, safer_check = FALSE)))
                base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "FIRST WHILE LOOP STEP -> GOING OUT FROM EQUALITY | LOOP COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE)))
                print.count.loop <- base::logical(length = print.count)
                print.count.loop[base::length(print.count.loop)] <- TRUE # counter to speedup
                count.loop <- 0 # 
                pos <- base::sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                tempo.date.loop <- base::Sys.time()
                tempo.time.loop <- base::as.numeric(tempo.date.loop)
                while(tempo.cor == cor.ini){ # to be out of equality between tempo.cor and cor.ini at the beginning (only valid for very long vector)
                    count <- count + 1
                    count.loop <- count.loop + 1
                    pos2 <- pos[count.loop]
                    tempo.pos[base::c(pos2 + 1, pos2)] <- tempo.pos[base::c(pos2, pos2 + 1)]
                    tempo.cor <- base::abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
                    if(print.count.loop[count.loop]){
                        count.loop <- 0
                        pos <- base::sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
                        tempo.time <- base::as.numeric(base::Sys.time())
                        tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(tempo.time - tempo.time.loop)), safer_check = FALSE)
                        base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "FIRST WHILE LOOP STEP", base::format(count.loop, big.mark=","), " / ? | COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), " | TIME SPENT: ", tempo.lapse))
                    }
                }
                tempo.time <- base::as.numeric(base::Sys.time())
                tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(tempo.time - ini.time)), safer_check = FALSE)
                base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "FIRST WHILE LOOP STEP END | LOOP COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), " | TOTAL SPENT TIME: ", tempo.lapse))
                if(tempo.cor < cor.limit){
                    warn.count <- warn.count + 1
                    tempo.warn <- base::paste0("(", warn.count,") THE FIRST FOR & WHILE LOOP STEPS HAVE BEEN TOO FAR AND SUBSEQUENT LOOP STEPS WILL NOT RUN")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
                }
                # end going out of tempo.cor == cor.ini
                # estimation of the average correlation decrease per loop on x loops and for loop execution
                base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "WHILE/FOR LOOPS INITIATION | LOOP COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE)))
                count.est <- 1e5
                first.round <- TRUE
                GOBACK <- FALSE
                while(tempo.cor > cor.limit){
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
                            tempo.cor.est <- base::abs(stats::cor(x = data1[tempo.pos.est], y = data2, use = "pairwise.complete.obs", method = cor.method))
                            cor.est[i6] <- tempo.cor.est
                            tempo.cor.dec.per.loop <- (cor.est.ini - tempo.cor.est) / count.est # correlation decrease per loop
                            if(base::is.na(tempo.cor.dec.per.loop) | ! base::is.finite(tempo.cor.dec.per.loop)){
                                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 2\ncor.est.ini: ", cor.est.ini, "\ntempo.cor.est: ", tempo.cor.est)
                                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                            }
                            cor.dec.per.loop[i6] <- tempo.cor.dec.per.loop
                        }
                        cor.est <- cor.est[base::which.max(cor.dec.per.loop)] # max to avoid to go to far with for loop (tempo.cor below tempo.limit)
                        cor.dec.per.loop <- base::max(cor.dec.per.loop, na.rm = TRUE) # max to avoid to go to far with for loop (tempo.cor below tempo.limit)
                        loop.nb.est <- saferTool::round2((tempo.cor - cor.limit) / cor.dec.per.loop, safer_check = FALSE)
                    }else{
                        if(GOBACK == TRUE){
                            loop.nb.est <- saferTool::round2(loop.nb.est / 2, safer_check = FALSE)
                        }else{
                            cor.dec.per.loop <- (cor.ini - tempo.cor) / count
                            loop.nb.est <- saferTool::round2((tempo.cor - cor.limit) / cor.dec.per.loop, safer_check = FALSE)
                        }
                    }
                    # end estimation step
                    # loop step
                    if(base::is.na(loop.nb.est) | ! base::is.finite(loop.nb.est)){
                        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 1\nloop.nb.est: ", loop.nb.est, "\ncor.ini: ", cor.ini, "\ntempo.cor: ", tempo.cor, "\ncor.limit: ", cor.limit, "\ncor.dec.per.loop: ", cor.dec.per.loop)
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                    }else if(loop.nb.est > 1e4){ # below -> leave the while loop
                        tempo.pos.secu <- tempo.pos
                        count.secu <- count
                        tempo.cor.secu <- tempo.cor
                        base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "INITIAL SETTINGS BEFORE ROUND: ", round, " | LOOP COUNT: ", base::format(count, big.mark=","), " | GO BACK: ", GOBACK, " | LOOP NUMBER ESTIMATION: ", base::format(loop.nb.est, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE)))
                        print.count.loop <- base::logical(length = print.count)
                        print.count.loop[base::length(print.count.loop)] <- TRUE # not this to avoid long vector, but not forget to reset during printing: print.count.loop[(1:trunc(n / print.count) * print.count)] <- TRUE # counter to speedup
                        count.loop <- 0
                        pos <- base::sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                        tempo.date.loop <- base::Sys.time()
                        tempo.time.loop <- base::as.numeric(tempo.date.loop)
                        for(i6 in 1:loop.nb.est){
                            count.loop <- count.loop + 1
                            pos2 <- pos[count.loop] # selection of 1 position
                            tempo.pos[base::c(pos2 + 1, pos2)] <- tempo.pos[base::c(pos2, pos2 + 1)]
                            if(print.count.loop[count.loop]){
                                count.loop <- 0
                                pos <- base::sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
                                tempo.time <- base::as.numeric(base::Sys.time())
                                tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(tempo.time - tempo.time.loop)), safer_check = FALSE)
                                final.loop <- (tempo.time - tempo.time.loop) / i6 * loop.nb.est # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
                                final.exp <- base::as.POSIXct(final.loop, origin = tempo.date.loop)
                                base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "FOR LOOP | ROUND ", round, " | LOOP: ", base::format(i6, big.mark=","), " / ", base::format(loop.nb.est, big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
                            }
                        }
                        count <- count + loop.nb.est # out of the loop to speedup
                        tempo.cor <- base::abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
                        if(tempo.cor > tempo.cor.secu | ((tempo.cor - cor.limit) < 0 & base::abs(tempo.cor - cor.limit) > smallest.cor.dec * saferTool::round2(base::log10(base::max(ini.pos, na.rm = TRUE)), safer_check = FALSE))){
                            GOBACK <- TRUE
                            tempo.pos <- tempo.pos.secu
                            count <- count.secu
                            tempo.cor <- tempo.cor.secu
                        }else{
                            GOBACK <- FALSE
                        }
                    }else{
                        base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "FINAL WHILE LOOP | LOOP COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE)))
                        print.count.loop <- base::logical(length = print.count)
                        print.count.loop[base::length(print.count.loop)] <- TRUE # counter to speedup
                        count.loop <- 0 # 
                        pos <- base::sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
                        tempo.cor.loop <- tempo.cor
                        tempo.date.loop <- base::Sys.time()
                        tempo.time.loop <- base::as.numeric(tempo.date.loop)
                        while(tempo.cor > cor.limit){
                            count <- count + 1
                            count.loop <- count.loop + 1
                            pos2 <- pos[count.loop]
                            tempo.pos[base::c(pos2 + 1, pos2)] <- tempo.pos[base::c(pos2, pos2 + 1)]
                            tempo.cor <- base::abs(stats::cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
                            if(print.count.loop[count.loop]){
                                count.loop <- 0
                                pos <- base::sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
                                tempo.time <- base::as.numeric(base::Sys.time())
                                tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(tempo.time - tempo.time.loop)), safer_check = FALSE)
                                final.loop <- (tempo.time - tempo.time.loop) / (tempo.cor.loop - tempo.cor) * (tempo.cor - cor.limit) # expected duration in seconds # tempo.cor.loop - tempo.cor always positive and tempo.cor decreases progressively starting from tempo.cor.loop
                                final.exp <- base::as.POSIXct(final.loop, origin = tempo.date.loop)
                                base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "WHILE LOOP | LOOP NB: ", base::format(count.loop, big.mark=","), " | COUNT: ", base::format(count, big.mark=","), " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
                            }
                        }
                    }
                }
                tempo.time <- base::as.numeric(base::Sys.time())
                tempo.lapse <- saferTool::round2(base::as.numeric(lubridate::seconds_to_period(tempo.time - ini.time)), safer_check = FALSE)
                base::cat(base::paste0("\n", base::ifelse(text.print == "", "", base::paste0(text.print, " | ")), "WHILE/FOR LOOPS END | LOOP COUNT: ", base::format(count, big.mark=","), " | NB OF ROUNDS: ", round, " | CORRELATION LIMIT: ", saferTool::round2(cor.limit, 4, safer_check = FALSE), " | ABS TEMPO CORRELATION: ", saferTool::round2(tempo.cor, 4, safer_check = FALSE), " | TOTAL SPENT TIME: ", tempo.lapse))
            }
            tempo.cor <- base::ifelse(neg.cor == TRUE, -tempo.cor, tempo.cor)
        }
    }
    base::cat("\n\n")
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE), add = TRUE)
    }
    base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    # output
    # warning output
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
      }
      base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    # end warning output
    output <- base::list(data = data1[tempo.pos], warn = warn, cor = if(base::is.null(data2)){stats::cor(ini.pos, tempo.pos, method = "spearman")}else{tempo.cor}, count = count)
    base::return(output)
    # end output
    # end main code
}
