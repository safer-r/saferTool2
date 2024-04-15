#' @title codon_finder
#' @description
#' Gives the codon number and position in the codon of nucleotid positions.
#' @param pos Vector of integers indicating the positions of nucleotids in a sequence. Must be between begin and end arguments.
#' @param begin Single integer indicating the position of the first base of the coding sequence.
#' @param end Single indicating the position of the last base of the coding sequence.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns
#' A data frame with column names:
#' 
#' - pos: values of the pos argument.
#' 
#' - codon_nb: the codon number in the CDS encompassing the pos value.
#' 
#' - codon_pos: the position of pos in the codon (either 1, 2 or 3).
#' 
#' - codon_begin: the first base position of the codon.
#' 
#' - codon_end: the last base position of the codon.
#' @details 
#' WARNINGS
#' 
#' Only for coding sequences (no introns): ((end - begin) + 1) / 3 must be an integer (i.e., modulo zero)
#' 
#' Negatives positions allowed but this implies that one base has the position 0 in the sequence
#' @examples
#' codon_finder(c(5, 6, 8, 10), begin = 5, end = 10)
#' 
#' codon_finder(c(0, 5, 6, 8, 10), begin = -2, end = 12)
#' @importFrom saferDev arg_check
#' @export
codon_finder <- function(
        pos, 
        begin, 
        end,
        safer_check = TRUE
){
    # DEBUGGING
    # pos = c(5, 6, 8, 10) ; begin = 5 ; end = 10 ; safer_check = TRUE
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
                "saferDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name,
        external.package.name = package.name
    )
    }
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "pos", 
        "begin", 
        "end"
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
    tempo <- saferDev::arg_check(data = pos, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = begin, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = end, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
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
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "pos", 
        "begin", 
        "end",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = is.null)
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
    if(begin >= end){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: end ARGUMENT MUST BE STRICTLY GREATER THAN begin ARGUMENT")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if((end - begin + 1) %% 3 != 0L){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: ((end - begin) + 1) / 3 MUST BE AN INTEGER (I.E., MODULO ZERO)")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(base::any(pos < begin | pos > end, na.rm = TRUE)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: pos ARGUMENT VALUES MUST BE BETWEEN begin AND end ARGUMENT VALUES")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    first <- base::seq.int(from = begin, to = end, by = 3)
    last <- base::seq.int(from = begin + 2, to = end, by = 3)
    tempo <- base::lapply(X = pos, FUN = function(x){
        tempo.log <- x >= first & x <= last
        if(base::sum(tempo.log, na.rm = TRUE) != 1){ # check that 1 possible TRUE
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: INTERNAL ERROR. CODE HAS TO BE MODIFIED")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            codon_nb <- base::which(tempo.log)
            codon_pos <- base::as.integer((x - (begin + (codon_nb - 1) * 3) + 1))
            codon_begin <- base::as.integer(first[tempo.log])
            codon_end <- base::as.integer(last[tempo.log])
        }
        base::return(base::data.frame(codon_nb = codon_nb, codon_pos = codon_pos, codon_begin = codon_begin, codon_end = codon_end))
    })
    tempo <- base::do.call("rbind", tempo)
    # output
    # warning output
    # end warning output
    output <- base::data.frame(pos = base::as.integer(pos), tempo)
    base::return(output)
    # end output
    # end main code
}