#' @title codon_finder
#' @description
#' Gives the codon number and position in the codon of nucleotid positions.
#' @param pos Vector of integers indicating the positions of nucleotids in a sequence. Must be between begin and end arguments.
#' @param begin Single integer indicating the position of the first base of the coding sequence.
#' @param end Single indicating the position of the last base of the coding sequence.
#' @returns
#' A data frame with column names:
#' - pos: values of the pos argument.
#' - codon_nb: the codon number in the CDS encompassing the pos value.
#' - codon_pos: the position of pos in the codon (either 1, 2 or 3).
#' - codon_begin: the first base position of the codon.
#' - codon_end: the last base position of the codon.
#' @details 
#' REQUIRED PACKAGES
#' 
#' cuteDev
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' arg_check()
#'
#'
#' WARNINGS
#' 
#' Only for coding sequences (no introns): ((end - begin) + 1) / 3 must be an integer (i.e., modulo zero)
#' 
#' Negatives positions allowed but this implies that one base has the position 0 in the sequence
#' @examples
#' codon_finder(c(5, 6, 8, 10), begin = 5, end = 10)
#' 
#' codon_finder(c(0, 5, 6, 8, 10), begin = -2, end = 12)
#' @importFrom cuteDev arg_check
#' @export
codon_finder <- function(
        pos, 
        begin, 
        end
){
    # DEBUGGING
    # pos = c(5, 6, 8, 10) ; begin = 5 ; end = 10
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
            "cuteDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "pos", 
        "begin", 
        "end"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check = c(argum.check, tempo$problem) , text.check = c(text.check, tempo$text) , checked.arg.names = c(checked.arg.names, tempo$object.name))
    tempo <- cuteDev::arg_check(data = pos, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = begin, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = end, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using cuteDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
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
        "pos", 
        "begin", 
        "end"
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
    if(begin >= end){
        tempo.cat <- paste0("ERROR IN ", function.name, ": end ARGUMENT MUST BE STRICTLY GREATER THAN begin ARGUMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if((end - begin + 1) %% 3 != 0L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": ((end - begin) + 1) / 3 MUST BE AN INTEGER (I.E., MODULO ZERO)")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(any(pos < begin | pos > end, na.rm = TRUE)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": pos ARGUMENT VALUES MUST BE BETWEEN begin AND end ARGUMENT VALUES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end other checkings
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # end second round of checking and data preparation
    
    # main code
    first <- seq.int(from = begin, to = end, by = 3)
    last <- seq.int(from = begin + 2, to = end, by = 3)
    tempo <- lapply(X = pos, FUN = function(x = X){
        tempo.log <- x >= first & x <= last
        if(sum(tempo.log, na.rm = TRUE) != 1){ # check that 1 possible TRUE
            tempo.cat <- paste0("ERROR IN ", function.name, ": INTERNAL ERROR. CODE HAS TO BE MODIFIED")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            codon_nb <- which(tempo.log)
            codon_pos <- as.integer((x - (begin + (codon_nb - 1) * 3) + 1))
            codon_begin <- as.integer(first[tempo.log])
            codon_end <- as.integer(last[tempo.log])
        }
        return(data.frame(codon_nb = codon_nb, codon_pos = codon_pos, codon_begin = codon_begin, codon_end = codon_end))
    })
    tempo <- do.call("rbind", tempo)
    # output
    # warning output
    # end warning output
    output <- data.frame(pos = as.integer(pos), tempo)
    return(output)
    # end output
    # end main code
}