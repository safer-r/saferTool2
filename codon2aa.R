#' @title codon2aa
#' @description
#' Convert codon to amino acid using standard genetic code indicated in https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables.
#' @param data Single caracter string of three characters, or vector of three characters, indicating the DNA codon (only "A", "T", "G" and "C" allowed). Case insensitive. Omitted if display argument is TRUE.
#' @param display Single logical value. Display the whole genetic table? if TRUE, override data.
#' @returns The 1 letter uppercase amino acid of the submitted codon or the whole table if display argument is TRUE.
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
#' None
#' @examples
#' fun_codon2aa(data = "ATC", display = TRUE)
#' @importFrom cuteDev arg_check
#' @export
fun_codon2aa <- function(
        data,
        display = FALSE
){
    # DEBUGGING
    # data = "atg" ; display = FALSE
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # required function checking
    req.function <- c(
        "fun_check"
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
        "data"
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
    tempo <- fun_check(data = data, class = "vector", typeof = "character", fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = display, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
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
        "data", 
        "display"
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
    if(length(data) == 1L){
        data <- unlist(strsplit(data, split = ""))
    }else if(length(data) != 3L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": data ARGUMENT MUST BE A STRING OF THREE CHARACTERS OR A VECTOR OF THREE CHARACTERS, MADE OF \"A\", \"C\", \"G\", \"T\" ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! all(toupper(data) %in% c("A", "C", "G","T"))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": data ARGUMENT MUST BE A STRING OF THREE CHARACTERS OR A VECTOR OF THREE CHARACTERS, MADE OF \"A\", \"C\", \"G\", \"T\" ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end other checkings
    # reserved word checking
    # end reserved word checking
    # end second round of checking and data preparation
    # package checking
    # end package checking
    # main code
    # standard genetic code
    sgc <- array(
        c(
            "F", "L", "I", "V",
            "S", "P", "T", "A",
            "Y", "H", "N", "D",
            "C", "R", "S", "G",
            
            "F", "L", "I", "V",
            "S", "P", "T", "A",
            "Y", "H", "N", "D",
            "C", "R", "S", "G",
            
            "L", "L", "I", "V",
            "S", "P", "T", "A",
            "stop", "Q", "K", "E",
            "stop", "R", "R", "G",
            
            "L", "L", "M", "V",
            "S", "P", "T", "A",
            "stop", "Q", "K", "E",
            "W", "R", "R", "G"
        ), 
        dim = c(4, 4, 4),
        dimnames = list(
            first = c("T", "C", "A", "G"), 
            second = c("T", "C", "A", "G"), 
            third = c("T", "C", "A", "G")
        )
    )
    # end standard genetic code
    # output
    if(display == TRUE){
        output <- sgc
    }else{
        data <- toupper(data)
        output <- eval(parse(text = paste0("sgc['", paste0(data, collapse = "','"), "']")))
    }
    return(output)
    # end output
    # end main code
}
