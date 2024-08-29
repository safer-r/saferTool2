#' @title codon2aa
#' @description
#' Convert codon to amino acid using standard genetic code indicated in https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables.
#' @param data Single caracter string of three characters, or vector of three characters, indicating the DNA codon (only "A", "T", "G" and "C" allowed). Case insensitive. Omitted if display argument is TRUE.
#' @param display Single logical value. Display the whole genetic table? if TRUE, override data.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns The 1 letter uppercase amino acid of the submitted codon or the whole table if display argument is TRUE.
#' @seealso \code{\link[Biostrings]{translate}}
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' codon2aa(data = "ATC", display = TRUE)
#' @importFrom saferDev arg_check
#' @export
codon2aa <- function(
        data,
        display = FALSE,
        safer_check = TRUE
){
    # DEBUGGING
    # data = "atg" ; display = FALSE ; safer_check = TRUE
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
    if(safer_check == TRUE){.pack_and_function_check(
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
        "data"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "),base::missing("), "))")))
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    # argument checking with arg_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(arg.check = base::c(arg.check, tempo$problem) , text.check = base::c(text.check, tempo$text) , checked.arg.names = base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = data, class = "vector", typeof = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = display, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(arg.check)){
        if(base::any(arg.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because base::is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "data", 
        "display",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    if(base::length(data) == 1L){
        data <- base::unlist(base::strsplit(data, split = ""))
    }else if(base::length(data) != 3L){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\ndata ARGUMENT MUST BE A STRING OF THREE CHARACTERS OR A VECTOR OF THREE CHARACTERS, MADE OF \"A\", \"C\", \"G\", \"T\" ONLY")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! base::all(base::toupper(data) %in% base::c("A", "C", "G","T"))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\ndata ARGUMENT MUST BE A STRING OF THREE CHARACTERS OR A VECTOR OF THREE CHARACTERS, MADE OF \"A\", \"C\", \"G\", \"T\" ONLY")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end other checkings
    # end second round of checking and data preparation

    # main code
    # standard genetic code
    sgc <- base::array(
        base::c(
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
        dim = base::c(4, 4, 4),
        dimnames = base::list(
            first = base::c("T", "C", "A", "G"), 
            second = base::c("T", "C", "A", "G"), 
            third = base::c("T", "C", "A", "G")
        )
    )
    # end standard genetic code
    # output
    # warning output
    # end warning output
    if(display == TRUE){
        output <- sgc
    }else{
        data <- base::toupper(data)
        output <- base::eval(base::parse(text = base::paste0("sgc['", base::paste0(data, collapse = "','"), "']")))
    }
    base::return(output)
    # end output
    # end main code
}
