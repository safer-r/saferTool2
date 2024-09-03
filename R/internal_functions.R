#' @title .pack_and_function_check
#' @description
#' Check if 1) required functions are present in required packages and 2) required packages are installed locally.
#' Simplified version of saferDev::is_function_here(), used as internal function for the other functions of the package.
#' @param fun Character vector of the names of the required functions, preceded by the name of the package they belong to and a double colon. Example: c("ggplot2::geom_point", "grid::gpar").
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the listed packages in the fun argument, if not in the default directories. If NULL, the function checks only in the .libPaths() default R library folders.
#' @param external.function.name Name of the function using the .pack_and_function_check() function.
#' @param external.package.name Name of the package of the function using the .pack_and_function_check() function.
#' @returns An error message if at least one of the checked packages is missing in lib.path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' # .pack_and_function_check(fun = "ggplot2::notgood") # commented because this example returns an error
#' \dontrun{
#' # Example that shouldn't be run because this is an internal function
#' .pack_and_function_check(fun = c("ggplot2::geom_point", "grid::gpar"))
#' }
#' @keywords internal
#' @rdname internal_function


.pack_and_function_check <- function(
        fun, 
        lib.path,
        external.function.name,
        external.package.name
){
    # AIM
    # Check for the presence of required package::functions in the system  
    # WARNING
    # arguments of the .pack_and_function_check() function are not checked, so use carefully inside other functions
    # ARGUMENTS
    # fun: vector of string of the package::function names to check
    # lib.path: path of the library folder in the system
    # external.function.name: function name
    # external.package.name: package name
    # RETURN
    # An error message or nothing 
    # DEBUGGING
    # fun = "ggplot2::geom_point" ; lib.path = "C:/Program Files/R/R-4.3.1/library" ; external.function.name = "fun1" ; external.package.name = "1"
    # check of lib.path
    # full check already done in the main function
    if(base::is.null(lib.path)){
        lib.path <- base::.libPaths() # base::.libPaths(new = lib.path) # or base::.libPaths(new = base::c(base::.libPaths(), lib.path))
    }
    # end check of lib.path
    # main code
    tempo.log <- base::grepl(x = fun, pattern = "^.+::.+$")
    if( ! base::all(tempo.log)){
        tempo.cat <- base::paste0("ERROR IN THE CODE OF THE ", external.function.name, " OF THE ", external.package.name, " PACKAGE\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\":\n", base::paste(fun[ ! tempo.log], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    pkg.fun.name.list <- base::strsplit(fun, "::") # package in 1 and function in 2
    pkg.name <- base::sapply(X = pkg.fun.name.list, FUN = function(x){x[1]})
    pkg.log <- pkg.name %in% base::rownames(utils::installed.packages(lib.loc = lib.path))
    if( ! base::all(pkg.log)){
        tempo <- pkg.name[ ! pkg.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            external.function.name, 
            " OF THE ", external.package.name, " PACKAGE\nREQUIRED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n"))), 
            "\nMUST BE INSTALLED IN", 
            base::ifelse(base::length(lib.path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            base::paste(lib.path, collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    fun.log <- base::sapply(X = pkg.fun.name.list, FUN = function(x){base::exists(x[2], envir = base::asNamespace(x[1]))})
    if( ! base::all(fun.log)){
        tempo <- fun[ ! fun.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            external.function.name, 
            " OF THE ", external.package.name, " PACKAGE\nREQUIRED FUNCTION",
            base::ifelse(base::length(tempo) == 1L, " IS ", "S ARE "), 
            "MISSING IN THE INSTALLED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n")))
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
}


#' @title .base_op_check
#' @description
#' Check if critical operators of R are not present in other packages or in the global env.
#' Others functions of the R scope can be overwritten because safer functions always use :: when using any function.
#' @param external.function.name Name of the function using the .pack_and_function_check() function.
#' @param external.package.name Name of the package of the function using the .pack_and_function_check() function.
#' @returns An error message if at least one of the checked operator is present in the R scope, nothing otherwise.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' \dontrun{
#' # Example that shouldn't be run because this is an internal function
#' assign("!", 1) ; .base_op_check(external.function.name = "fun1") # commented because this example returns an error
#' }
#' @keywords internal
#' @rdname internal_function
.base_op_check <- function(
    external.function.name,
    external.package.name
){
    # AIM
    # Check if basic operator names have been used in the scope of the opened environement
    # WARNING
    # arguments of the .base_op_check() function are not checked, so use carefully inside other functions
    # ARGUMENTS
    # external.function.name: function name
    # external.package.name: package name
    # RETURN
    # An error message or nothing 
    # DEBUGGING
    # external.function.name = "test" ; external.package.name = "p1"
    # main code
    reserved.objects <- base::c(
        "-", 
        "!", 
        "!=", 
        "$", 
        "%%", 
        "%*%", 
        "%/%", 
        "%in%", 
        "&", 
        "&&", 
        "(", 
        "*", 
        "/", 
        ":", 
        "::", 
        ":::", 
        "@", 
        "[", 
        "[[", 
        "^", 
        "{", 
        "|", 
        "||", 
        "~", 
        "+", 
        "<", 
        "<-", 
        "<<-", 
        "<=", 
        "=", 
        "==", 
        ">", 
        ">=", 
        "\\", 
        "if", 
        "else", 
        "function",
        "for",
        "while",
        "repeat"
    )
    tempo.log <- base::sapply(X = reserved.objects, FUN = function(x){ 
        if( ! base::all(utils::find(x) == "package:base")){
            base::return(TRUE)
        }else{
            base::return(FALSE)
        }
    })
    if(base::any(tempo.log)){
        tempo.name <-  reserved.objects[tempo.log]
        tempo.pos <- base::sapply(X = tempo.name, FUN = function(x){base::paste(utils::find(x), collapse = " ")})
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            external.function.name, 
            " OF THE ", external.package.name, " PACKAGE\nCRITICAL R OBJECT",
            base::ifelse(base::length(tempo.log) == 1L, " ", "S "), 
            "CANNOT BE PRESENT SOMEWHERE ELSE IN THE R SCOPE THAN IN \"package::base\":\n", 
            base::paste(base::paste(tempo.name, tempo.pos, sep = "\t"), collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end main code
}