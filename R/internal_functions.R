#' @title .pack_and_function_check
#' @description
#' Check if 1) required functions are present in required packages and 2) required packages are installed locally.
#' @param fun Character vector of the names of the required functions, preceded by the name of the package they belong to and a double colon. Example: c("ggplot2::geom_point", "grid::gpar").
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the listed packages in the fun argument, if not in the default directories. If NULL, the function checks only in the .libPaths() default R library folders.
#' @param external.function.name Name of the function using the .pack_and_function_check() function.
#' @returns An error message if at least one of the checked packages is missing in lib.path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
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
        external.function.name
){
    # WARNING
    # arguments of the .pack_and_function_check() function are not checked, so use carefully inside other functions
    # DEBUGGING
    # fun = "ggplot2::geom_point" ; lib.path = "C:/Program Files/R/R-4.3.1/library" ; external.function.name = "fun1"
    # check of lib.path
    # already done in the main function
    # end check of lib.path
    # main code
    tempo.log <- grepl(x = fun, pattern = "^.+::.+$")
    if( ! all(tempo.log)){
        tempo.cat <- paste0("ERROR IN THE CODE OF THE ", external.function.name, " OF THE saferTool2 PACKAGE.\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\":\n", paste(fun[ ! tempo.log], collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    pkg.fun.name.list <- base::strsplit(fun, "::") # package in 1 and function in 2
    pkg.name <- sapply(X = pkg.fun.name.list, FUN = function(x){x[1]})
    pkg.log <- pkg.name %in% rownames(utils::installed.packages(lib.loc = lib.path))
    if( ! all(pkg.log)){
        tempo <- pkg.name[ ! pkg.log]
        tempo.cat <- paste0(
            "ERROR IN ", 
            external.function.name, 
            " OF THE saferTool2 PACKAGE. REQUIRED PACKAGE", 
            ifelse(length(tempo) == 1L, paste0(":\n", tempo), paste0("S:\n", paste(tempo, collapse = "\n"))), 
            "\nMUST BE INSTALLED IN", 
            ifelse(length(lib.path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            paste(lib.path, collapse = "\n")
        )
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    fun.log <- sapply(X = pkg.fun.name.list, FUN = function(x){base::exists(x[2], envir = base::asNamespace(x[1]))})
    if( ! all(fun.log)){
        tempo <- fun[ ! fun.log]
        tempo.cat <- paste0(
            "ERROR IN ", 
            external.function.name, 
            " OF THE saferTool2 PACKAGE. REQUIRED FUNCTION",
            ifelse(length(tempo) == 1L, " IS ", "S ARE "), 
            "MISSING IN THE INSTALLED PACKAGE", 
            ifelse(length(tempo) == 1L, paste0(":\n", tempo), paste0("S:\n", paste(tempo, collapse = "\n")))
        )
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
}
