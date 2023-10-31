#' @title .cuteDev_package_check
#' @description
#' Check if the cuteDev packages is present in the computer.
#' @param req.package Character vector of package names to import.
#' @param lib.path Optional character vector specifying the absolute pathways of the directories containing some of the listed packages in the req.package argument, if not in the default directories. Ignored if NULL.
#' @returns Nothing.
#' @importFrom utils installed.packages
#' @keywords internal
#' @rdname internal_function


.cuteDev_package_check <- function(
        req.package = "cuteDev",  
        lib.path = NULL
){
    if(is.null(lib.path)){
        lib.path <- .libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }else{
        .libPaths(new = sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
        lib.path <- .libPaths()
    }
    if( ! req.package %in% rownames(utils::installed.packages(lib.loc = lib.path))){
        tempo.cat <- paste0(
            "ERROR IN ONE OF THE FUNCTION OF THE cuteTool2 PACKAGE\nPACKAGE cuteDev MUST BE INSTALLED IN", 
            ifelse(length(lib.path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            paste(lib.path, collapse = "\n")
        )
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
}