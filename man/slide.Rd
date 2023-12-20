\name{slide}
\alias{slide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
slide(data, window.size, step, from = NULL, to = NULL, fun, args = NULL, boundary = "left", parall = FALSE, thread.nb = NULL, print.count = 100, res.path = NULL, lib.path = NULL, verbose = TRUE, cute.path = "C:\\Users\\Gael\\Documents\\Git_projects\\cute_little_R_functions\\cute_little_R_functions.R")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{window.size}{
%%     ~~Describe \code{window.size} here~~
}
  \item{step}{
%%     ~~Describe \code{step} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
}
  \item{fun}{
%%     ~~Describe \code{fun} here~~
}
  \item{args}{
%%     ~~Describe \code{args} here~~
}
  \item{boundary}{
%%     ~~Describe \code{boundary} here~~
}
  \item{parall}{
%%     ~~Describe \code{parall} here~~
}
  \item{thread.nb}{
%%     ~~Describe \code{thread.nb} here~~
}
  \item{print.count}{
%%     ~~Describe \code{print.count} here~~
}
  \item{res.path}{
%%     ~~Describe \code{res.path} here~~
}
  \item{lib.path}{
%%     ~~Describe \code{lib.path} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{cute.path}{
%%     ~~Describe \code{cute.path} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, window.size, step, from = NULL, to = NULL, fun, 
    args = NULL, boundary = "left", parall = FALSE, thread.nb = NULL, 
    print.count = 100, res.path = NULL, lib.path = NULL, verbose = TRUE, 
    cute.path = "C:\\Users\\Gael\\Documents\\Git_projects\\cute_little_R_functions\\cute_little_R_functions.R") 
{
    package.name <- "cuteTool2"
    ini <- match.call(expand.dots = FALSE)
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    if (function.name[1] == "::()") {
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    if (!is.null(lib.path)) {
        if (!all(typeof(lib.path) == "character")) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", 
                paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else if (!all(dir.exists(lib.path), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", 
                paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else {
            .libPaths(new = sub(x = lib.path, pattern = "/$|\\\\$", 
                replacement = ""))
            lib.path <- .libPaths()
        }
    }
    else {
        lib.path <- .libPaths()
    }
    .pack_and_function_check(fun = c("cuteDev::arg_check", "cuteDev::get_message", 
        "parallel::detectCores", "parallel::makeCluster", "parallel::clusterSplit", 
        "parallel::clusterApply", "parallel::stopCluster", "lubridate::seconds_to_period"), 
        lib.path = lib.path, external.function.name = function.name)
    mandat.args <- c("data", "window.size", "step", "fun")
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, 
        collapse = "),missing("), "))")))
    if (any(tempo)) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: \nFOLLOWING ARGUMENT", ifelse(sum(tempo, 
                na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    argum.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(argum.check = c(argum.check, tempo$problem), 
        text.check = c(text.check, tempo$text), checked.arg.names = c(checked.arg.names, 
            tempo$object.name))
    tempo <- cuteDev::arg_check(data = data, mode = "numeric", 
        na.contain = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = window.size, class = "vector", 
        mode = "numeric", length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = step, class = "vector", 
        mode = "numeric", length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(from)) {
        tempo <- cuteDev::arg_check(data = from, class = "vector", 
            mode = "numeric", length = 1, fun.name = function.name)
        eval(ee)
    }
    if (!is.null(to)) {
        tempo <- cuteDev::arg_check(data = to, class = "vector", 
            mode = "numeric", length = 1, fun.name = function.name)
        eval(ee)
    }
    tempo1 <- cuteDev::arg_check(data = fun, class = "vector", 
        mode = "character", length = 1, fun.name = function.name)
    tempo2 <- cuteDev::arg_check(data = fun, class = "function", 
        length = 1, fun.name = function.name)
    if (tempo1$problem == TRUE & tempo2$problem == TRUE) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: fun ARGUMENT MUST BE A FUNCTION OR A CHARACTER STRING OF THE NAME OF A FUNCTION")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    if (!is.null(args)) {
        tempo <- cuteDev::arg_check(data = args, class = "vector", 
            mode = "character", length = 1, fun.name = function.name)
        eval(ee)
    }
    tempo <- cuteDev::arg_check(data = boundary, options = c("left", 
        "right"), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = parall, class = "vector", 
        mode = "logical", length = 1, fun.name = function.name)
    eval(ee)
    if (parall == TRUE) {
        if (!is.null(thread.nb)) {
            tempo <- cuteDev::arg_check(data = thread.nb, typeof = "integer", 
                double.as.integer.allowed = TRUE, neg.values = FALSE, 
                length = 1, fun.name = function.name)
            eval(ee)
            if (tempo$problem == FALSE & thread.nb < 1) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  " OF THE", package.name, " PACKAGE: thread.nb PARAMETER MUST EQUAL OR GREATER THAN 1: ", 
                  thread.nb)
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }
    tempo <- cuteDev::arg_check(data = print.count, class = "vector", 
        typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
        neg.values = FALSE, fun.name = function.name)
    eval(ee)
    if (!is.null(res.path)) {
        tempo <- cuteDev::arg_check(data = res.path, class = "vector", 
            mode = "character", fun.name = function.name)
        eval(ee)
    }
    if (!is.null(lib.path)) {
        tempo <- cuteDev::arg_check(data = lib.path, class = "vector", 
            mode = "character", fun.name = function.name)
        eval(ee)
    }
    tempo <- cuteDev::arg_check(data = verbose, class = "vector", 
        mode = "logical", length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = cute.path, class = "vector", 
        typeof = "character", length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(argum.check)) {
        if (any(argum.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], 
                collapse = "\n"), "\n\n================\n\n"), 
                call. = FALSE)
        }
    }
    env.name <- paste0("env", as.numeric(Sys.time()))
    if (exists(env.name, where = -1)) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    else {
        assign(env.name, new.env())
    }
    if (!(all(class(arg.user.setting) == "list", na.rm = TRUE) & 
        length(arg.user.setting) == 0)) {
        tempo.arg <- names(arg.user.setting)
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = length) == 1L
        if (any(tempo.log) == TRUE) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: \n", ifelse(sum(tempo.log, 
                  na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), 
                " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], 
                  collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    tempo.arg <- c("data", "window.size", "step", "fun", "boundary", 
        "print.count", "verbose", "cute.path")
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), 
        inherit = FALSE), FUN = is.null)
    if (any(tempo.log) == TRUE) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE:\n", ifelse(sum(tempo.log, 
                na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), 
            paste0(tempo.arg[tempo.log], collapse = "\n"), "\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (length(data) == 0) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: data ARGUMENT CANNOT BE LENGTH 0")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (any(!is.finite(data), na.rm = TRUE)) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: data ARGUMENT CANNOT CONTAIN Inf VALUES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (step > window.size) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: step ARGUMENT MUST BE LOWER THAN window.size ARGUMENT\nstep: ", 
            paste(step, collapse = " "), "\nwindow.size: ", paste(window.size, 
                collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!is.null(res.path)) {
        if (!all(dir.exists(res.path), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE res.path ARGUMENT DOES NOT EXISTS:\n", 
                paste(res.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    else {
        res.path <- getwd()
    }
    if (!is.null(lib.path)) {
        if (!all(dir.exists(lib.path), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", 
                paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (grepl(x = cute.path, pattern = "^http")) {
        tempo.error1 <- any(grepl(x = cuteDev::get_message(data = "source(cute.path)", 
            kind = "error", header = FALSE, env = get(env.name, 
                envir = sys.nframe(), inherits = FALSE)), pattern = "^[Ee]rror"), 
            na.rm = TRUE)
        tempo.error2 <- FALSE
    }
    else {
        tempo.error1 <- FALSE
        tempo.error2 <- !file.exists(cute.path)
    }
    if (tempo.error1 | tempo.error2) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: ", ifelse(grepl(x = cute.path, 
                pattern = "^http"), "URL", "FILE"), " PATH INDICATED IN THE cute.path PARAMETER DOES NOT EXISTS:\n", 
            cute.path)
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    if (verbose == TRUE) {
        cat("\nslide JOB IGNITION\n")
    }
    ini.date <- Sys.time()
    ini.time <- as.numeric(ini.date)
    fun <- match.fun(fun)
    if (boundary == "left") {
        left <- ">="
        right <- "<"
        right.last.wind <- ">"
    }
    else if (boundary == "right") {
        left <- ">"
        right <- "<="
        right.last.wind <- ">="
    }
    else {
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, 
            " OF THE", package.name, " PACKAGE: \nCODE INCONSISTENCY 1")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    data <- as.vector(data)
    data <- sort(data, na.last = NA)
    wind <- data.frame(left = seq(from = if (is.null(from)) {
        min(data, na.rm = TRUE)
    }
    else {
        from
    }, to = if (is.null(to)) {
        max(data, na.rm = TRUE)
    }
    else {
        to
    }, by = step), stringsAsFactors = TRUE)
    wind <- data.frame(wind, right = wind$left + window.size, 
        stringsAsFactors = TRUE)
    wind <- data.frame(wind, center = (wind$left + wind$right)/2, 
        stringsAsFactors = TRUE)
    if (all(wind$right < if (is.null(to)) {
        max(data, na.rm = TRUE)
    } else {
        to
    })) {
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, 
            " OF THE", package.name, " PACKAGE: \nCODE INCONSISTENCY 2")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (sum(get(right.last.wind)(wind$right, if (is.null(to)) {
        max(data, na.rm = TRUE)
    } else {
        to
    }), na.rm = TRUE) > 1) {
        tempo.log <- get(right.last.wind)(wind$right, if (is.null(to)) {
            max(data, na.rm = TRUE)
        }
        else {
            to
        })
        tempo.log[min(which(tempo.log), na.rm = TRUE)] <- FALSE
        wind <- wind[!tempo.log, ]
    }
    if (parall == FALSE) {
        assign("wind", wind, envir = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
        assign("data", data, envir = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
        tempo.message <- cuteDev::get_message(data = "lapply(X = wind$left, Y = data, FUN = function(X, Y){res <- get(left)(Y, X) ; return(res)})", 
            kind = "error", header = FALSE, env = get(env.name, 
                envir = sys.nframe(), inherits = FALSE), print.no = FALSE)
    }
    else {
        tempo.message <- "ERROR"
    }
    if (!any(grepl(x = tempo.message, pattern = "ERROR.*"), na.rm = TRUE)) {
        left.log <- lapply(X = wind$left, Y = data, FUN = function(X, 
            Y) {
            res <- get(left)(Y, X)
            return(res)
        })
        right.log <- lapply(X = wind$right, Y = data, FUN = function(X, 
            Y) {
            res <- get(right)(Y, X)
            return(res)
        })
        log <- mapply(FUN = "&", left.log, right.log, SIMPLIFY = FALSE)
        output <- eval(parse(text = paste0("sapply(lapply(log, FUN = function(X){(data[X])}), FUN = fun", 
            if (!is.null(args)) {
                paste0(", ", args)
            }, ")")))
        if (length(output) != nrow(wind)) {
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, 
                " OF THE", package.name, " PACKAGE: \nCODE INCONSISTENCY 3")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else {
            output <- data.frame(wind, value = output, stringsAsFactors = TRUE)
        }
    }
    else {
        if (verbose == TRUE) {
            tempo.cat <- paste0("PARALLELIZATION INITIATED AT: ", 
                ini.date)
            cat(paste0("\n", tempo.cat, "\n"))
        }
        tempo.thread.nb = parallel::detectCores(all.tests = FALSE, 
            logical = TRUE)
        if (!is.null(thread.nb)) {
            if (tempo.thread.nb < thread.nb) {
                thread.nb <- tempo.thread.nb
                if (verbose == TRUE) {
                  tempo.cat <- paste0("ONLY: ", tempo.thread.nb, 
                    " THREADS AVAILABLE")
                  cat(paste0("\n", tempo.cat, "\n"))
                }
            }
        }
        else {
            thread.nb <- tempo.thread.nb
        }
        if (verbose == TRUE) {
            tempo.cat <- paste0("NUMBER OF THREADS USED: ", thread.nb)
            cat(paste0("\n    ", tempo.cat, "\n"))
        }
        Clust <- parallel::makeCluster(thread.nb, outfile = paste0(res.path, 
            "/fun_slide_parall_log.txt"))
        cluster.list <- parallel::clusterSplit(Clust, 1:nrow(wind))
        if (verbose == TRUE) {
            tempo.cat <- paste0("SPLIT OF TEST NUMBERS IN PARALLELISATION:")
            cat(paste0("\n    ", tempo.cat, "\n"))
            str(cluster.list)
            cat("\n")
        }
        paral.output.list <- parallel::clusterApply(cl = Clust, 
            x = cluster.list, function.name = function.name, 
            data = data, FUN = fun, args = args, thread.nb = thread.nb, 
            print.count = print.count, wind = wind, left = left, 
            right = right, res.path = res.path, lib.path = lib.path, 
            verbose = verbose, cute.path = cute.path, fun = function(x, 
                function.name, data, FUN, args, thread.nb, print.count, 
                wind, left, right, res.path, lib.path, verbose, 
                cute.path) {
                process.id <- Sys.getpid()
                cat(paste0("\nPROCESS ID ", process.id, " -> TESTS ", 
                  x[1], " TO ", x[length(x)], "\n"))
                source(cute.path, local = .GlobalEnv)
                fun_pack(req.package = "lubridate", lib.path = lib.path, 
                  load = TRUE)
                ini.date <- Sys.time()
                ini.time <- as.numeric(ini.date)
                output <- NULL
                print.count.loop <- 0
                for (i4 in 1:length(x)) {
                  print.count.loop <- print.count.loop + 1
                  log <- get(left)(data, wind$left[x[i4]]) & 
                    get(right)(data, wind$right[x[i4]])
                  output <- c(output, eval(parse(text = paste0("FUN(data[log]", 
                    if (!is.null(args)) {
                      paste0(", ", args)
                    }, ")"))))
                  if (verbose == TRUE) {
                    if (print.count.loop == print.count) {
                      print.count.loop <- 0
                      tempo.time <- as.numeric(Sys.time())
                      tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - 
                        ini.time))
                      final.loop <- (tempo.time - ini.time)/i4 * 
                        length(x)
                      final.exp <- as.POSIXct(final.loop, origin = ini.date)
                      cat(paste0("\nIN PROCESS ", process.id, 
                        " | LOOP ", format(i4, big.mark = ","), 
                        " / ", format(length(x), big.mark = ","), 
                        " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", 
                        final.exp))
                    }
                    if (i4 == length(x)) {
                      tempo.time <- as.numeric(Sys.time())
                      tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - 
                        ini.time))
                      cat(paste0("\nPROCESS ", process.id, " ENDED | LOOP ", 
                        format(i4, big.mark = ","), " / ", format(length(x), 
                          big.mark = ","), " | TIME SPENT: ", 
                        tempo.lapse, "\n\n"))
                    }
                  }
                }
                wind <- wind[x, ]
                if (length(output) != nrow(wind)) {
                  tempo.cat <- paste0("INTERNAL CODE ERROR IN ", 
                    function.name, " OF THE", package.name, " PACKAGE: \nCODE INCONSISTENCY 4")
                  stop(paste0("\n\n================\n\n", tempo.cat, 
                    "\n\n================\n\n"), call. = FALSE)
                }
                else {
                  output <- data.frame(wind, value = output, 
                    stringsAsFactors = TRUE)
                  return(output)
                }
            })
        parallel::stopCluster(Clust)
        output <- data.frame()
        for (i2 in 1:length(paral.output.list)) {
            output <- rbind(output, paral.output.list[[i2]], 
                stringsAsFactors = TRUE)
        }
        if (nrow(output) != nrow(wind)) {
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, 
                " OF THE", package.name, " PACKAGE: \nCODE INCONSISTENCY 5\nlength(output): ", 
                length(output), "\nnrow(wind): ", nrow(wind))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else {
            output <- output[order(output$left), ]
        }
    }
    if (verbose == TRUE) {
        end.date <- Sys.time()
        end.time <- as.numeric(end.date)
        total.lapse <- round(lubridate::seconds_to_period(end.time - 
            ini.time))
        cat(paste0("\nslide JOB END\n\nTIME: ", end.date, "\n\nTOTAL TIME LAPSE: ", 
            total.lapse, "\n\n\n"))
    }
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
