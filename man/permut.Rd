\name{permut}
\alias{permut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
permut(data1, data2 = NULL, n = NULL, seed = NULL, print.count = 10, text.print = "", cor.method = "spearman", cor.limit = 0.2, warn.print = FALSE, lib.path = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{
%%     ~~Describe \code{data1} here~~
}
  \item{data2}{
%%     ~~Describe \code{data2} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
  \item{print.count}{
%%     ~~Describe \code{print.count} here~~
}
  \item{text.print}{
%%     ~~Describe \code{text.print} here~~
}
  \item{cor.method}{
%%     ~~Describe \code{cor.method} here~~
}
  \item{cor.limit}{
%%     ~~Describe \code{cor.limit} here~~
}
  \item{warn.print}{
%%     ~~Describe \code{warn.print} here~~
}
  \item{lib.path}{
%%     ~~Describe \code{lib.path} here~~
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
function (data1, data2 = NULL, n = NULL, seed = NULL, print.count = 10, 
    text.print = "", cor.method = "spearman", cor.limit = 0.2, 
    warn.print = FALSE, lib.path = NULL) 
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
    .pack_and_function_check(fun = c("cuteDev::arg_check", "cuteTool::round", 
        "lubridate::seconds_to_period"), lib.path = NULL, external.function.name = function.name)
    mandat.args <- c("data1")
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
    tempo <- cuteDev::arg_check(data = data1, class = "vector", 
        fun.name = function.name)
    eval(ee)
    if (tempo$problem == FALSE & length(data1) < 2) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: data1 ARGUMENT MUST BE A VECTOR OF MINIMUM LENGTH 2. HERE IT IS: ", 
            length(data1))
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    if (!is.null(data2)) {
        tempo <- cuteDev::arg_check(data = data1, class = "vector", 
            mode = "numeric", fun.name = function.name)
        eval(ee)
        if (tempo$problem == TRUE) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: data1 MUST BE A NUMERIC VECTOR IF data2 ARGUMENT IS SPECIFIED")
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
        tempo <- cuteDev::arg_check(data = data2, class = "vector", 
            mode = "numeric", fun.name = function.name)
        eval(ee)
        if (length(data1) != length(data2)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: data1 AND data2 MUST BE VECTOR OF SAME LENGTH. HERE IT IS ", 
                length(data1), " AND ", length(data2))
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
    }
    else if (is.null(n)) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: n ARGUMENT CANNOT BE NULL IF data2 ARGUMENT IS NULL")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    if (!is.null(n)) {
        tempo <- cuteDev::arg_check(data = n, class = "vector", 
            typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
            neg.values = FALSE, fun.name = function.name)
        eval(ee)
    }
    if (!is.null(seed)) {
        tempo <- cuteDev::arg_check(data = seed, class = "vector", 
            typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
            neg.values = TRUE, fun.name = function.name)
        eval(ee)
    }
    tempo <- cuteDev::arg_check(data = print.count, class = "vector", 
        typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
        neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = text.print, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = cor.method, options = c("pearson", 
        "kendall", "spearman"), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = cor.limit, class = "vector", 
        mode = "numeric", prop = TRUE, length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = warn.print, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(lib.path)) {
        tempo <- cuteDev::arg_check(data = lib.path, class = "vector", 
            mode = "character", fun.name = function.name)
        eval(ee)
        if (tempo$problem == FALSE) {
            if (!all(dir.exists(lib.path), na.rm = TRUE)) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  " OF THE", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", 
                  paste(lib.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }
    if (!is.null(argum.check)) {
        if (any(argum.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], 
                collapse = "\n"), "\n\n================\n\n"), 
                call. = FALSE)
        }
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
    tempo.arg <- c("data1", "print.count", "text.print", "cor.method", 
        "cor.limit", "warn.print")
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
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    if (exists(".Random.seed", envir = .GlobalEnv)) {
        tempo.random.seed <- .Random.seed
        on.exit(assign(".Random.seed", tempo.random.seed, envir = .GlobalEnv))
    }
    else {
        on.exit(set.seed(NULL))
    }
    set.seed(seed)
    ini.date <- Sys.time()
    ini.time <- as.numeric(ini.date)
    ini.pos <- 1:length(data1)
    tempo.pos <- ini.pos
    pos.selec.seq.max <- length(ini.pos) - 1
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    count <- 0
    round <- 0
    BREAK <- FALSE
    tempo.cor <- 0
    if (is.null(data2)) {
        if (length(table(data1)) == 1L) {
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count, ") NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", 
                names(table(data1)))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                paste0(warn, "\n\n", tempo.warn)))
        }
        else {
            if (print.count > n) {
                print.count <- n
            }
            cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, 
                " | ")), "FOR LOOP OF ", n, " LOOPS INITIATED | LOOP COUNT: ", 
                format(count, big.mark = ",")))
            print.count.loop <- logical(length = print.count)
            print.count.loop[length(print.count.loop)] <- TRUE
            count.loop <- 0
            pos <- sample.int(n = pos.selec.seq.max, size = print.count, 
                replace = TRUE)
            tempo.date.loop <- Sys.time()
            tempo.time.loop <- as.numeric(tempo.date.loop)
            for (i3 in 1:n) {
                count.loop <- count.loop + 1
                pos2 <- pos[count.loop]
                tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, 
                  pos2 + 1)]
                if (print.count.loop[count.loop]) {
                  count.loop <- 0
                  pos <- sample.int(n = pos.selec.seq.max, size = print.count, 
                    replace = TRUE)
                  tempo.time <- as.numeric(Sys.time())
                  tempo.lapse <- cuteTool::round(lubridate::seconds_to_period(tempo.time - 
                    tempo.time.loop))
                  final.loop <- (tempo.time - tempo.time.loop)/i3 * 
                    n
                  final.exp <- as.POSIXct(final.loop, origin = tempo.date.loop)
                  cat(paste0("\n", ifelse(text.print == "", "", 
                    paste0(text.print, " | ")), "FOR LOOP ", 
                    i3, " / ", n, " | TIME SPENT: ", tempo.lapse, 
                    " | EXPECTED END: ", final.exp))
                }
            }
            count <- count + n
            cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, 
                " | ")), "FOR LOOP ENDED | LOOP COUNT: ", format(count, 
                big.mark = ",")))
            cat("\n\n")
        }
    }
    else {
        if (length(table(data1)) == 1L) {
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count, ") NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", 
                names(table(data1)))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                paste0(warn, "\n\n", tempo.warn)))
            tempo.cor <- 1
        }
        else if (length(table(data2)) == 1L) {
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count, ") NO PERMUTATION PERFORMED BECAUSE data2 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", 
                names(table(data2)))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                paste0(warn, "\n\n", tempo.warn)))
            tempo.cor <- 1
        }
        else {
            cor.ini <- cor(x = data1, y = data2, use = "pairwise.complete.obs", 
                method = cor.method)
            tempo.cor <- cor.ini
            neg.cor <- FALSE
            if (tempo.cor < 0) {
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count, ") INITIAL ", 
                  toupper(cor.method), " CORRELATION BETWEEN data1 AND data2 HAS BEEN DETECTED AS NEGATIVE: ", 
                  tempo.cor, ". THE LOOP STEPS WILL BE PERFORMED USING POSITIVE CORRELATIONS BUT THE FINAL CORRELATION WILL BE NEGATIVE")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                  paste0(warn, "\n\n", tempo.warn)))
                neg.cor <- TRUE
                tempo.cor <- abs(tempo.cor)
                cor.ini <- abs(cor.ini)
            }
            if (tempo.cor < cor.limit) {
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count, ") INITIAL ABSOLUTE VALUE OF THE ", 
                  toupper(cor.method), " CORRELATION ", cuteTool::round(tempo.cor), 
                  " BETWEEN data1 AND data2 HAS BEEN DETECTED AS BELOW THE CORRELATION LIMIT PARAMETER ", 
                  cor.limit, "\nTHE data1 SEQUENCE HAS BEEN COMPLETELY RANDOMIZED TO CORRESPOND TO CORRELATION ZERO")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                  paste0(warn, "\n\n", tempo.warn)))
                for (i4 in 1:5) {
                  tempo.pos <- sample(x = tempo.pos, size = length(tempo.pos), 
                    replace = FALSE)
                }
                count <- count + 5
            }
            else {
                count <- count + 1
                pos <- sample.int(n = pos.selec.seq.max, size = 1, 
                  replace = TRUE)
                tempo.pos[c(pos + 1, pos)] <- tempo.pos[c(pos, 
                  pos + 1)]
                tempo.cor <- abs(cor(x = data1[tempo.pos], y = data2, 
                  use = "pairwise.complete.obs", method = cor.method))
                smallest.cor.dec <- cor.ini - tempo.cor
                cat(paste0("\n", ifelse(text.print == "", "", 
                  paste0(text.print, " | ")), "CORRELATION DECREASE AFTER A SINGLE PERMUTATION: ", 
                  cuteTool::round(smallest.cor.dec, 4)))
                cat(paste0("\n", ifelse(text.print == "", "", 
                  paste0(text.print, " | ")), "FIRST WHILE LOOP STEP -> GOING OUT FROM EQUALITY | LOOP COUNT: ", 
                  format(count, big.mark = ","), " | CORRELATION LIMIT: ", 
                  cuteTool::round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", 
                  cuteTool::round(tempo.cor, 4)))
                print.count.loop <- logical(length = print.count)
                print.count.loop[length(print.count.loop)] <- TRUE
                count.loop <- 0
                pos <- sample.int(n = pos.selec.seq.max, size = print.count, 
                  replace = TRUE)
                tempo.date.loop <- Sys.time()
                tempo.time.loop <- as.numeric(tempo.date.loop)
                while (tempo.cor == cor.ini) {
                  count <- count + 1
                  count.loop <- count.loop + 1
                  pos2 <- pos[count.loop]
                  tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, 
                    pos2 + 1)]
                  tempo.cor <- abs(cor(x = data1[tempo.pos], 
                    y = data2, use = "pairwise.complete.obs", 
                    method = cor.method))
                  if (print.count.loop[count.loop]) {
                    count.loop <- 0
                    pos <- sample.int(n = pos.selec.seq.max, 
                      size = print.count, replace = TRUE)
                    tempo.time <- as.numeric(Sys.time())
                    tempo.lapse <- cuteTool::round(lubridate::seconds_to_period(tempo.time - 
                      tempo.time.loop))
                    cat(paste0("\n", ifelse(text.print == "", 
                      "", paste0(text.print, " | ")), "FIRST WHILE LOOP STEP", 
                      format(count.loop, big.mark = ","), " / ? | COUNT: ", 
                      format(count, big.mark = ","), " | CORRELATION LIMIT: ", 
                      cuteTool::round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", 
                      cuteTool::round(tempo.cor, 4), " | TIME SPENT: ", 
                      tempo.lapse))
                  }
                }
                tempo.time <- as.numeric(Sys.time())
                tempo.lapse <- cuteTool::round(lubridate::seconds_to_period(tempo.time - 
                  ini.time))
                cat(paste0("\n", ifelse(text.print == "", "", 
                  paste0(text.print, " | ")), "FIRST WHILE LOOP STEP END | LOOP COUNT: ", 
                  format(count, big.mark = ","), " | CORRELATION LIMIT: ", 
                  cuteTool::round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", 
                  cuteTool::round(tempo.cor, 4), " | TOTAL SPENT TIME: ", 
                  tempo.lapse))
                if (tempo.cor < cor.limit) {
                  warn.count <- warn.count + 1
                  tempo.warn <- paste0("(", warn.count, ") THE FIRST FOR & WHILE LOOP STEPS HAVE BEEN TOO FAR AND SUBSEQUENT LOOP STEPS WILL NOT RUN")
                  warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                    paste0(warn, "\n\n", tempo.warn)))
                }
                cat(paste0("\n", ifelse(text.print == "", "", 
                  paste0(text.print, " | ")), "WHILE/FOR LOOPS INITIATION | LOOP COUNT: ", 
                  format(count, big.mark = ","), " | CORRELATION LIMIT: ", 
                  cuteTool::round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", 
                  cuteTool::round(tempo.cor, 4)))
                count.est <- 1e+05
                first.round <- TRUE
                GOBACK <- FALSE
                while (tempo.cor > cor.limit) {
                  round <- round + 1
                  if (first.round == TRUE) {
                    first.round <- FALSE
                    cor.dec.per.loop <- numeric(length = 5)
                    loop.nb.est <- Inf
                    cor.est.ini <- tempo.cor
                    cor.est <- numeric(length = 5)
                    for (i6 in 1:5) {
                      tempo.pos.est <- tempo.pos
                      pos <- sample.int(n = pos.selec.seq.max, 
                        size = count.est, replace = TRUE)
                      for (i7 in 1:count.est) {
                        pos2 <- pos[i7]
                        tempo.pos.est[c(pos2 + 1, pos2)] <- tempo.pos.est[c(pos2, 
                          pos2 + 1)]
                      }
                      tempo.cor.est <- abs(cor(x = data1[tempo.pos.est], 
                        y = data2, use = "pairwise.complete.obs", 
                        method = cor.method))
                      cor.est[i6] <- tempo.cor.est
                      tempo.cor.dec.per.loop <- (cor.est.ini - 
                        tempo.cor.est)/count.est
                      if (is.na(tempo.cor.dec.per.loop) | !is.finite(tempo.cor.dec.per.loop)) {
                        tempo.cat <- paste0("ERROR IN ", function.name, 
                          " OF THE", package.name, " PACKAGE: CODE INCONSISTENCY 2\ncor.est.ini: ", 
                          cor.est.ini, "\ntempo.cor.est: ", tempo.cor.est)
                        stop(paste0("\n\n================\n\n", 
                          tempo.cat, "\n\n================\n\n", 
                          ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                            ifelse(warn.count > 1, "S", ""), 
                            ":\n\n", warn))), call. = FALSE)
                      }
                      cor.dec.per.loop[i6] <- tempo.cor.dec.per.loop
                    }
                    cor.est <- cor.est[which.max(cor.dec.per.loop)]
                    cor.dec.per.loop <- max(cor.dec.per.loop, 
                      na.rm = TRUE)
                    loop.nb.est <- cuteTool::round((tempo.cor - 
                      cor.limit)/cor.dec.per.loop)
                  }
                  else {
                    if (GOBACK == TRUE) {
                      loop.nb.est <- cuteTool::round(loop.nb.est/2)
                    }
                    else {
                      cor.dec.per.loop <- (cor.ini - tempo.cor)/count
                      loop.nb.est <- cuteTool::round((tempo.cor - 
                        cor.limit)/cor.dec.per.loop)
                    }
                  }
                  if (is.na(loop.nb.est) | !is.finite(loop.nb.est)) {
                    tempo.cat <- paste0("ERROR IN ", function.name, 
                      " OF THE", package.name, " PACKAGE: CODE INCONSISTENCY 1\nloop.nb.est: ", 
                      loop.nb.est, "\ncor.ini: ", cor.ini, "\ntempo.cor: ", 
                      tempo.cor, "\ncor.limit: ", cor.limit, 
                      "\ncor.dec.per.loop: ", cor.dec.per.loop)
                    stop(paste0("\n\n================\n\n", tempo.cat, 
                      "\n\n================\n\n", ifelse(is.null(warn), 
                        "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                          1, "S", ""), ":\n\n", warn))), call. = FALSE)
                  }
                  else if (loop.nb.est > 10000) {
                    tempo.pos.secu <- tempo.pos
                    count.secu <- count
                    tempo.cor.secu <- tempo.cor
                    cat(paste0("\n", ifelse(text.print == "", 
                      "", paste0(text.print, " | ")), "INITIAL SETTINGS BEFORE ROUND: ", 
                      round, " | LOOP COUNT: ", format(count, 
                        big.mark = ","), " | GO BACK: ", GOBACK, 
                      " | LOOP NUMBER ESTIMATION: ", format(loop.nb.est, 
                        big.mark = ","), " | CORRELATION LIMIT: ", 
                      cuteTool::round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", 
                      cuteTool::round(tempo.cor, 4)))
                    print.count.loop <- logical(length = print.count)
                    print.count.loop[length(print.count.loop)] <- TRUE
                    count.loop <- 0
                    pos <- sample.int(n = pos.selec.seq.max, 
                      size = print.count, replace = TRUE)
                    tempo.date.loop <- Sys.time()
                    tempo.time.loop <- as.numeric(tempo.date.loop)
                    for (i6 in 1:loop.nb.est) {
                      count.loop <- count.loop + 1
                      pos2 <- pos[count.loop]
                      tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, 
                        pos2 + 1)]
                      if (print.count.loop[count.loop]) {
                        count.loop <- 0
                        pos <- sample.int(n = pos.selec.seq.max, 
                          size = print.count, replace = TRUE)
                        tempo.time <- as.numeric(Sys.time())
                        tempo.lapse <- cuteTool::round(lubridate::seconds_to_period(tempo.time - 
                          tempo.time.loop))
                        final.loop <- (tempo.time - tempo.time.loop)/i6 * 
                          loop.nb.est
                        final.exp <- as.POSIXct(final.loop, origin = tempo.date.loop)
                        cat(paste0("\n", ifelse(text.print == 
                          "", "", paste0(text.print, " | ")), 
                          "FOR LOOP | ROUND ", round, " | LOOP: ", 
                          format(i6, big.mark = ","), " / ", 
                          format(loop.nb.est, big.mark = ","), 
                          " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", 
                          final.exp))
                      }
                    }
                    count <- count + loop.nb.est
                    tempo.cor <- abs(cor(x = data1[tempo.pos], 
                      y = data2, use = "pairwise.complete.obs", 
                      method = cor.method))
                    if (tempo.cor > tempo.cor.secu | ((tempo.cor - 
                      cor.limit) < 0 & abs(tempo.cor - cor.limit) > 
                      smallest.cor.dec * cuteTool::round(log10(max(ini.pos, 
                        na.rm = TRUE))))) {
                      GOBACK <- TRUE
                      tempo.pos <- tempo.pos.secu
                      count <- count.secu
                      tempo.cor <- tempo.cor.secu
                    }
                    else {
                      GOBACK <- FALSE
                    }
                  }
                  else {
                    cat(paste0("\n", ifelse(text.print == "", 
                      "", paste0(text.print, " | ")), "FINAL WHILE LOOP | LOOP COUNT: ", 
                      format(count, big.mark = ","), " | CORRELATION LIMIT: ", 
                      cuteTool::round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", 
                      cuteTool::round(tempo.cor, 4)))
                    print.count.loop <- logical(length = print.count)
                    print.count.loop[length(print.count.loop)] <- TRUE
                    count.loop <- 0
                    pos <- sample.int(n = pos.selec.seq.max, 
                      size = print.count, replace = TRUE)
                    tempo.cor.loop <- tempo.cor
                    tempo.date.loop <- Sys.time()
                    tempo.time.loop <- as.numeric(tempo.date.loop)
                    while (tempo.cor > cor.limit) {
                      count <- count + 1
                      count.loop <- count.loop + 1
                      pos2 <- pos[count.loop]
                      tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, 
                        pos2 + 1)]
                      tempo.cor <- abs(cor(x = data1[tempo.pos], 
                        y = data2, use = "pairwise.complete.obs", 
                        method = cor.method))
                      if (print.count.loop[count.loop]) {
                        count.loop <- 0
                        pos <- sample.int(n = pos.selec.seq.max, 
                          size = print.count, replace = TRUE)
                        tempo.time <- as.numeric(Sys.time())
                        tempo.lapse <- cuteTool::round(lubridate::seconds_to_period(tempo.time - 
                          tempo.time.loop))
                        final.loop <- (tempo.time - tempo.time.loop)/(tempo.cor.loop - 
                          tempo.cor) * (tempo.cor - cor.limit)
                        final.exp <- as.POSIXct(final.loop, origin = tempo.date.loop)
                        cat(paste0("\n", ifelse(text.print == 
                          "", "", paste0(text.print, " | ")), 
                          "WHILE LOOP | LOOP NB: ", format(count.loop, 
                            big.mark = ","), " | COUNT: ", format(count, 
                            big.mark = ","), " | CORRELATION LIMIT: ", 
                          cuteTool::round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", 
                          cuteTool::round(tempo.cor, 4), " | TIME SPENT: ", 
                          tempo.lapse, " | EXPECTED END: ", final.exp))
                      }
                    }
                  }
                }
                tempo.time <- as.numeric(Sys.time())
                tempo.lapse <- cuteTool::round(lubridate::seconds_to_period(tempo.time - 
                  ini.time))
                cat(paste0("\n", ifelse(text.print == "", "", 
                  paste0(text.print, " | ")), "WHILE/FOR LOOPS END | LOOP COUNT: ", 
                  format(count, big.mark = ","), " | NB OF ROUNDS: ", 
                  round, " | CORRELATION LIMIT: ", cuteTool::round(cor.limit, 
                    4), " | ABS TEMPO CORRELATION: ", cuteTool::round(tempo.cor, 
                    4), " | TOTAL SPENT TIME: ", tempo.lapse))
            }
            tempo.cor <- ifelse(neg.cor == TRUE, -tempo.cor, 
                tempo.cor)
        }
    }
    cat("\n\n")
    if (warn.print == TRUE & !is.null(warn)) {
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", 
            warn), call. = FALSE), add = TRUE)
    }
    on.exit(exp = options(warning.length = ini.warning.length), 
        add = TRUE)
    if (warn.print == TRUE & !is.null(warn)) {
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", 
            warn), call. = FALSE))
    }
    on.exit(exp = options(warning.length = ini.warning.length), 
        add = TRUE)
    output <- list(data = data1[tempo.pos], warn = warn, cor = if (is.null(data2)) {
        cor(ini.pos, tempo.pos, method = "spearman")
    } else {
        tempo.cor
    }, count = count)
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
