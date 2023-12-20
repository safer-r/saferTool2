\name{codon_finder}
\alias{codon_finder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
codon_finder(pos, begin, end)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos}{
%%     ~~Describe \code{pos} here~~
}
  \item{begin}{
%%     ~~Describe \code{begin} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
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
function (pos, begin, end) 
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
    .pack_and_function_check(fun = c("cuteDev::arg_check"), lib.path = NULL, 
        external.function.name = function.name)
    mandat.args <- c("pos", "begin", "end")
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
    tempo <- cuteDev::arg_check(data = pos, class = "vector", 
        typeof = "integer", double.as.integer.allowed = TRUE, 
        fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = begin, class = "vector", 
        typeof = "integer", double.as.integer.allowed = TRUE, 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = end, class = "vector", 
        typeof = "integer", double.as.integer.allowed = TRUE, 
        length = 1, fun.name = function.name)
    eval(ee)
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
                package.name, " PACKAGE:\n", ifelse(sum(tempo.log, 
                  na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), 
                " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], 
                  collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    tempo.arg <- c("pos", "begin", "end")
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
    if (begin >= end) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: end ARGUMENT MUST BE STRICTLY GREATER THAN begin ARGUMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if ((end - begin + 1)\%\%3 != 0L) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: ((end - begin) + 1) / 3 MUST BE AN INTEGER (I.E., MODULO ZERO)")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (any(pos < begin | pos > end, na.rm = TRUE)) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: pos ARGUMENT VALUES MUST BE BETWEEN begin AND end ARGUMENT VALUES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    first <- seq.int(from = begin, to = end, by = 3)
    last <- seq.int(from = begin + 2, to = end, by = 3)
    tempo <- lapply(X = pos, FUN = function(x) {
        tempo.log <- x >= first & x <= last
        if (sum(tempo.log, na.rm = TRUE) != 1) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: INTERNAL ERROR. CODE HAS TO BE MODIFIED")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else {
            codon_nb <- which(tempo.log)
            codon_pos <- as.integer((x - (begin + (codon_nb - 
                1) * 3) + 1))
            codon_begin <- as.integer(first[tempo.log])
            codon_end <- as.integer(last[tempo.log])
        }
        return(data.frame(codon_nb = codon_nb, codon_pos = codon_pos, 
            codon_begin = codon_begin, codon_end = codon_end))
    })
    tempo <- do.call("rbind", tempo)
    output <- data.frame(pos = as.integer(pos), tempo)
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
