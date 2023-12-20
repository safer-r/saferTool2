\name{codon2aa}
\alias{codon2aa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
codon2aa(data, display = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{display}{
%%     ~~Describe \code{display} here~~
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
function (data, display = FALSE) 
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
    mandat.args <- c("data")
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
    arg.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(arg.check = c(arg.check, tempo$problem), 
        text.check = c(text.check, tempo$text), checked.arg.names = c(checked.arg.names, 
            tempo$object.name))
    tempo <- cuteDev::arg_check(data = data, class = "vector", 
        typeof = "character", fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = display, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(arg.check)) {
        if (any(arg.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], 
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
    tempo.arg <- c("data", "display")
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
    if (length(data) == 1L) {
        data <- unlist(strsplit(data, split = ""))
    }
    else if (length(data) != 3L) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: data ARGUMENT MUST BE A STRING OF THREE CHARACTERS OR A VECTOR OF THREE CHARACTERS, MADE OF \"A\", \"C\", \"G\", \"T\" ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!all(toupper(data) \%in\% c("A", "C", "G", "T"))) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: data ARGUMENT MUST BE A STRING OF THREE CHARACTERS OR A VECTOR OF THREE CHARACTERS, MADE OF \"A\", \"C\", \"G\", \"T\" ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    sgc <- array(c("F", "L", "I", "V", "S", "P", "T", "A", "Y", 
        "H", "N", "D", "C", "R", "S", "G", "F", "L", "I", "V", 
        "S", "P", "T", "A", "Y", "H", "N", "D", "C", "R", "S", 
        "G", "L", "L", "I", "V", "S", "P", "T", "A", "stop", 
        "Q", "K", "E", "stop", "R", "R", "G", "L", "L", "M", 
        "V", "S", "P", "T", "A", "stop", "Q", "K", "E", "W", 
        "R", "R", "G"), dim = c(4, 4, 4), dimnames = list(first = c("T", 
        "C", "A", "G"), second = c("T", "C", "A", "G"), third = c("T", 
        "C", "A", "G")))
    if (display == TRUE) {
        output <- sgc
    }
    else {
        data <- toupper(data)
        output <- eval(parse(text = paste0("sgc['", paste0(data, 
            collapse = "','"), "']")))
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
