\name{trim}
\alias{trim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
trim(data, displayed.nb = NULL, single.value.display = FALSE, trim.method = "", trim.cutoffs = c(0.05, 0.975), interval.scale.disp = TRUE, down.space = 0.75, left.space = 0.75, up.space = 0.3, right.space = 0.25, orient = 1, dist.legend = 0.37, box.type = "l", amplif.label = 1.25, amplif.axis = 1.25, std.x.range = TRUE, std.y.range = TRUE, cex.pt = 0.2, col.box = hsv(0.55, 0.8, 0.8), x.nb.inter.tick = 4, y.nb.inter.tick = 0, tick.length = 1, sec.tick.length = 0.75, corner.text = "", amplif.legend = 1, corner.text.size = 0.75, trim.return = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{displayed.nb}{
%%     ~~Describe \code{displayed.nb} here~~
}
  \item{single.value.display}{
%%     ~~Describe \code{single.value.display} here~~
}
  \item{trim.method}{
%%     ~~Describe \code{trim.method} here~~
}
  \item{trim.cutoffs}{
%%     ~~Describe \code{trim.cutoffs} here~~
}
  \item{interval.scale.disp}{
%%     ~~Describe \code{interval.scale.disp} here~~
}
  \item{down.space}{
%%     ~~Describe \code{down.space} here~~
}
  \item{left.space}{
%%     ~~Describe \code{left.space} here~~
}
  \item{up.space}{
%%     ~~Describe \code{up.space} here~~
}
  \item{right.space}{
%%     ~~Describe \code{right.space} here~~
}
  \item{orient}{
%%     ~~Describe \code{orient} here~~
}
  \item{dist.legend}{
%%     ~~Describe \code{dist.legend} here~~
}
  \item{box.type}{
%%     ~~Describe \code{box.type} here~~
}
  \item{amplif.label}{
%%     ~~Describe \code{amplif.label} here~~
}
  \item{amplif.axis}{
%%     ~~Describe \code{amplif.axis} here~~
}
  \item{std.x.range}{
%%     ~~Describe \code{std.x.range} here~~
}
  \item{std.y.range}{
%%     ~~Describe \code{std.y.range} here~~
}
  \item{cex.pt}{
%%     ~~Describe \code{cex.pt} here~~
}
  \item{col.box}{
%%     ~~Describe \code{col.box} here~~
}
  \item{x.nb.inter.tick}{
%%     ~~Describe \code{x.nb.inter.tick} here~~
}
  \item{y.nb.inter.tick}{
%%     ~~Describe \code{y.nb.inter.tick} here~~
}
  \item{tick.length}{
%%     ~~Describe \code{tick.length} here~~
}
  \item{sec.tick.length}{
%%     ~~Describe \code{sec.tick.length} here~~
}
  \item{corner.text}{
%%     ~~Describe \code{corner.text} here~~
}
  \item{amplif.legend}{
%%     ~~Describe \code{amplif.legend} here~~
}
  \item{corner.text.size}{
%%     ~~Describe \code{corner.text.size} here~~
}
  \item{trim.return}{
%%     ~~Describe \code{trim.return} here~~
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
function (data, displayed.nb = NULL, single.value.display = FALSE, 
    trim.method = "", trim.cutoffs = c(0.05, 0.975), interval.scale.disp = TRUE, 
    down.space = 0.75, left.space = 0.75, up.space = 0.3, right.space = 0.25, 
    orient = 1, dist.legend = 0.37, box.type = "l", amplif.label = 1.25, 
    amplif.axis = 1.25, std.x.range = TRUE, std.y.range = TRUE, 
    cex.pt = 0.2, col.box = hsv(0.55, 0.8, 0.8), x.nb.inter.tick = 4, 
    y.nb.inter.tick = 0, tick.length = 1, sec.tick.length = 0.75, 
    corner.text = "", amplif.legend = 1, corner.text.size = 0.75, 
    trim.return = FALSE) 
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
    argum.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(argum.check <- c(argum.check, tempo$problem), 
        text.check <- c(text.check, tempo$text), checked.arg.names <- c(checked.arg.names, 
            tempo$object.name))
    if (!is.null(displayed.nb)) {
        tempo <- cuteDev::arg_check(data = displayed.nb, class = "vector", 
            mode = "numeric", length = 1, fun.name = function.name)
        eval(ee)
        if (displayed.nb < 2) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: displayed.nb ARGUMENT MUST BE A SINGLE INTEGER VALUE GREATER THAN 1 AND NOT: ", 
                paste(displayed.nb, collapse = " "))
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
    }
    tempo <- cuteDev::arg_check(data = single.value.display, 
        class = "logical", length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = trim.method, options = c("", 
        "mean.sd", "quantile"), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = trim.cutoffs, class = "vector", 
        mode = "numeric", length = 2, prop = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = interval.scale.disp, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = down.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = left.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = up.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = right.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = orient, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = dist.legend, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = box.type, options = c("o", 
        "l", "7", "c", "u", "]", "n"), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = amplif.label, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = amplif.axis, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = std.x.range, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = std.y.range, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = cex.pt, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = col.box, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = x.nb.inter.tick, class = "integer", 
        length = 1, neg.values = FALSE, double.as.integer.allowed = TRUE, 
        fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = y.nb.inter.tick, class = "integer", 
        length = 1, neg.values = FALSE, double.as.integer.allowed = TRUE, 
        fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = tick.length, class = "vector", 
        mode = "numeric", length = 1, prop = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = sec.tick.length, class = "vector", 
        mode = "numeric", length = 1, prop = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = corner.text, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = amplif.legend, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = corner.text.size, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = trim.return, class = "logical", 
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
                package.name, " PACKAGE: \n", ifelse(sum(tempo.log, 
                  na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), 
                " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], 
                  collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    tempo.arg <- c("data", "single.value.display", "trim.method", 
        "trim.cutoffs", "interval.scale.disp", "down.space", 
        "left.space", "up.space", "right.space", "orient", "dist.legend", 
        "box.type", "amplif.label", "amplif.axis", "std.x.range", 
        "std.y.range", "cex.pt", "col.box", "x.nb.inter.tick", 
        "y.nb.inter.tick", "tick.length", "sec.tick.length", 
        "corner.text", "amplif.legend", "corner.text.size", "trim.return")
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
    if (all(!is.finite(data))) {
        tempo.cat <- paste0("ERROR IN ", function.name, " FUNCTION OF THE", 
            package.name, " PACKAGE: \ndata ARGUMENT CANNOT CONTAIN NA AND Inf ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!(all(class(data) == "numeric") | all(class(data) == 
        "integer") | (all(class(data) \%in\% c("matrix", "array")) & 
        base::mode(data) == "numeric"))) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: data ARGUMENT MUST BE A NUMERIC VECTOR OR NUMERIC MATRIX")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (all(class(data) \%in\% c("matrix", "array"))) {
        data <- as.vector(data)
    }
    na.nb <- NULL
    if (any(is.na(data))) {
        na.nb <- sum(c(is.na(data)))
        data <- data[!is.na(data)]
    }
    color.cut <- hsv(0.75, 1, 1)
    col.mean <- hsv(0.25, 1, 0.8)
    col.quantile <- "orange"
    quantiles.selection <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 
        0.75, 0.9, 0.95, 0.975, 0.99)
    if (single.value.display == FALSE & length(unique(data)) == 
        1L) {
        par(bty = "n", xaxt = "n", yaxt = "n", xpd = TRUE)
        plot(1, pch = 16, col = "white", xlab = "", ylab = "")
        text(x = 1, y = 1, paste0("No graphic displayed\nBecause data made of a single different value (", 
            formatC(as.double(table(data))), ")"), cex = 2)
        output <- list(trim.method = NULL, trim.cutoffs = NULL, 
            real.trim.cutoffs = NULL, trimmed.values = NULL, 
            kept.values = NULL)
    }
    else {
        output <- list(trim.method = trim.method, trim.cutoffs = trim.cutoffs, 
            real.trim.cutoffs = NULL, trimmed.values = NULL, 
            kept.values = NULL)
        fun.rug <- function(sec.tick.length.f = sec.tick.length, 
            x.nb.inter.tick.f = x.nb.inter.tick, y.nb.inter.tick.f = y.nb.inter.tick) {
            if (x.nb.inter.tick.f > 0) {
                inter.tick.unit <- (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3]
                par.ini <- par()[c("xpd", "tcl")]
                par(xpd = FALSE)
                par(tcl = -par()$mgp[2] * sec.tick.length.f)
                suppressWarnings(rug(seq(par("xaxp")[1] - 10 * 
                  inter.tick.unit, par("xaxp")[2] + 10 * inter.tick.unit, 
                  by = inter.tick.unit/(1 + x.nb.inter.tick.f)), 
                  ticksize = NA, side = 1))
                par(par.ini)
                rm(par.ini)
            }
            if (y.nb.inter.tick.f > 0) {
                inter.tick.unit <- (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3]
                par.ini <- par()[c("xpd", "tcl")]
                par(xpd = FALSE)
                par(tcl = -par()$mgp[2] * sec.tick.length.f)
                suppressWarnings(rug(seq(par("yaxp")[1] - 10 * 
                  inter.tick.unit, par("yaxp")[2] + 10 * inter.tick.unit, 
                  by = inter.tick.unit/(1 + y.nb.inter.tick.f)), 
                  ticksize = NA, side = 2))
                par(par.ini)
                rm(par.ini)
            }
        }
        fun.add.cut <- function(data.f, trim.method.f = trim.method, 
            trim.cutoffs.f = trim.cutoffs, color.cut.f = color.cut, 
            return.f = FALSE) {
            real.trim.cutoffs.f <- NULL
            if (trim.method.f != "") {
                data.f <- sort(data.f)
                par.ini <- par()$xpd
                par(xpd = FALSE)
                if (trim.method.f == "mean.sd") {
                  real.trim.cutoffs.f <- qnorm(trim.cutoffs.f, 
                    mean(data.f, na.rm = TRUE), sd(data.f, na.rm = TRUE))
                  abline(v = qnorm(trim.cutoffs.f, mean(data.f, 
                    na.rm = TRUE), sd(data.f, na.rm = TRUE)), 
                    col = color.cut.f)
                  segments(qnorm(trim.cutoffs.f[1], mean(data.f, 
                    na.rm = TRUE), sd(data.f, na.rm = TRUE)), 
                    par()$usr[4] * 0.75, qnorm(trim.cutoffs.f[2], 
                      mean(data.f, na.rm = TRUE), sd(data.f, 
                        na.rm = TRUE)), par()$usr[4] * 0.75, 
                    col = color.cut.f)
                }
                if (trim.method.f == "quantile") {
                  real.trim.cutoffs.f <- quantile(data.f, probs = trim.cutoffs.f, 
                    type = 7, na.rm = TRUE)
                  abline(v = quantile(data.f, probs = trim.cutoffs.f, 
                    type = 7, na.rm = TRUE), col = color.cut.f)
                  segments(quantile(data.f, probs = trim.cutoffs.f[1], 
                    type = 7, na.rm = TRUE), par()$usr[4] * 0.75, 
                    quantile(data.f, probs = trim.cutoffs.f[2], 
                      type = 7, na.rm = TRUE), par()$usr[4] * 
                      0.75, col = color.cut.f)
                }
                par(par.ini)
                if (return.f == TRUE) {
                  trimmed.values.f <- data.f[data.f <= real.trim.cutoffs.f[1] | 
                    data.f >= real.trim.cutoffs.f[2]]
                  kept.values.f <- data.f[data.f > real.trim.cutoffs.f[1] & 
                    data.f < real.trim.cutoffs.f[2]]
                }
            }
            else {
                real.trim.cutoffs.f <- NULL
                trimmed.values.f <- NULL
                kept.values.f <- NULL
            }
            if (return.f == TRUE) {
                output <- list(trim.method = trim.method.f, trim.cutoffs = trim.cutoffs.f, 
                  real.trim.cutoffs = real.trim.cutoffs.f, trimmed.values = trimmed.values.f, 
                  kept.values = kept.values.f)
                return(output)
            }
        }
        fun.interval.scale.display <- function(data.f, col.quantile.f = col.quantile, 
            quantiles.selection.f = quantiles.selection, col.mean.f = col.mean) {
            par.ini <- par()[c("mgp", "xpd")]
            par(mgp = c(0.25, 0.25, 0), xpd = NA)
            axis(side = 3, at = c(par()$usr[1], par()$usr[2]), 
                labels = rep("", 2), col = col.quantile.f, lwd.ticks = 0)
            par(xpd = FALSE)
            axis(side = 3, at = quantile(as.vector(data.f), probs = quantiles.selection.f, 
                type = 7, na.rm = TRUE), labels = quantiles.selection.f, 
                col.axis = col.quantile.f, col = col.quantile.f)
            par(mgp = c(1.75, 1.75, 1.5), xpd = NA)
            axis(side = 3, at = c(par()$usr[1], par()$usr[2]), 
                labels = rep("", 2), col = col.mean.f, lwd.ticks = 0)
            par(xpd = FALSE)
            axis(side = 3, at = m + s * qnorm(quantiles.selection.f), 
                labels = formatC(round(qnorm(quantiles.selection.f), 
                  2)), col.axis = col.mean.f, col = col.mean.f, 
                lwd.ticks = 1)
            par(par.ini)
        }
        zone <- matrix(1:4, ncol = 2)
        layout(zone)
        par(omi = c(0, 0, 1.5, 0), mai = c(down.space, left.space, 
            up.space, right.space), las = orient, mgp = c(dist.legend/0.2, 
            0.5, 0), xpd = FALSE, bty = box.type, cex.lab = amplif.label, 
            cex.axis = amplif.axis, xaxs = ifelse(std.x.range, 
                "i", "r"), yaxs = ifelse(std.y.range, "i", "r"))
        par(tcl = -par()$mgp[2] * tick.length)
        if (is.null(displayed.nb)) {
            sampled.data <- as.vector(data)
            if (corner.text == "") {
                corner.text <- paste0("ALL VALUES OF THE DATASET DISPLAYED")
            }
            else {
                corner.text <- paste0(corner.text, "\nALL VALUES OF THE DATASET DISPLAYED")
            }
        }
        else {
            if (length(as.vector(data)) > displayed.nb) {
                sampled.data <- sample(as.vector(data), displayed.nb, 
                  replace = FALSE)
                if (corner.text == "") {
                  corner.text <- paste0("WARNING: ONLY ", displayed.nb, 
                    " VALUES ARE DISPLAYED AMONG THE ", length(as.vector(data)), 
                    " VALUES OF THE DATASET ANALYZED")
                }
                else {
                  corner.text <- paste0(corner.text, "\nWARNING: ONLY ", 
                    displayed.nb, " VALUES ARE DISPLAYED AMONG THE ", 
                    length(as.vector(data)), " VALUES OF THE DATASET ANALYZED")
                }
            }
            else {
                sampled.data <- as.vector(data)
                if (corner.text == "") {
                  corner.text <- paste0("WARNING: THE DISPLAYED NUMBER OF VALUES PARAMETER ", 
                    deparse(substitute(displayed.nb)), " HAS BEEN SET TO ", 
                    displayed.nb, " WHICH IS ABOVE THE NUMBER OF VALUES OF THE DATASET ANALYZED -> ALL VALUES DISPLAYED")
                }
                else {
                  corner.text <- paste0(corner.text, "\nWARNING: THE DISPLAYED NUMBER OF VALUES PARAMETER ", 
                    deparse(substitute(displayed.nb)), " HAS BEEN SET TO ", 
                    displayed.nb, " WHICH IS ABOVE THE NUMBER OF VALUES OF THE DATASET ANALYZED -> ALL VALUES DISPLAYED")
                }
            }
        }
        if (!is.null(na.nb)) {
            if (corner.text == "") {
                corner.text <- paste0("WARNING: NUMBER OF NA REMOVED IS ", 
                  na.nb)
            }
            else {
                corner.text <- paste0("WARNING: NUMBER OF NA REMOVED IS ", 
                  na.nb)
            }
        }
        stripchart(sampled.data, method = "jitter", jitter = 0.4, 
            vertical = FALSE, ylim = c(0.5, 1.5), group.names = "", 
            xlab = "Value", ylab = "", pch = 1, cex = cex.pt/0.2)
        fun.rug(y.nb.inter.tick.f = 0)
        boxplot(as.vector(data), horizontal = TRUE, add = TRUE, 
            boxwex = 0.4, staplecol = col.box, whiskcol = col.box, 
            medcol = col.box, boxcol = col.box, range = 0, whisklty = 1)
        m <- mean(as.vector(data), na.rm = TRUE)
        s <- sd(as.vector(data), na.rm = TRUE)
        segments(m, 0.8, m, 1, lwd = 2, col = "red")
        segments(m - 1.96 * s, 0.9, m + 1.96 * s, 0.9, lwd = 1, 
            col = "red")
        graph.xlim <- par()$usr[1:2]
        if (interval.scale.disp == TRUE) {
            fun.interval.scale.display(data.f = data)
            if (corner.text == "") {
                corner.text <- paste0("MULTIPLYING FACTOR DISPLAYED (MEAN +/- SD) ON SCALES: ", 
                  paste(formatC(round(qnorm(quantiles.selection), 
                    2))[-(1:(length(quantiles.selection) - 1)/2)], 
                    collapse = ", "), "\nQUANTILES DISPLAYED ON SCALES: ", 
                  paste(quantiles.selection, collapse = ", "))
            }
            else {
                corner.text <- paste0(corner.text, "\nMULTIPLYING FACTOR DISPLAYED (MEAN +/- SD) ON SCALES: ", 
                  paste(formatC(round(qnorm(quantiles.selection), 
                    2))[-(1:(length(quantiles.selection) - 1)/2)], 
                    collapse = ", "), "\nQUANTILES DISPLAYED ON SCALES: ", 
                  paste(quantiles.selection, collapse = ", "))
            }
        }
        output.tempo <- fun.add.cut(data.f = data, return.f = TRUE)
        if (trim.return == TRUE) {
            output <- output.tempo
        }
        par(xpd = NA)
        if (trim.method != "") {
            if (corner.text == "") {
                corner.text <- paste0("SELECTED CUT-OFFS (PROPORTION): ", 
                  paste(trim.cutoffs, collapse = ", "), "\nSELECTED CUT-OFFS: ", 
                  paste(output.tempo$real.trim.cutoffs, collapse = ", "))
            }
            else {
                corner.text <- paste0(corner.text, "\nSELECTED CUT-OFFS (PROPORTION): ", 
                  paste(trim.cutoffs, collapse = ", "), "\nSELECTED CUT-OFFS: ", 
                  paste(output.tempo$real.trim.cutoffs, collapse = ", "))
            }
            if (interval.scale.disp == TRUE) {
                legend(x = (par("usr")[1] - ((par("usr")[2] - 
                  par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
                  par("plt")[1] - ((par("usr")[2] - par("usr")[1])/(par("omd")[2] - 
                  par("omd")[1])) * par("omd")[1]), y = (par("usr")[4] + 
                  ((par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
                    par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - 
                  par("usr")[3])/(par("omd")[4] - par("omd")[3])) * 
                  (1 - par("omd")[4])/2), legend = c(c("min, Q1, Median, Q3, max"), 
                  "mean +/- 1.96sd", paste0("Trimming interval: ", 
                    paste0(trim.cutoffs, collapse = " , ")), 
                  "Mean +/- sd multiplying factor", "Quantile"), 
                  yjust = 0, lty = 1, col = c(col.box, "red", 
                    color.cut, col.mean, col.quantile), bty = "n", 
                  cex = amplif.legend)
            }
            else {
                legend(x = (par("usr")[1] - ((par("usr")[2] - 
                  par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
                  par("plt")[1] - ((par("usr")[2] - par("usr")[1])/(par("omd")[2] - 
                  par("omd")[1])) * par("omd")[1]), y = (par("usr")[4] + 
                  ((par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
                    par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - 
                  par("usr")[3])/(par("omd")[4] - par("omd")[3])) * 
                  (1 - par("omd")[4])/2), legend = c(c("min, Q1, Median, Q3, max"), 
                  "mean +/- 1.96sd", paste0("Trimming interval: ", 
                    paste0(trim.cutoffs, collapse = " , "))), 
                  yjust = 0, lty = 1, col = c(col.box, "red", 
                    color.cut), bty = "n", cex = amplif.legend, 
                  y.intersp = 1.25)
            }
        }
        else {
            if (interval.scale.disp == TRUE) {
                legend(x = (par("usr")[1] - ((par("usr")[2] - 
                  par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
                  par("plt")[1] - ((par("usr")[2] - par("usr")[1])/(par("omd")[2] - 
                  par("omd")[1])) * par("omd")[1]), y = (par("usr")[4] + 
                  ((par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
                    par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - 
                  par("usr")[3])/(par("omd")[4] - par("omd")[3])) * 
                  (1 - par("omd")[4])/2), legend = c(c("min, Q1, Median, Q3, max"), 
                  "mean +/- sd", "Mean +/- sd multiplying factor", 
                  "Quantile"), yjust = 0, lty = 1, col = c(col.box, 
                  "red", col.mean, col.quantile), bty = "n", 
                  cex = amplif.legend)
            }
            else {
                legend(x = (par("usr")[1] - ((par("usr")[2] - 
                  par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
                  par("plt")[1] - ((par("usr")[2] - par("usr")[1])/(par("omd")[2] - 
                  par("omd")[1])) * par("omd")[1]), y = (par("usr")[4] + 
                  ((par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
                    par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - 
                  par("usr")[3])/(par("omd")[4] - par("omd")[3])) * 
                  (1 - par("omd")[4])/2), legend = c(c("min, Q1, Median, Q3, max"), 
                  "mean +/- sd"), yjust = 0, lty = 1, col = c(col.box, 
                  "red"), bty = "n", cex = amplif.legend, y.intersp = 1.25)
            }
        }
        par(xpd = FALSE, xaxs = ifelse(std.x.range, "i", "r"), 
            yaxs = ifelse(std.y.range, "i", "r"))
        hist(as.vector(data), main = "", xlim = graph.xlim, xlab = "Value", 
            ylab = "Density", col = grey(0.25))
        abline(h = par()$usr[3])
        fun.rug()
        if (interval.scale.disp == TRUE) {
            fun.interval.scale.display(data.f = data)
        }
        fun.add.cut(data.f = data)
        par(xaxs = ifelse(std.x.range, "i", "r"))
        stripchart(rank(sampled.data), method = "stack", vertical = FALSE, 
            ylim = c(0.99, 1.3), group.names = "", xlab = "Rank of values", 
            ylab = "", pch = 1, cex = cex.pt/0.2)
        fun.rug(y.nb.inter.tick.f = 0)
        x.text <- par("usr")[2] + (par("usr")[2] - par("usr")[1])/(par("plt")[2] - 
            par("plt")[1]) * (1 - par("plt")[2])/2
        y.text <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
            par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - 
            par("usr")[3])/((par()$omd[4]/2) * ((par("plt")[4] - 
            par("plt")[3])))) * (1 - par("omd")[4]))
        par(xpd = NA)
        text(x = x.text, y = y.text, paste0(corner.text), adj = c(1, 
            1.1), cex = corner.text.size)
        par(xpd = FALSE)
        par(xaxs = ifelse(std.x.range, "i", "r"), yaxs = ifelse(std.y.range, 
            "i", "r"))
        qqnorm(as.vector(sampled.data), main = "", datax = TRUE, 
            ylab = "Value", pch = 1, col = "red", cex = cex.pt/0.2)
        fun.rug()
        if (diff(quantile(as.vector(data), probs = c(0.25, 0.75), 
            na.rm = TRUE)) != 0) {
            qqline(as.vector(data), datax = TRUE)
        }
        if (interval.scale.disp == TRUE) {
            fun.interval.scale.display(data.f = data)
        }
        fun.add.cut(data.f = data)
    }
    if (trim.return == TRUE) {
        return(output)
    }
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
