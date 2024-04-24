#' @title trim
#' @description
#' Trim and display values from a numeric vector or matrix.
#' 
#' Plot 4 graphs: stripchart of values, stripchart of rank of values, histogram and normal QQPlot.
#' 
#' Different kinds of intervals are displayed on the top of graphes to facilitate the analysis of the variable and a trimming setting.
#' 
#' The trimming interval chosen is displayed on top of graphs.
#' 
#' Both trimmed and not trimmed values are returned in a list.
#' @param data Values to plot (either a numeric vector or a numeric matrix).
#' @param displayed.nb Single numeric value indicating the number of values displayed. If NULL, all the values are displayed. Otherwise, if the number of values is over displayed.nb, then displayed.nb values are displayed after random selection.
#' @param single.value.display Single logical value. Provide the 4 graphs if data is made of a single (potentially repeated value)? If FALSE, an empty graph is displayed if data is made of a single (potentially repeated value). And the return list is made of NULL compartments.
#' @param trim.method Write "" if not required. write "mean.sd" if mean +/- sd has to be displayed as a trimming interval (only recommanded for normal distribution). Write "quantile" to display a trimming interval based on quantile cut-offs. No other possibility allowed. See trim.cutoffs below.
#' @param trim.cutoffs 2 values cutoff for the trimming interval displayed, each value between 0 and 1. Not used if trim.method == "".The couple of values c(lower, upper) represents the lower and upper boundaries of the trimming interval (in proportion), which represent the interval of distribution kept (between 0 and 1). Example: trim.cutoffs = c(0.05, 0.975). What is strictly kept for the display is ]lower , upper[, boundaries excluded. Using the "mean.sd" method, 0.025 and 0.975 represent 95\% CI which is mean +/- 1.96 * sd.
#' @param interval.scale.disp Single logical value. Display sd and quantiles intervals on top of graphs ?
#' @param down.space Single positive numeric value indicating the lower vertical margin (in inches, mai argument of par()).
#' @param left.space Single positive numeric value indicating the left horizontal margin (in inches, mai argument of par()).
#' @param up.space Single positive numeric value indicating the upper vertical margin between plot region and grapical window (in inches, mai argument of par()).
#' @param right.space Single positive numeric value indicating the right horizontal margin (in inches, mai argument of par()).
#' @param orient Single positive numeric value indicating the scale number orientation (las argument of par()). 0, always parallel to the axis; 1, always horizontal; 2, always perpendicular to the axis; 3, always vertical.
#' @param dist.legend Single positive numeric value that moves axis legends away in inches (first number of mgp argument of par() but in inches thus / 0.2).
#' @param box.type The bty argument of par(). Either "o", "l", "7", "c", "u", "]", the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box.
#' @param amplif.label Single positive numeric value to increase or decrease the size of the text in legends.
#' @param amplif.axis Single positive numeric value to increase or decrease the size of the scale numbers in axis.
#' @param std.x.range Single logical value. Standard range on the x-axis? TRUE (no range extend) or FALSE (4\% range extend). Controls xaxs argument of par() (TRUE is xaxs = "i", FALSE is xaxs = "r").
#' @param std.y.range Single logical value. Standard range on the y-axis? TRUE (no range extend) or FALSE (4\% range extend). Controls yaxs argument of par() (TRUE is yaxs = "i", FALSE is yaxs = "r").
#' @param cex.pt Single positive numeric value indicating the size of points in stripcharts (in inches, thus cex.pt will be thereafter / 0.2).
#' @param col.box Single character string indicating the color of boxplot.
#' @param x.nb.inter.tick Single positive integer value indicating the number of secondary ticks between main ticks on x-axis (only if not log scale). Zero means non secondary ticks.
#' @param y.nb.inter.tick Single positive integer value indicating the number of secondary ticks between main ticks on y-axis (only if not log scale). Zero means non secondary ticks.
#' @param tick.length Single proportion value indicating the length of the ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc. 0 means no tick.
#' @param sec.tick.length Single proportion value indicating the length of the secondary ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc., 0 for no ticks).
#' @param corner.text Single character string. Text to add at the top right corner of the window.
#' @param amplif.legend Single positive numeric value to increase or decrease the size of the text of legend.
#' @param corner.text.size Single positive numeric value to increase or decrease the size of the text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2.
#' @param trim.return Single logical value. Return the trimmed and non trimmed values? NULL returned for trimmed and non trimmed values if trim.method == "".
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns
#' A list containing:
#' 
#' - $trim.method: correspond to the trim.method argument.
#' 
#' - $trim.cutoffs: correspond to the trim.cutoffs argument.
#' 
#' - $real.trim.cutoffs: the two boundary values in the unit of the numeric vector or numeric matrix analyzed.
#' 
#' - $trimmed.values: the values outside of the trimming interval as defined in the trim.cutoffs argument.
#' 
#' - $kept.values: the values inside the trimming interval as defined in the trim.cutoffs argument.
#' @examples
#' trim(data = c(1:100, 1:10), displayed.nb = NULL, single.value.display = FALSE, trim.method = "mean.sd", trim.cutoffs = c(0.05, 0.975), interval.scale.disp = TRUE, down.space = 0.75, left.space = 0.75, up.space = 0.3, right.space = 0.25, orient = 1, dist.legend = 0.37, box.type = "l", amplif.label = 1.25, amplif.axis = 1.25, std.x.range = TRUE, std.y.range = TRUE, cex.pt = 0.2, col.box = grDevices::hsv(0.55, 0.8, 0.8), x.nb.inter.tick = 4, y.nb.inter.tick = 0, tick.length = 0.5, sec.tick.length = 0.3, corner.text = "", amplif.legend = 1, corner.text.size = 0.75, trim.return = TRUE)
#' @importFrom saferDev arg_check
#' @export
trim <- function(
        data, 
        displayed.nb = NULL, 
        single.value.display = FALSE, 
        trim.method = "", 
        trim.cutoffs = base::c(0.05, 
                         0.975), 
        interval.scale.disp = TRUE, 
        down.space = 0.75, 
        left.space = 0.75, 
        up.space = 0.3, 
        right.space = 0.25, 
        orient = 1, 
        dist.legend = 0.37, 
        box.type = "l", 
        amplif.label = 1.25, 
        amplif.axis = 1.25, 
        std.x.range = TRUE, 
        std.y.range = TRUE, 
        cex.pt = 0.2, 
        col.box = grDevices::hsv(0.55, 
                      0.8, 
                      0.8), 
        x.nb.inter.tick = 4, 
        y.nb.inter.tick = 0, 
        tick.length = 1, 
        sec.tick.length = 0.75, 
        corner.text = "", 
        amplif.legend = 1, 
        corner.text.size = 0.75, 
        trim.return = FALSE,
        safer_check = TRUE
){
    # DEBUGGING
    # data = c(1:100, 1:10) ; displayed.nb = NULL ; single.value.display = FALSE ; trim.method = "quantile" ; trim.cutoffs = c(0.05, 0.975) ; interval.scale.disp = TRUE ; down.space = 1 ; left.space = 1 ; up.space = 0.5 ; right.space = 0.25 ; orient = 1 ; dist.legend = 0.5 ; box.type = "l" ; amplif.label = 1 ; amplif.axis = 1 ; std.x.range = TRUE ; std.y.range = TRUE ; cex.pt = 0.1 ; col.box = grDevices::hsv(0.55, 0.8, 0.8) ; x.nb.inter.tick = 4 ; y.nb.inter.tick = 0 ; tick.length = 0.5 ; sec.tick.length = 0.3 ; corner.text = "" ; amplif.legend = 1 ; corner.text.size = 0.75 ; trim.return = TRUE ; safer_check = TRUE # for function debugging
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
    if(safer_check == TRUE){
        .pack_and_function_check(
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
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    if( ! base::is.null(displayed.nb)){
        tempo <- saferDev::arg_check(data = displayed.nb, class = "vector", mode = "numeric", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(displayed.nb < 2){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: displayed.nb ARGUMENT MUST BE A SINGLE INTEGER VALUE GREATER THAN 1 AND NOT: ", base::paste(displayed.nb, collapse = " "))
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = single.value.display, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = trim.method, options = base::c("", "mean.sd", "quantile"), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = trim.cutoffs, class = "vector", mode = "numeric", length = 2, prop = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = interval.scale.disp, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = down.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = left.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = up.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = right.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = orient, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = dist.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = box.type, options = base::c("o", "l", "7", "c", "u", "]", "n"), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = amplif.label, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = amplif.axis, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = std.x.range, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = std.y.range, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = cex.pt, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = col.box, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.nb.inter.tick, class = "integer", length = 1, neg.values = FALSE, double.as.integer.allowed = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.nb.inter.tick, class = "integer", length = 1, neg.values = FALSE, double.as.integer.allowed = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = sec.tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = corner.text, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = amplif.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = corner.text.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = trim.return, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-base::c(
        "data", 
        # "displayed.nb", # inactivated because can be null
        "single.value.display", 
        "trim.method", 
        "trim.cutoffs", 
        "interval.scale.disp", 
        "down.space", 
        "left.space", 
        "up.space", 
        "right.space", 
        "orient", 
        "dist.legend", 
        "box.type", 
        "amplif.label", 
        "amplif.axis", 
        "std.x.range",
        "std.y.range", 
        "cex.pt", 
        "col.box", 
        "x.nb.inter.tick", 
        "y.nb.inter.tick", 
        "tick.length", 
        "sec.tick.length", 
        "corner.text", 
        "amplif.legend", 
        "corner.text.size", 
        "trim.return",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    if(base::all( ! base::is.finite(data))){ # is.finite() tests if it is not one of the values NA, NaN, Inf or -Inf
        tempo.cat <- base::paste0("ERROR IN ", function.name, " FUNCTION OF THE ", package.name, " PACKAGE: \ndata ARGUMENT CANNOT CONTAIN NA AND Inf ONLY")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # argument checking without arg_check()
    if( ! (base::all(base::class(data) == "numeric") | base::all(base::class(data) == "integer") | (base::all(base::class(data) %in% base::c("matrix", "array")) & base::mode(data) == "numeric"))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data ARGUMENT MUST BE A NUMERIC VECTOR OR NUMERIC MATRIX")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end argument checking without arg_check()
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    if(base::all(base::class(data)%in% base::c("matrix", "array"))){
        data <- base::as.vector(data)
    }
    na.nb <- NULL
    if(base::any(base::is.na(data))){
        na.nb <- base::sum(base::c(base::is.na(data)))
        data <- data[ ! base::is.na(data)]
    }
    color.cut <- grDevices::hsv(0.75, 1, 1) # color of interval selected
    col.mean <- grDevices::hsv(0.25, 1, 0.8) # color of interval using mean+/-sd
    col.quantile <- "orange" # color of interval using quantiles
    quantiles.selection <- base::c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99) # quantiles used in axis to help for choosing trimming cutoffs
    if(single.value.display == FALSE & base::length(base::unique(data)) == 1L){
        graphics::par(bty = "n", xaxt = "n", yaxt = "n", xpd = TRUE)
        base::plot(1, pch = 16, col = "white", xlab = "", ylab = "")
        graphics::text(x = 1, y = 1, base::paste0("No graphic displayed\nBecause data made of a single different value (", base::formatC(base::as.double(base::table(data))), ")"), cex = 2)
        output <- base::list(trim.method = NULL, trim.cutoffs = NULL, real.trim.cutoffs = NULL, trimmed.values = NULL, kept.values = NULL)
    }else{
        output <- base::list(trim.method = trim.method, trim.cutoffs = trim.cutoffs, real.trim.cutoffs = NULL, trimmed.values = NULL, kept.values = NULL)
        fun.rug <- function(sec.tick.length.f = sec.tick.length, x.nb.inter.tick.f = x.nb.inter.tick, y.nb.inter.tick.f = y.nb.inter.tick){
            if(x.nb.inter.tick.f > 0){
                inter.tick.unit <- (graphics::par("xaxp")[2] - graphics::par("xaxp")[1]) / graphics::par("xaxp")[3]
                par.ini <- graphics::par()[base::c("xpd", "tcl")]
                graphics::par(xpd = FALSE)
                graphics::par(tcl = -graphics::par()$mgp[2] * sec.tick.length.f) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
                base::suppressWarnings(graphics::rug(base::seq(graphics::par("xaxp")[1] - 10 * inter.tick.unit, graphics::par("xaxp")[2] + 10 * inter.tick.unit, by = inter.tick.unit / (1 + x.nb.inter.tick.f)), ticksize = NA, side = 1)) # ticksize = NA to allow the use of graphics::par()$tcl value
                graphics::par(par.ini)
                base::rm(par.ini)
            }
            if(y.nb.inter.tick.f > 0){
                inter.tick.unit <- (graphics::par("yaxp")[2] - graphics::par("yaxp")[1]) / graphics::par("yaxp")[3]
                par.ini <- graphics::par()[base::c("xpd", "tcl")]
                graphics::par(xpd = FALSE)
                graphics::par(tcl = -graphics::par()$mgp[2] * sec.tick.length.f) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
                base::suppressWarnings(graphics::rug(base::seq(graphics::par("yaxp")[1] - 10 * inter.tick.unit, graphics::par("yaxp")[2] + 10 * inter.tick.unit, by = inter.tick.unit / (1 + y.nb.inter.tick.f)), ticksize = NA, side = 2)) # ticksize = NA to allow the use of graphics::par()$tcl value
                graphics::par(par.ini)
                base::rm(par.ini)
            }
        }
        fun.add.cut <- function(data.f, trim.method.f = trim.method, trim.cutoffs.f = trim.cutoffs, color.cut.f = color.cut, return.f = FALSE){
            # DEBUGGING
            # data.f = data ; trim.method.f = "mean.sd"; trim.cutoffs.f = trim.cutoffs ; color.cut.f = color.cut ; return.f = TRUE
            real.trim.cutoffs.f <- NULL
            if(trim.method.f != ""){
                data.f <- base::sort(data.f)
                par.ini <- graphics::par()$xpd
                graphics::par(xpd = FALSE)
                if(trim.method.f == "mean.sd"){
                    real.trim.cutoffs.f <- stats::qnorm(trim.cutoffs.f, base::mean(data.f, na.rm = TRUE), stats::sd(data.f, na.rm = TRUE))
                    graphics::abline(v = stats::qnorm(trim.cutoffs.f, base::mean(data.f, na.rm = TRUE), stats::sd(data.f, na.rm = TRUE)), col = color.cut.f)
                    graphics::segments(stats::qnorm(trim.cutoffs.f[1], base::mean(data.f, na.rm = TRUE), stats::sd(data.f, na.rm = TRUE)), graphics::par()$usr[4] * 0.75, stats::qnorm(trim.cutoffs.f[2], base::mean(data.f, na.rm = TRUE), stats::sd(data.f, na.rm = TRUE)), graphics::par()$usr[4] * 0.75, col = color.cut.f)
                }
                if(trim.method.f == "quantile"){
                    real.trim.cutoffs.f <- stats::quantile(data.f, probs = trim.cutoffs.f, type = 7, na.rm = TRUE)
                    graphics::abline(v = stats::quantile(data.f, probs = trim.cutoffs.f, type = 7, na.rm = TRUE), col = color.cut.f)
                    graphics::segments(stats::quantile(data.f, probs = trim.cutoffs.f[1], type = 7, na.rm = TRUE), graphics::par()$usr[4] * 0.75, stats::quantile(data.f, probs = trim.cutoffs.f[2], type = 7, na.rm = TRUE), graphics::par()$usr[4] * 0.75, col = color.cut.f)
                }
                graphics::par(par.ini)
                if(return.f == TRUE){
                    trimmed.values.f <- data.f[data.f <= real.trim.cutoffs.f[1] | data.f >= real.trim.cutoffs.f[2]]
                    kept.values.f <- data.f[data.f > real.trim.cutoffs.f[1] & data.f < real.trim.cutoffs.f[2]]
                }
            }else{
                real.trim.cutoffs.f <- NULL
                trimmed.values.f <- NULL
                kept.values.f <- NULL
            }
            # output
            # warning output
            # end warning output
            if(return.f == TRUE){
                output <- base::list(trim.method = trim.method.f, trim.cutoffs = trim.cutoffs.f, real.trim.cutoffs = real.trim.cutoffs.f, trimmed.values = trimmed.values.f, kept.values = kept.values.f)
                base::return(output)
            }
        }
        fun.interval.scale.display <- function(data.f, col.quantile.f = col.quantile, quantiles.selection.f = quantiles.selection, col.mean.f = col.mean){ # intervals on top of graphs
            par.ini <- graphics::par()[base::c("mgp", "xpd")]
            graphics::par(mgp = base::c(0.25, 0.25, 0), xpd = NA)
            graphics::axis(side = 3, at = base::c(graphics::par()$usr[1], graphics::par()$usr[2]), labels = base::rep("", 2), col = col.quantile.f, lwd.ticks = 0)
            graphics::par(xpd = FALSE)
            graphics::axis(side = 3, at = stats::quantile(base::as.vector(data.f), probs = quantiles.selection.f, type = 7, na.rm = TRUE), labels = quantiles.selection.f, col.axis = col.quantile.f, col = col.quantile.f)
            graphics::par(mgp = base::c(1.75, 1.75, 1.5), xpd = NA)
            graphics::axis(side = 3, at = base::c(graphics::par()$usr[1], graphics::par()$usr[2]), labels = base::rep("", 2), col = col.mean.f, lwd.ticks = 0)
            graphics::par(xpd = FALSE)
            graphics::axis(side = 3, at = m + s * stats::qnorm(quantiles.selection.f), labels = base::formatC(base::round(stats::qnorm(quantiles.selection.f), 2)), col.axis = col.mean.f, col = col.mean.f, lwd.ticks = 1)
            graphics::par(par.ini)
        }
        zone<-base::matrix(1:4, ncol=2)
        graphics::layout(zone)
        graphics::par(omi = base::c(0, 0, 1.5, 0), mai = base::c(down.space, left.space, up.space, right.space), las = orient, mgp = base::c(dist.legend / 0.2, 0.5, 0), xpd = FALSE, bty= box.type, cex.lab = amplif.label, cex.axis = amplif.axis, xaxs = base::ifelse(std.x.range, "i", "r"), yaxs = base::ifelse(std.y.range, "i", "r"))
        graphics::par(tcl = -graphics::par()$mgp[2] * tick.length) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
        if(base::is.null(displayed.nb)){
            sampled.data <- base::as.vector(data)
            if(corner.text == ""){
                corner.text <- base::paste0("ALL VALUES OF THE DATASET DISPLAYED")
            }else{
                corner.text <- base::paste0(corner.text, "\nALL VALUES OF THE DATASET DISPLAYED")
            }
        }else{
            if(base::length(base::as.vector(data)) > displayed.nb){
                sampled.data <- base::sample(base::as.vector(data), displayed.nb, replace = FALSE)
                if(corner.text == ""){
                    corner.text <- base::paste0("WARNING: ONLY ", displayed.nb, " VALUES ARE DISPLAYED AMONG THE ", base::length(base::as.vector(data)), " VALUES OF THE DATASET ANALYZED")
                }else{
                    corner.text <- base::paste0(corner.text, "\nWARNING: ONLY ", displayed.nb, " VALUES ARE DISPLAYED AMONG THE ", base::length(base::as.vector(data)), " VALUES OF THE DATASET ANALYZED")
                }
            }else{
                sampled.data <- base::as.vector(data)
                if(corner.text == ""){
                    corner.text <- base::paste0("WARNING: THE DISPLAYED NUMBER OF VALUES PARAMETER ", base::deparse(base::substitute(displayed.nb)), " HAS BEEN SET TO ", displayed.nb, " WHICH IS ABOVE THE NUMBER OF VALUES OF THE DATASET ANALYZED -> ALL VALUES DISPLAYED")
                }else{
                    corner.text <- base::paste0(corner.text, "\nWARNING: THE DISPLAYED NUMBER OF VALUES PARAMETER ", base::deparse(base::substitute(displayed.nb)), " HAS BEEN SET TO ", displayed.nb, " WHICH IS ABOVE THE NUMBER OF VALUES OF THE DATASET ANALYZED -> ALL VALUES DISPLAYED")
                }
            }
        }
        if( ! base::is.null(na.nb)){
            if(corner.text == ""){
                corner.text <- base::paste0("WARNING: NUMBER OF NA REMOVED IS ", na.nb)
            }else{
                corner.text <- base::paste0("WARNING: NUMBER OF NA REMOVED IS ", na.nb)
            }
        }
        graphics::stripchart(sampled.data, method="jitter", jitter=0.4, vertical=FALSE, ylim= base::c(0.5, 1.5), group.names = "", xlab = "Value", ylab="", pch=1, cex = cex.pt / 0.2)
        fun.rug(y.nb.inter.tick.f = 0)
        graphics::boxplot(base::as.vector(data), horizontal=TRUE, add=TRUE, boxwex = 0.4, staplecol = col.box, whiskcol = col.box, medcol = col.box, boxcol = col.box, range = 0, whisklty = 1)
        m <- base::mean(base::as.vector(data), na.rm = TRUE)
        s <- stats::sd(base::as.vector(data), na.rm = TRUE)
        graphics::segments(m, 0.8, m, 1, lwd=2, col="red") # mean 
        graphics::segments(m -1.96 * s, 0.9, m + 1.96 * s, 0.9, lwd=1, col="red") # mean 
        graph.xlim <- graphics::par()$usr[1:2] # for graphics::hist() and stats::qqnorm() below
        if(interval.scale.disp == TRUE){
            fun.interval.scale.display(data.f = data)
            if(corner.text == ""){
                corner.text <- base::paste0("MULTIPLYING FACTOR DISPLAYED (MEAN +/- SD) ON SCALES: ", base::paste(base::formatC(base::round(stats::qnorm(quantiles.selection), 2))[-(1:(base::length(quantiles.selection) - 1) / 2)], collapse = ", "), "\nQUANTILES DISPLAYED ON SCALES: ", base::paste(quantiles.selection, collapse = ", "))
            }else{
                corner.text <- base::paste0(corner.text, "\nMULTIPLYING FACTOR DISPLAYED (MEAN +/- SD) ON SCALES: ", base::paste(base::formatC(base::round(stats::qnorm(quantiles.selection), 2))[-(1:(base::length(quantiles.selection) - 1) / 2)], collapse = ", "), "\nQUANTILES DISPLAYED ON SCALES: ", base::paste(quantiles.selection, collapse = ", "))
            }
        }
        output.tempo <- fun.add.cut(data.f = data, return.f = TRUE) # to recover real.trim.cutoffs
        if(trim.return == TRUE){
            output <- output.tempo
        }
        graphics::par(xpd = NA)
        if(trim.method != ""){
            if(corner.text == ""){
                corner.text <- base::paste0("SELECTED CUT-OFFS (PROPORTION): ", base::paste(trim.cutoffs, collapse = ", "), "\nSELECTED CUT-OFFS: ", base::paste(output.tempo$real.trim.cutoffs, collapse = ", "))
            }else{
                corner.text <- base::paste0(corner.text, "\nSELECTED CUT-OFFS (PROPORTION): ", base::paste(trim.cutoffs, collapse = ", "), "\nSELECTED CUT-OFFS: ", base::paste(output.tempo$real.trim.cutoffs, collapse = ", "))
            }
            if(interval.scale.disp == TRUE){
                graphics::legend(x = (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("omd")[2] - graphics::par("omd")[1])) * graphics::par("omd")[1]), y = (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("omd")[4] - graphics::par("omd")[3])) * (1 - graphics::par("omd")[4]) / 2), legend = base::c(base::c("min, Q1, Median, Q3, max"), "mean +/- 1.96sd", base::paste0("Trimming interval: ", base::paste0(trim.cutoffs, collapse = " , ")), "Mean +/- sd multiplying factor", "Quantile"), yjust = 0, lty=1, col=base::c(col.box, "red", color.cut, col.mean, col.quantile), bty="n", cex = amplif.legend)
            }else{
                graphics::legend(x = (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("omd")[2] - graphics::par("omd")[1])) * graphics::par("omd")[1]), y = (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("omd")[4] - graphics::par("omd")[3])) * (1 - graphics::par("omd")[4]) / 2), legend = base::c(base::c("min, Q1, Median, Q3, max"), "mean +/- 1.96sd", base::paste0("Trimming interval: ", base::paste0(trim.cutoffs, collapse = " , "))), yjust = 0, lty=1, col=base::c(col.box, "red", color.cut), bty="n", cex = amplif.legend, y.intersp=1.25)
            }
        }else{
            if(interval.scale.disp == TRUE){
                graphics::legend(x = (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("omd")[2] - graphics::par("omd")[1])) * graphics::par("omd")[1]), y = (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("omd")[4] - graphics::par("omd")[3])) * (1 - graphics::par("omd")[4]) / 2), legend = base::c(base::c("min, Q1, Median, Q3, max"), "mean +/- sd", "Mean +/- sd multiplying factor", "Quantile"), yjust = 0, lty=1, col=base::c(col.box, "red", col.mean, col.quantile), bty="n", cex = amplif.legend)
            }else{
                graphics::legend(x = (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("omd")[2] - graphics::par("omd")[1])) * graphics::par("omd")[1]), y = (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("omd")[4] - graphics::par("omd")[3])) * (1 - graphics::par("omd")[4]) / 2), legend = base::c(base::c("min, Q1, Median, Q3, max"), "mean +/- sd"), yjust = 0, lty=1, col=base::c(col.box, "red"), bty="n", cex = amplif.legend, y.intersp=1.25)
            }
        }
        graphics::par(xpd = FALSE, xaxs = base::ifelse(std.x.range, "i", "r"), yaxs = base::ifelse(std.y.range, "i", "r"))
        graphics::hist(base::as.vector(data), main = "", xlim = graph.xlim, xlab = "Value", ylab="Density", col = grDevices::grey(0.25)) # removed: breaks = seq(min(as.vector(data), na.rm = TRUE), max(as.vector(data), na.rm = TRUE), length.out = length(as.vector(data)) / 10)
        graphics::abline(h = graphics::par()$usr[3])
        fun.rug()
        if(interval.scale.disp == TRUE){
            fun.interval.scale.display(data.f = data)
        }
        fun.add.cut(data.f = data)
        graphics::par(xaxs = base::ifelse(std.x.range, "i", "r"))
        graphics::stripchart(base::rank(sampled.data), method="stack", vertical=FALSE, ylim=base::c(0.99, 1.3), group.names = "", xlab = "Rank of values", ylab="", pch=1, cex = cex.pt / 0.2)
        fun.rug(y.nb.inter.tick.f = 0)
        x.text <- graphics::par("usr")[2] + (graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1]) * (1 - graphics::par("plt")[2]) / 2
        y.text <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par()$omd[4] / 2) * ((graphics::par("plt")[4] - graphics::par("plt")[3])))) * (1 - graphics::par("omd")[4])) # BEWARE. Here in "(graphics::par()$omd[4] / 2", division by two because there are 2 graphs staked on the y axis, and not one
        graphics::par(xpd=NA)
        graphics::text(x = x.text, y = y.text, base::paste0(corner.text), adj=base::c(1, 1.1), cex = corner.text.size) # text at the topright corner
        graphics::par(xpd=FALSE)
        graphics::par(xaxs = base::ifelse(std.x.range, "i", "r"), yaxs = base::ifelse(std.y.range, "i", "r"))
        stats::qqnorm(base::as.vector(sampled.data), main = "", datax = TRUE, ylab = "Value", pch = 1, col = "red", cex = cex.pt / 0.2)
        fun.rug()
        if(base::diff(stats::quantile(base::as.vector(data), probs = base::c(0.25, 0.75), na.rm = TRUE)) != 0){ # otherwise, error generated
            stats::qqline(base::as.vector(data), datax = TRUE)
        }
        if(interval.scale.disp == TRUE){
            fun.interval.scale.display(data.f = data)
        }
        fun.add.cut(data.f = data)
    }
    if(trim.return == TRUE){
        base::return(output)
    }
    # end output
    # end main code
}