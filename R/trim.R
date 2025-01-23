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
#' @param seed Single integer number used by set.seed(). Write NULL if random result is required, an integer otherwise. BEWARE: if not NULL,the function will systematically return the same result when the other parameters keep the same settings.
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) correct lib_path argument value 2) required functions and related packages effectively present in local R lybraries and 3) R classical operators (like "<-") not overwritten by another package because of the R scope. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns  An error message if at least one of the checked packages is missing in lib_path, or if at least one of the checked functions is missing in the required package,
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
#' @seealso \code{\link{head}}, \code{\link{tail}}
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' trim(data = c(1:100, 1:10), displayed.nb = NULL, single.value.display = FALSE, trim.method = "mean.sd", trim.cutoffs = c(0.05, 0.975), interval.scale.disp = TRUE, down.space = 0.75, left.space = 0.75, up.space = 0.3, right.space = 0.25, orient = 1, dist.legend = 0.37, box.type = "l", amplif.label = 1.25, amplif.axis = 1.25, std.x.range = TRUE, std.y.range = TRUE, cex.pt = 0.2, col.box = grDevices::hsv(0.55, 0.8, 0.8), x.nb.inter.tick = 4, y.nb.inter.tick = 0, tick.length = 0.5, sec.tick.length = 0.3, corner.text = "", amplif.legend = 1, corner.text.size = 0.75, trim.return = TRUE)
#' @importFrom saferDev arg_check
#' @export
trim <- function(
    data, 
    displayed.nb = NULL, 
    single.value.display = FALSE, 
    trim.method = "", 
    trim.cutoffs = base::c(0.05, 0.975), 
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
    seed = NULL, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # data = c(1:100, 1:10) ; displayed.nb = NULL ; single.value.display = FALSE ; trim.method = "quantile" ; trim.cutoffs = c(0.05, 0.975) ; interval.scale.disp = TRUE ; down.space = 1 ; left.space = 1 ; up.space = 0.5 ; right.space = 0.25 ; orient = 1 ; dist.legend = 0.5 ; box.type = "l" ; amplif.label = 1 ; amplif.axis = 1 ; std.x.range = TRUE ; std.y.range = TRUE ; cex.pt = 0.1 ; col.box = grDevices::hsv(0.55, 0.8, 0.8) ; x.nb.inter.tick = 4 ; y.nb.inter.tick = 0 ; tick.length = 0.5 ; sec.tick.length = 0.3 ; corner.text = "" ; amplif.legend = 1 ; corner.text.size = 0.75 ; trim.return = TRUE ; seed = 1 ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    #### package name
    package_name <- "saferTool2" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are espression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }
    # end evaluation of values if they are espression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name,
        collapse = NULL, 
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    intern_error_text_start <- base::paste0(
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present below 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "data"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
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
        # "seed", # inactivated because can be null
        "safer_check" 
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data", 
        "displayed.nb", 
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
        "seed", 
        "safer_check", 
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval, 
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even is 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(tempo_arg_user_setting_eval[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval, 
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ), 
                FUN = function(x){
                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even is 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(arg_user_setting_names[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n", 
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(safer_check == base::quote(expr = ), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if(safer_check == TRUE){
        if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
            if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                    base::ifelse(test = base::length(x = lib_path) == 0 | base::all(lib_path == base::quote(expr = ), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
                tempo_log <- ! base::dir.exists(paths = lib_path)
                tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
                tempo_cat_b[tempo_cat_b == ""] <- "\"\""
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH",
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""), 
                    " INDICATED IN THE lib_path ARGUMENT DO", 
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"), 
                    " NOT EXIST:\n", 
                    base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            }else{
                ini_lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base:::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                "saferDev::arg_check", 
                "saferDev:::.base_op_check"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    if( ! base::is.null(displayed.nb)){
        tempo <- saferDev::arg_check(data = displayed.nb, class = "vector", typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = single.value.display, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = trim.method, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("", "mean.sd", "quantile"), all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = trim.cutoffs, class = "vector", typeof = NULL, mode = "numeric", length = 2, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = interval.scale.disp, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = down.space, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = left.space, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = up.space, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = right.space, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = orient, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = dist.legend, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box.type, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("o", "l", "7", "c", "u", "]", "n"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = amplif.label, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = amplif.axis, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = std.x.range, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = std.y.range, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = cex.pt, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = col.box, class = "vector", typeof = NULL, mode = "character", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = x.nb.inter.tick, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y.nb.inter.tick, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = tick.length, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = sec.tick.length, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = corner.text, class = "vector", typeof = NULL, mode = "character", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = amplif.legend, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = corner.text.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = trim.return, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(seed)){
        tempo <- saferDev::arg_check(data = seed, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    # lib_path already checked above
    # safer_check already checked above
    # error_text already checked above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    tempo_arg <- base::c(
        # "trim.method", # inactivated because can be ""
        "box.type"
        # "corner.text", # inactivated because can be ""
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    # Warning: seeding is always at the .GlobalEnv level, whenever the seeding is applied inside a function, another envir, etc.
    if (base::exists(x = ".Random.seed", where = -1, envir = .GlobalEnv, frame = , mode = "any", inherits = TRUE)) {
        # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo.random.seed <- .Random.seed
        base::on.exit(expr = base::assign(x = ".Random.seed", value = tempo.random.seed, pos = -1, envir = .GlobalEnv, inherits = FALSE, immediate = TRUE), add = TRUE, after = TRUE)
    }else{
        base::on.exit(expr = base::set.seed(seed = NULL, kind = NULL, normal.kind = NULL, sample.kind = NULL), add = TRUE, after = TRUE) # inactivate seeding -> return to complete randomness
    }
    base::set.seed(seed = seed, kind = NULL, normal.kind = NULL, sample.kind = NULL) # seed value is the seed argument of the function
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    # check the number of graphic devices on exit
    # not required
    # end check the number of graphic devices on exit
    # restore the graphic parameters on exit
    if(base::length(x = grDevices::dev.list()) > 0){
        par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = "warning") # to recover the present graphical parameters
        base::on.exit(expr = base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = "warning"), add = TRUE, after = TRUE)
    }
    # end restore the graphic parameters on exit
    ######## end graphic device checking

    ######## other checkings
    if(base::all( ! base::is.finite(data))){ # base::is.finite() tests if it is not one of the values NA, NaN, Inf or -Inf
        tempo.cat <- base::paste0(error_text_start, "data ARGUMENT CANNOT CONTAIN NA AND Inf ONLY.")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(data) == "numeric") | base::all(base::class(data) == "integer") | (base::all(base::class(data) %in% base::c("matrix", "array")) & base::mode(data) == "numeric"))){
        tempo.cat <- base::paste0(error_text_start, "data ARGUMENT MUST BE A NUMERIC VECTOR OR NUMERIC MATRIX.")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! base::is.null(displayed.nb)){
        if(displayed.nb < 2){
            tempo.cat <- base::paste0(error_text_start, "displayed.nb ARGUMENT MUST BE A SINGLE INTEGER VALUE GREATER THAN 1 AND NOT:\n", base::paste0(displayed.nb, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
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
        graphics::hist(base::as.vector(data), main = "", xlim = graph.xlim, xlab = "Value", ylab="Density", col = grDevices::grey(0.25)) # removed: breaks = base::seq(base::min(base::as.vector(data), na.rm = TRUE), base::max(base::as.vector(data), na.rm = TRUE), length.out = base::length(base::as.vector(data)) / 10)
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

    #### end main code

    #### output
    if(trim.return == TRUE){
        base::return(output)
    }
    #### end output

    #### warning output
    #### end warning output




}