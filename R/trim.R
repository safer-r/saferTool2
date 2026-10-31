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
#' @param displayed_nb Single integer-like numeric value, greater than 1, indicating the number of values displayed. If NULL, all the values are displayed. Otherwise, if the number of values is over displayed_nb, then displayed_nb values are displayed after random selection.
#' @param single_value_display Single logical value. Provide the 4 graphs if data is made of a single (potentially repeated value)? If FALSE, an empty graph is displayed if data is made of a single (potentially repeated value). And the return list is made of NULL compartments.
#' @param trim_method Write "" if not required. write "mean.sd" if mean +/- sd has to be displayed as a trimming interval (only recommanded for normal distribution). Write "quantile" to display a trimming interval based on quantile cut-offs. No other possibility allowed. See trim_cutoffs below.
#' @param trim_cutoffs 2 values cutoff for the trimming interval displayed, each value between 0 and 1. Not used if trim_method == "".The couple of values c(lower, upper) represents the lower and upper boundaries of the trimming interval (in proportion), which represent the interval of distribution kept (between 0 and 1). Example: trim_cutoffs = c(0.05, 0.975). What is strictly kept for the display is ]lower , upper[, boundaries excluded. Using the "mean.sd" method, 0.025 and 0.975 represent 95\% CI which is mean +/- 1.96 * sd.
#' @param interval_scale_disp Single logical value. Display sd and quantiles intervals on top of graphs ?
#' @param down_space Single positive numeric value indicating the lower vertical margin (in inches, mai argument of par()).
#' @param left_space Single positive numeric value indicating the left horizontal margin (in inches, mai argument of par()).
#' @param up_space Single positive numeric value indicating the upper vertical margin between plot region and grapical window (in inches, mai argument of par()).
#' @param right_space Single positive numeric value indicating the right horizontal margin (in inches, mai argument of par()).
#' @param orient Single positive numeric value indicating the scale number orientation (las argument of par()). 0, always parallel to the axis; 1, always horizontal; 2, always perpendicular to the axis; 3, always vertical.
#' @param dist_legend Single positive numeric value that moves axis legends away in inches (first number of mgp argument of par() but in inches thus / 0.2).
#' @param box_type The bty argument of par(). Either "o", "l", "7", "c", "u", "]", the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box.
#' @param amplif_label Single positive numeric value to increase or decrease the size of the text in legends.
#' @param amplif_axis Single positive numeric value to increase or decrease the size of the scale numbers in axis.
#' @param std_x_range Single logical value. Standard range on the x-axis? TRUE (no range extend) or FALSE (4\% range extend). Controls xaxs argument of par() (TRUE is xaxs = "i", FALSE is xaxs = "r").
#' @param std_y_range Single logical value. Standard range on the y-axis? TRUE (no range extend) or FALSE (4\% range extend). Controls yaxs argument of par() (TRUE is yaxs = "i", FALSE is yaxs = "r").
#' @param cex_pt Single positive numeric value indicating the size of points in stripcharts (in inches, thus cex_pt will be thereafter / 0.2).
#' @param col_box Single character string indicating the color of boxplot.
#' @param x_nb_inter_tick Single positive integer value indicating the number of secondary ticks between main ticks on x-axis (only if not log scale). Zero means non secondary ticks.
#' @param y_nb_inter_tick Single positive integer value indicating the number of secondary ticks between main ticks on y-axis (only if not log scale). Zero means non secondary ticks.
#' @param tick_length Single proportion value indicating the length of the ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc. 0 means no tick.
#' @param sec_tick_length Single proportion value indicating the length of the secondary ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc., 0 for no ticks).
#' @param corner_text Single character string. Text to add at the top right corner of the window.
#' @param amplif_legend Single positive numeric value to increase or decrease the size of the text of legend.
#' @param corner_text_size Single positive numeric value to increase or decrease the size of the text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2.
#' @param trim_return Single logical value. Return the trimmed and non trimmed values? NULL returned for trimmed and non trimmed values if trim_method == "".
#' @param seed Single positive or null integer number used by set.seed(). Write NULL if random result is required, an integer otherwise. BEWARE: if not NULL,the function will systematically return the same result when the other parameters keep the same settings.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) required functions and related packages effectively present in local R libraries and 2) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns  An error message if at least one of the checked packages is missing in lib_path, or if at least one of the checked functions is missing in the required package,
#' A list containing:
#' 
#' - $trim_method: correspond to the trim_method argument.
#' 
#' - $trim_cutoffs: correspond to the trim_cutoffs argument.
#' 
#' - $real.trim.cutoffs: the two boundary values in the unit of the numeric vector or numeric matrix analyzed.
#' 
#' - $trimmed.values: the values outside of the trimming interval as defined in the trim_cutoffs argument.
#' 
#' - $kept.values: the values inside the trimming interval as defined in the trim_cutoffs argument.
#' @seealso \code{\link{head}}, \code{\link{tail}}
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' trim(data = c(1:100, 1:10), displayed_nb = NULL, single_value_display = FALSE, trim_method = "mean.sd", trim_cutoffs = c(0.05, 0.975), interval_scale_disp = TRUE, down_space = 0.75, left_space = 0.75, up_space = 0.3, right_space = 0.25, orient = 1, dist_legend = 0.37, box_type = "l", amplif_label = 1.25, amplif_axis = 1.25, std_x_range = TRUE, std_y_range = TRUE, cex_pt = 0.2, col_box = grDevices::hsv(0.55, 0.8, 0.8), x_nb_inter_tick = 4, y_nb_inter_tick = 0, tick_length = 0.5, sec_tick_length = 0.3, corner_text = "", amplif_legend = 1, corner_text_size = 0.75, trim_return = TRUE)
#' @importFrom saferDev arg_check
#' @export
trim <- function(
    data, 
    displayed_nb = NULL, 
    single_value_display = FALSE, 
    trim_method = "", 
    trim_cutoffs = base::c(0.05, 0.975), 
    interval_scale_disp = TRUE, 
    down_space = 0.75, 
    left_space = 0.75, 
    up_space = 0.3, 
    right_space = 0.25, 
    orient = 1, 
    dist_legend = 0.37, 
    box_type = "l", 
    amplif_label = 1.25, 
    amplif_axis = 1.25, 
    std_x_range = TRUE, 
    std_y_range = TRUE, 
    cex_pt = 0.2, 
    col_box = grDevices::hsv(h = 0.55, 
                    s = 0.8, 
                    v = 0.8, alpha = ), 
    x_nb_inter_tick = 4, 
    y_nb_inter_tick = 0, 
    tick_length = 1, 
    sec_tick_length = 0.75, 
    corner_text = "", 
    amplif_legend = 1, 
    corner_text_size = 0.75, 
    trim_return = FALSE,
    seed = NULL, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # data = c(1:100, 1:10) ; displayed_nb = NULL ; single_value_display = FALSE ; trim_method = "quantile" ; trim_cutoffs = c(0.05, 0.975) ; interval_scale_disp = TRUE ; down_space = 1 ; left_space = 1 ; up_space = 0.5 ; right_space = 0.25 ; orient = 1 ; dist_legend = 0.5 ; box_type = "l" ; amplif_label = 1 ; amplif_axis = 1 ; std_x_range = TRUE ; std_y_range = TRUE ; cex_pt = 0.1 ; col_box = grDevices::hsv(0.55, 0.8, 0.8) ; x_nb_inter_tick = 4 ; y_nb_inter_tick = 0 ; tick_length = 0.5 ; sec_tick_length = 0.3 ; corner_text = "" ; amplif_legend = 1 ; corner_text_size = 0.75 ; trim_return = TRUE ; seed = 1 ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
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
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of values if they are espression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 0)), envir = base::parent.frame(n = 1))) # names of all the arguments
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

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"), 
            " MANDATORY IN SAFER-R FUNCTIONS:\n", 
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

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
        # "displayed_nb", # inactivated because can be null
        "single_value_display", 
        "trim_method", 
        "trim_cutoffs", 
        "interval_scale_disp", 
        "down_space", 
        "left_space", 
        "up_space", 
        "right_space", 
        "orient", 
        "dist_legend", 
        "box_type", 
        "amplif_label", 
        "amplif_axis", 
        "std_x_range",
        "std_y_range", 
        "cex_pt", 
        "col_box", 
        "x_nb_inter_tick", 
        "y_nb_inter_tick", 
        "tick_length", 
        "sec_tick_length", 
        "corner_text", 
        "amplif_legend", 
        "corner_text_size", 
        "trim_return",
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
        "displayed_nb", 
        "single_value_display", 
        "trim_method", 
        "trim_cutoffs", 
        "interval_scale_disp", 
        "down_space", 
        "left_space", 
        "up_space", 
        "right_space", 
        "orient", 
        "dist_legend", 
        "box_type", 
        "amplif_label", 
        "amplif_axis", 
        "std_x_range",
        "std_y_range", 
        "cex_pt", 
        "col_box", 
        "x_nb_inter_tick", 
        "y_nb_inter_tick", 
        "tick_length", 
        "sec_tick_length", 
        "corner_text", 
        "amplif_legend", 
        "corner_text_size", 
        "trim_return",
        "seed", 
        "safer_check", 
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
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
                    base::all(x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
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
    # if(safer_check == TRUE){ # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
            if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
            }
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
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
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
        }
    }else{
        lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    # } # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = -1, envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check", # write each function preceeded by their package name
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = -1, envir = )
    # add here in the internal functions that are used in your main code (copy-paste the line above and replace .base_op_check by the name of the internal function
    # not mandatory if your function is not designed for submission to the CRAN

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
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
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object_name))
    # add as many lines as below, for each of your arguments of your function in development
    # data can be a numeric vector or a numeric matrix. Since a matrix is class c("matrix", "array"), and "vector" cannot be used as a class/typeof/mode value (see saferDev::arg_check() doc), several arg_check() calls are performed and the error is triggered only if all of them fail
    tempo1 <- saferDev::arg_check(data = data, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    tempo2 <- saferDev::arg_check(data = data, class = base::c("matrix", "array"), typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    if(tempo1$problem == TRUE & tempo2$problem == TRUE){
        tempo_cat <- base::paste0(
            arg_check_error_text,
            "data ARGUMENT MUST BE A NUMERIC VECTOR OR A NUMERIC MATRIX",
            collapse = NULL,
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo_cat)
        argum_check <- base::c(argum_check, TRUE)
        checked_arg_names <- base::c(checked_arg_names, tempo1$object_name)
    }
    if( ! base::is.null(x = displayed_nb)){
        tempo <- saferDev::arg_check(data = displayed_nb, class = NULL, typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = single_value_display, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = trim_method, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("", "mean.sd", "quantile"), all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = trim_cutoffs, class = NULL, typeof = NULL, mode = NULL, length = 2, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = interval_scale_disp, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = down_space, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = left_space, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = up_space, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = right_space, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = orient, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = dist_legend, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box_type, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("o", "l", "7", "c", "u", "]", "n"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = amplif_label, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = amplif_axis, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = std_x_range, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = std_y_range, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = cex_pt, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = col_box, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = x_nb_inter_tick, class = NULL, typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_nb_inter_tick, class = NULL, typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = tick_length, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = sec_tick_length, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = corner_text, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = amplif_legend, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = corner_text_size, class = NULL, typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = trim_return, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = seed)){
        tempo <- saferDev::arg_check(data = seed, class = NULL, typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
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
        # "trim_method", # inactivated because can be ""
        "box_type"
        # "corner_text", # inactivated because can be ""
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
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
        # nocov end
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            # nocov start
            # codecov inactivated because box_type is the only argument of tempo_arg and it is already checked by arg_check() options ("" cannot reach this block).
            tempo_cat <- base::paste0(
                error_text_start,
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"),
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
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
        tempo_random_seed <- .Random.seed
        base::on.exit(expr = base::assign(x = ".Random.seed", value = tempo_random_seed, pos = -1, envir = .GlobalEnv, inherits = FALSE, immediate = TRUE), add = TRUE, after = TRUE)
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
    if(base::all( ! base::is.finite(x = data), na.rm = FALSE)){ # base::is.finite() tests if it is not one of the values NA, NaN, Inf or -Inf
        tempo_cat <- base::paste0(error_text_start, "data ARGUMENT CANNOT CONTAIN NA AND Inf ONLY.", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    if(base::is.null(x = base::dim(x = data))){
        tempo1 <- base::is.numeric(x = data)
        tempo2 <- base::is.integer(x = data)
        if( ! (tempo1 | tempo2)){
            # nocov start
            # codecov inactivated because non numeric objects are already rejected by the arg_check() calls of the data argument above (this block is a redundant second-round checking).
            tempo_cat <- base::paste0(error_text_start, "data ARGUMENT MUST BE A NUMERIC VECTOR OR NUMERIC MATRIX.", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            # nocov end
        }
    }else{
        if( ! base::is.numeric(x = data)){
            # nocov start
            # codecov inactivated because non numeric matrices are already rejected by the arg_check() calls of the data argument above (this block is a redundant second-round checking).
            tempo_cat <- base::paste0(error_text_start, "data ARGUMENT MUST BE A NUMERIC VECTOR OR NUMERIC MATRIX.", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            # nocov end
        }
    }
    if( ! base::is.null(x = displayed_nb)){
        if(displayed_nb < 2){
            tempo_cat <- base::paste0(error_text_start, "displayed_nb ARGUMENT MUST BE A SINGLE INTEGER VALUE GREATER THAN 1 AND NOT:\n", base::paste0(displayed_nb, collapse = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    if(base::all(base::class(x = data)%in% base::c("matrix", "array"), na.rm = FALSE)){
        data <- base::as.vector(x = data, mode = "any")
    }
    na_nb <- NULL
    if(base::any(base::is.na(x = data), na.rm = FALSE)){
        na_nb <- base::sum(base::c(base::is.na(x = data)), na.rm = FALSE)
        data <- data[ ! base::is.na(x = data)]
    }
    color_cut <- grDevices::hsv(h = 0.75, s = 1, v = 1, alpha = ) # color of interval selected
    col_mean <- grDevices::hsv(h = 0.25, s = 1, v = 0.8, alpha = ) # color of interval using mean+/-sd
    col_quantile <- "orange" # color of interval using quantiles
    quantiles_selection <- base::c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99) # quantiles used in axis to help for choosing trimming cutoffs
    if(single_value_display == FALSE & base::length(x = base::unique(x = data, incomparables = FALSE)) == 1L){
        graphics::par(bty = "n", xaxt = "n", yaxt = "n", xpd = TRUE, no.readonly = FALSE)
        base::plot(x = 1, pch = 16, col = "white", xlab = "", ylab = "", y = )
        graphics::text(x = 1, y = 1, base::paste0("No graphic displayed\nBecause data made of a single different value (", base::formatC(x = base::as.double(x = base::table(data, exclude = , useNA = base::c("no", "ifany", "always"), dnn = , deparse.level = 1)), digits = NULL, width = NULL, format = NULL, flag = "", mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), ")", collapse = NULL, recycle0 = FALSE), cex = 2)
        output <- base::list(trim_method = NULL, trim_cutoffs = NULL, real.trim.cutoffs = NULL, trimmed.values = NULL, kept.values = NULL)
    }else{
        output <- base::list(trim_method = trim_method, trim_cutoffs = trim_cutoffs, real.trim.cutoffs = NULL, trimmed.values = NULL, kept.values = NULL)
        fun_rug <- function(sec_tick_length_f = sec_tick_length, x_nb_inter_tick_f = x_nb_inter_tick, y_nb_inter_tick_f = y_nb_inter_tick){
            if(x_nb_inter_tick_f > 0){
                inter_tick_unit <- (graphics::par("xaxp", no.readonly = FALSE)[2] - graphics::par("xaxp", no.readonly = FALSE)[1]) / graphics::par("xaxp", no.readonly = FALSE)[3]
                par_ini <- graphics::par(no.readonly = FALSE)[base::c("xpd", "tcl")]
                graphics::par(xpd = FALSE, no.readonly = FALSE)
                graphics::par(tcl = -graphics::par(no.readonly = FALSE)$mgp[2] * sec_tick_length_f, no.readonly = FALSE) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
                base::suppressWarnings(expr = graphics::rug(x = base::seq(graphics::par("xaxp", no.readonly = FALSE)[1] - 10 * inter_tick_unit, graphics::par("xaxp", no.readonly = FALSE)[2] + 10 * inter_tick_unit, by = inter_tick_unit / (1 + x_nb_inter_tick_f)), ticksize = NA, side = 1, lwd = 0.5, col = graphics::par("fg", no.readonly = FALSE), quiet = base::getOption(x = "warn", default = NULL) < 0), classes = "warning") # ticksize = NA to allow the use of graphics::par()$tcl value
                graphics::par(par_ini, no.readonly = FALSE)
                base::rm(par_ini, list = base::character(length = 0L), pos = -1, envir = , inherits = FALSE)
            }
            if(y_nb_inter_tick_f > 0){
                inter_tick_unit <- (graphics::par("yaxp", no.readonly = FALSE)[2] - graphics::par("yaxp", no.readonly = FALSE)[1]) / graphics::par("yaxp", no.readonly = FALSE)[3]
                par_ini <- graphics::par(no.readonly = FALSE)[base::c("xpd", "tcl")]
                graphics::par(xpd = FALSE, no.readonly = FALSE)
                graphics::par(tcl = -graphics::par(no.readonly = FALSE)$mgp[2] * sec_tick_length_f, no.readonly = FALSE) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
                base::suppressWarnings(expr = graphics::rug(x = base::seq(graphics::par("yaxp", no.readonly = FALSE)[1] - 10 * inter_tick_unit, graphics::par("yaxp", no.readonly = FALSE)[2] + 10 * inter_tick_unit, by = inter_tick_unit / (1 + y_nb_inter_tick_f)), ticksize = NA, side = 2, lwd = 0.5, col = graphics::par("fg", no.readonly = FALSE), quiet = base::getOption(x = "warn", default = NULL) < 0), classes = "warning") # ticksize = NA to allow the use of graphics::par()$tcl value
                graphics::par(par_ini, no.readonly = FALSE)
                base::rm(par_ini, list = base::character(length = 0L), pos = -1, envir = , inherits = FALSE)
            }
        }
        fun_add_cut <- function(data_f, trim_method_f = trim_method, trim_cutoffs_f = trim_cutoffs, color_cut_f = color_cut, return_f = FALSE){
            # DEBUGGING
            # data_f = data ; trim_method_f = "mean.sd"; trim_cutoffs_f = trim_cutoffs ; color_cut_f = color_cut ; return_f = TRUE
            real_trim_cutoffs_f <- NULL
            if(trim_method_f != ""){
                data_f <- base::sort(x = data_f, decreasing = FALSE)
                par_ini <- graphics::par(no.readonly = FALSE)$xpd
                graphics::par(xpd = FALSE, no.readonly = FALSE)
                if(trim_method_f == "mean.sd"){
                    real_trim_cutoffs_f <- stats::qnorm(p = trim_cutoffs_f, mean = base::mean(x = data_f, na.rm = TRUE), sd = stats::sd(x = data_f, na.rm = TRUE), lower.tail = TRUE, log.p = FALSE)
                    graphics::abline(v = stats::qnorm(p = trim_cutoffs_f, mean = base::mean(x = data_f, na.rm = TRUE), sd = stats::sd(x = data_f, na.rm = TRUE), lower.tail = TRUE, log.p = FALSE), col = color_cut_f, a = NULL, b = NULL, h = NULL, reg = NULL, coef = NULL, untf = FALSE)
                    graphics::segments(x0 = stats::qnorm(p = trim_cutoffs_f[1], mean = base::mean(x = data_f, na.rm = TRUE), sd = stats::sd(x = data_f, na.rm = TRUE), lower.tail = TRUE, log.p = FALSE), y0 = graphics::par(no.readonly = FALSE)$usr[4] * 0.75, x1 = stats::qnorm(p = trim_cutoffs_f[2], mean = base::mean(x = data_f, na.rm = TRUE), sd = stats::sd(x = data_f, na.rm = TRUE), lower.tail = TRUE, log.p = FALSE), y1 = graphics::par(no.readonly = FALSE)$usr[4] * 0.75, col = color_cut_f, lty = graphics::par("lty", no.readonly = FALSE), lwd = graphics::par("lwd", no.readonly = FALSE))
                }
                if(trim_method_f == "quantile"){
                    real_trim_cutoffs_f <- stats::quantile(x = data_f, probs = trim_cutoffs_f, type = 7, na.rm = TRUE)
                    graphics::abline(v = stats::quantile(x = data_f, probs = trim_cutoffs_f, type = 7, na.rm = TRUE), col = color_cut_f, a = NULL, b = NULL, h = NULL, reg = NULL, coef = NULL, untf = FALSE)
                    graphics::segments(x0 = stats::quantile(x = data_f, probs = trim_cutoffs_f[1], type = 7, na.rm = TRUE), y0 = graphics::par(no.readonly = FALSE)$usr[4] * 0.75, x1 = stats::quantile(x = data_f, probs = trim_cutoffs_f[2], type = 7, na.rm = TRUE), y1 = graphics::par(no.readonly = FALSE)$usr[4] * 0.75, col = color_cut_f, lty = graphics::par("lty", no.readonly = FALSE), lwd = graphics::par("lwd", no.readonly = FALSE))
                }
                graphics::par(par_ini, no.readonly = FALSE)
                if(return_f == TRUE){
                    trimmed_values_f <- data_f[data_f <= real_trim_cutoffs_f[1] | data_f >= real_trim_cutoffs_f[2]]
                    kept_values_f <- data_f[data_f > real_trim_cutoffs_f[1] & data_f < real_trim_cutoffs_f[2]]
                }
            }else{
                real_trim_cutoffs_f <- NULL
                trimmed_values_f <- NULL
                kept_values_f <- NULL
            }
            # output
            # warning output
            # end warning output
            if(return_f == TRUE){
                output <- base::list(trim_method = trim_method_f, trim_cutoffs = trim_cutoffs_f, real.trim.cutoffs = real_trim_cutoffs_f, trimmed.values = trimmed_values_f, kept.values = kept_values_f)
                base::return(output)
            }
        }
        fun_interval_scale_display <- function(data_f, col_quantile_f = col_quantile, quantiles_selection_f = quantiles_selection, col_mean_f = col_mean){ # intervals on top of graphs
            par_ini <- graphics::par(no.readonly = FALSE)[base::c("mgp", "xpd")]
            graphics::par(mgp = base::c(0.25, 0.25, 0), xpd = NA, no.readonly = FALSE)
            graphics::axis(side = 3, at = base::c(graphics::par(no.readonly = FALSE)$usr[1], graphics::par(no.readonly = FALSE)$usr[2]), labels = base::rep(x = "", 2), col = col_quantile_f, lwd.ticks = 0, tick = TRUE, line = NA, pos = NA, outer = FALSE, font = NA, lty = "solid", lwd = 1, col.ticks = NULL, hadj = NA, padj = NA, gap.axis = NA)
            graphics::par(xpd = FALSE, no.readonly = FALSE)
            graphics::axis(side = 3, at = stats::quantile(x = base::as.vector(x = data_f, mode = "any"), probs = quantiles_selection_f, type = 7, na.rm = TRUE), labels = quantiles_selection_f, col.axis = col_quantile_f, col = col_quantile_f, tick = TRUE, line = NA, pos = NA, outer = FALSE, font = NA, lty = "solid", lwd = 1, lwd.ticks = , col.ticks = NULL, hadj = NA, padj = NA, gap.axis = NA)
            graphics::par(mgp = base::c(1.75, 1.75, 1.5), xpd = NA, no.readonly = FALSE)
            graphics::axis(side = 3, at = base::c(graphics::par(no.readonly = FALSE)$usr[1], graphics::par(no.readonly = FALSE)$usr[2]), labels = base::rep(x = "", 2), col = col_mean_f, lwd.ticks = 0, tick = TRUE, line = NA, pos = NA, outer = FALSE, font = NA, lty = "solid", lwd = 1, col.ticks = NULL, hadj = NA, padj = NA, gap.axis = NA)
            graphics::par(xpd = FALSE, no.readonly = FALSE)
            graphics::axis(side = 3, at = m + s * stats::qnorm(p = quantiles_selection_f, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE), labels = base::formatC(x = base::round(x = stats::qnorm(p = quantiles_selection_f, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE), digits = 2), digits = NULL, width = NULL, format = NULL, flag = "", mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), col.axis = col_mean_f, col = col_mean_f, lwd.ticks = 1, tick = TRUE, line = NA, pos = NA, outer = FALSE, font = NA, lty = "solid", lwd = 1, col.ticks = NULL, hadj = NA, padj = NA, gap.axis = NA)
            graphics::par(par_ini, no.readonly = FALSE)
        }
        zone<-base::matrix(data = 1:4, ncol=2, nrow = 2, byrow = FALSE, dimnames = NULL) # nrow = 2 written: the default value nrow = 1 of base::matrix() is not used when nrow is missing (nrow then computed from the data length and ncol)
        graphics::layout(mat = zone, widths = , heights = , respect = FALSE)
        graphics::par(omi = base::c(0, 0, 1.5, 0), mai = base::c(down_space, left_space, up_space, right_space), las = orient, mgp = base::c(dist_legend / 0.2, 0.5, 0), xpd = FALSE, bty= box_type, cex.lab = amplif_label, cex.axis = amplif_axis, xaxs = base::ifelse(test = std_x_range, yes = "i", no = "r"), yaxs = base::ifelse(test = std_y_range, yes = "i", no = "r"), no.readonly = FALSE)
        graphics::par(tcl = -graphics::par(no.readonly = FALSE)$mgp[2] * tick_length, no.readonly = FALSE) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
        if(base::is.null(x = displayed_nb)){
            sampled_data <- base::as.vector(x = data, mode = "any")
            if(corner_text == ""){
                corner_text <- base::paste0("ALL VALUES OF THE DATASET DISPLAYED", collapse = NULL, recycle0 = FALSE)
            }else{
                corner_text <- base::paste0(corner_text, "\nALL VALUES OF THE DATASET DISPLAYED", collapse = NULL, recycle0 = FALSE)
            }
        }else{
            if(base::length(x = base::as.vector(x = data, mode = "any")) > displayed_nb){
                sampled_data <- base::sample(x = base::as.vector(x = data, mode = "any"), size = displayed_nb, replace = FALSE, prob = NULL)
                if(corner_text == ""){
                    corner_text <- base::paste0("WARNING: ONLY ", displayed_nb, " VALUES ARE DISPLAYED AMONG THE ", base::length(x = base::as.vector(x = data, mode = "any")), " VALUES OF THE DATASET ANALYZED", collapse = NULL, recycle0 = FALSE)
                }else{
                    corner_text <- base::paste0(corner_text, "\nWARNING: ONLY ", displayed_nb, " VALUES ARE DISPLAYED AMONG THE ", base::length(x = base::as.vector(x = data, mode = "any")), " VALUES OF THE DATASET ANALYZED", collapse = NULL, recycle0 = FALSE)
                }
            }else{
                sampled_data <- base::as.vector(x = data, mode = "any")
                if(corner_text == ""){
                    corner_text <- base::paste0("WARNING: THE DISPLAYED NUMBER OF VALUES PARAMETER ", base::deparse(expr = base::substitute(expr = displayed_nb, env = ), width.cutoff = 60L, backtick = , control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), " HAS BEEN SET TO ", displayed_nb, " WHICH IS ABOVE THE NUMBER OF VALUES OF THE DATASET ANALYZED -> ALL VALUES DISPLAYED", collapse = NULL, recycle0 = FALSE)
                }else{
                    corner_text <- base::paste0(corner_text, "\nWARNING: THE DISPLAYED NUMBER OF VALUES PARAMETER ", base::deparse(expr = base::substitute(expr = displayed_nb, env = ), width.cutoff = 60L, backtick = , control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), " HAS BEEN SET TO ", displayed_nb, " WHICH IS ABOVE THE NUMBER OF VALUES OF THE DATASET ANALYZED -> ALL VALUES DISPLAYED", collapse = NULL, recycle0 = FALSE)
                }
            }
        }
        if( ! base::is.null(x = na_nb)){
            # nocov start
            # codecov inactivated because corner_text is always non empty here: the displayed_nb management block above always sets it (either "ALL VALUES OF THE DATASET DISPLAYED" when displayed_nb is NULL, or a sampling/warning message).
            if(corner_text == ""){
                corner_text <- base::paste0("WARNING: NUMBER OF NA REMOVED IS ", na_nb, collapse = NULL, recycle0 = FALSE)
            }else{
                corner_text <- base::paste0("WARNING: NUMBER OF NA REMOVED IS ", na_nb, collapse = NULL, recycle0 = FALSE)
            }
            # nocov end
        }
        graphics::stripchart(x = sampled_data, method="jitter", jitter=0.4, vertical=FALSE, ylim= base::c(0.5, 1.5), group.names = "", xlab = "Value", ylab="", pch=1, cex = cex_pt / 0.2)
        fun_rug(y_nb_inter_tick_f = 0)
        graphics::boxplot(x = base::as.vector(x = data, mode = "any"), horizontal=TRUE, add=TRUE, boxwex = 0.4, staplecol = col_box, whiskcol = col_box, medcol = col_box, boxcol = col_box, range = 0, whisklty = 1)
        m <- base::mean(x = base::as.vector(x = data, mode = "any"), na.rm = TRUE)
        s <- stats::sd(x = base::as.vector(x = data, mode = "any"), na.rm = TRUE)
        graphics::segments(x0 = m, y0 = 0.8, x1 = m, y1 = 1, lwd=2, col="red", lty = graphics::par("lty", no.readonly = FALSE)) # mean 
        graphics::segments(x0 = m -1.96 * s, y0 = 0.9, x1 = m + 1.96 * s, y1 = 0.9, lwd=1, col="red", lty = graphics::par("lty", no.readonly = FALSE)) # mean 
        graph_xlim <- graphics::par(no.readonly = FALSE)$usr[1:2] # for graphics::hist() and stats::qqnorm() below
        if(interval_scale_disp == TRUE){
            fun_interval_scale_display(data_f = data)
            # nocov start
            # codecov inactivated because corner_text is always non empty here: the displayed_nb management block above always sets it (either "ALL VALUES OF THE DATASET DISPLAYED" when displayed_nb is NULL, or a sampling/warning message).
            if(corner_text == ""){
                corner_text <- base::paste0("MULTIPLYING FACTOR DISPLAYED (MEAN +/- SD) ON SCALES: ", base::paste(base::formatC(x = base::round(x = stats::qnorm(p = quantiles_selection, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE), digits = 2), digits = NULL, width = NULL, format = NULL, flag = "", mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE)[-(1:(base::length(x = quantiles_selection) - 1) / 2)], collapse = ", ", sep = " ", recycle0 = FALSE), "\nQUANTILES DISPLAYED ON SCALES: ", base::paste(quantiles_selection, collapse = ", ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            }else{
                corner_text <- base::paste0(corner_text, "\nMULTIPLYING FACTOR DISPLAYED (MEAN +/- SD) ON SCALES: ", base::paste(base::formatC(x = base::round(x = stats::qnorm(p = quantiles_selection, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE), digits = 2), digits = NULL, width = NULL, format = NULL, flag = "", mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE)[-(1:(base::length(x = quantiles_selection) - 1) / 2)], collapse = ", ", sep = " ", recycle0 = FALSE), "\nQUANTILES DISPLAYED ON SCALES: ", base::paste(quantiles_selection, collapse = ", ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            }
            # nocov end
        }
        output_tempo <- fun_add_cut(data_f = data, return_f = TRUE) # to recover real.trim.cutoffs
        if(trim_return == TRUE){
            output <- output_tempo
        }
        graphics::par(xpd = NA, no.readonly = FALSE)
        if(trim_method != ""){
            # nocov start
            # codecov inactivated because corner_text is always non empty here: the displayed_nb management block above always sets it (either "ALL VALUES OF THE DATASET DISPLAYED" when displayed_nb is NULL, or a sampling/warning message).
            if(corner_text == ""){
                corner_text <- base::paste0("SELECTED CUT-OFFS (PROPORTION): ", base::paste(trim_cutoffs, collapse = ", ", sep = " ", recycle0 = FALSE), "\nSELECTED CUT-OFFS: ", base::paste(output_tempo$real.trim.cutoffs, collapse = ", ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            }else{
                corner_text <- base::paste0(corner_text, "\nSELECTED CUT-OFFS (PROPORTION): ", base::paste(trim_cutoffs, collapse = ", ", sep = " ", recycle0 = FALSE), "\nSELECTED CUT-OFFS: ", base::paste(output_tempo$real.trim.cutoffs, collapse = ", ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            }
            # nocov end
            if(interval_scale_disp == TRUE){
                graphics::legend(x = (graphics::par("usr", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("plt", no.readonly = FALSE)[2] - graphics::par("plt", no.readonly = FALSE)[1])) * graphics::par("plt", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("omd", no.readonly = FALSE)[2] - graphics::par("omd", no.readonly = FALSE)[1])) * graphics::par("omd", no.readonly = FALSE)[1]), y = (graphics::par("usr", no.readonly = FALSE)[4] + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("plt", no.readonly = FALSE)[4] - graphics::par("plt", no.readonly = FALSE)[3])) * (1 - graphics::par("plt", no.readonly = FALSE)[4]) + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("omd", no.readonly = FALSE)[4] - graphics::par("omd", no.readonly = FALSE)[3])) * (1 - graphics::par("omd", no.readonly = FALSE)[4]) / 2), legend = base::c(base::c("min, Q1, Median, Q3, max"), "mean +/- 1.96sd", base::paste0("Trimming interval: ", base::paste0(trim_cutoffs, collapse = " , ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE), "Mean +/- sd multiplying factor", "Quantile"), yjust = 0, lty=1, col=base::c(col_box, "red", color_cut, col_mean, col_quantile), bty="n", cex = amplif_legend, fill = , border = "black", lwd = , pch = , angle = 45, density = , bg = graphics::par("bg", no.readonly = FALSE), box.lwd = graphics::par("lwd", no.readonly = FALSE), box.lty = graphics::par("lty", no.readonly = FALSE), box.col = graphics::par("fg", no.readonly = FALSE), pt.bg = NA, pt.cex = , pt.lwd = , xjust = 0, x.intersp = 1, y.intersp = 1, adj = base::c(0, 0.5), text.width = NULL, text.col = graphics::par("col", no.readonly = FALSE), text.font = NULL, merge = , trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd = , title.col = , title.adj = 0.5, title.cex = , title.font = , seg.len = 2) # fill and density written empty: graphics::legend() draws fill boxes when one of them is not missing (default value NULL not copied)
            }else{
                graphics::legend(x = (graphics::par("usr", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("plt", no.readonly = FALSE)[2] - graphics::par("plt", no.readonly = FALSE)[1])) * graphics::par("plt", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("omd", no.readonly = FALSE)[2] - graphics::par("omd", no.readonly = FALSE)[1])) * graphics::par("omd", no.readonly = FALSE)[1]), y = (graphics::par("usr", no.readonly = FALSE)[4] + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("plt", no.readonly = FALSE)[4] - graphics::par("plt", no.readonly = FALSE)[3])) * (1 - graphics::par("plt", no.readonly = FALSE)[4]) + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("omd", no.readonly = FALSE)[4] - graphics::par("omd", no.readonly = FALSE)[3])) * (1 - graphics::par("omd", no.readonly = FALSE)[4]) / 2), legend = base::c(base::c("min, Q1, Median, Q3, max"), "mean +/- 1.96sd", base::paste0("Trimming interval: ", base::paste0(trim_cutoffs, collapse = " , ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)), yjust = 0, lty=1, col=base::c(col_box, "red", color_cut), bty="n", cex = amplif_legend, y.intersp=1.25, fill = , border = "black", lwd = , pch = , angle = 45, density = , bg = graphics::par("bg", no.readonly = FALSE), box.lwd = graphics::par("lwd", no.readonly = FALSE), box.lty = graphics::par("lty", no.readonly = FALSE), box.col = graphics::par("fg", no.readonly = FALSE), pt.bg = NA, pt.cex = , pt.lwd = , xjust = 0, x.intersp = 1, adj = base::c(0, 0.5), text.width = NULL, text.col = graphics::par("col", no.readonly = FALSE), text.font = NULL, merge = , trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd = , title.col = , title.adj = 0.5, title.cex = , title.font = , seg.len = 2) # fill and density written empty: graphics::legend() draws fill boxes when one of them is not missing (default value NULL not copied)
            }
        }else{
            if(interval_scale_disp == TRUE){
                graphics::legend(x = (graphics::par("usr", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("plt", no.readonly = FALSE)[2] - graphics::par("plt", no.readonly = FALSE)[1])) * graphics::par("plt", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("omd", no.readonly = FALSE)[2] - graphics::par("omd", no.readonly = FALSE)[1])) * graphics::par("omd", no.readonly = FALSE)[1]), y = (graphics::par("usr", no.readonly = FALSE)[4] + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("plt", no.readonly = FALSE)[4] - graphics::par("plt", no.readonly = FALSE)[3])) * (1 - graphics::par("plt", no.readonly = FALSE)[4]) + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("omd", no.readonly = FALSE)[4] - graphics::par("omd", no.readonly = FALSE)[3])) * (1 - graphics::par("omd", no.readonly = FALSE)[4]) / 2), legend = base::c(base::c("min, Q1, Median, Q3, max"), "mean +/- sd", "Mean +/- sd multiplying factor", "Quantile"), yjust = 0, lty=1, col=base::c(col_box, "red", col_mean, col_quantile), bty="n", cex = amplif_legend, fill = , border = "black", lwd = , pch = , angle = 45, density = , bg = graphics::par("bg", no.readonly = FALSE), box.lwd = graphics::par("lwd", no.readonly = FALSE), box.lty = graphics::par("lty", no.readonly = FALSE), box.col = graphics::par("fg", no.readonly = FALSE), pt.bg = NA, pt.cex = , pt.lwd = , xjust = 0, x.intersp = 1, y.intersp = 1, adj = base::c(0, 0.5), text.width = NULL, text.col = graphics::par("col", no.readonly = FALSE), text.font = NULL, merge = , trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd = , title.col = , title.adj = 0.5, title.cex = , title.font = , seg.len = 2) # fill and density written empty: graphics::legend() draws fill boxes when one of them is not missing (default value NULL not copied)
            }else{
                graphics::legend(x = (graphics::par("usr", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("plt", no.readonly = FALSE)[2] - graphics::par("plt", no.readonly = FALSE)[1])) * graphics::par("plt", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("omd", no.readonly = FALSE)[2] - graphics::par("omd", no.readonly = FALSE)[1])) * graphics::par("omd", no.readonly = FALSE)[1]), y = (graphics::par("usr", no.readonly = FALSE)[4] + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("plt", no.readonly = FALSE)[4] - graphics::par("plt", no.readonly = FALSE)[3])) * (1 - graphics::par("plt", no.readonly = FALSE)[4]) + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("omd", no.readonly = FALSE)[4] - graphics::par("omd", no.readonly = FALSE)[3])) * (1 - graphics::par("omd", no.readonly = FALSE)[4]) / 2), legend = base::c(base::c("min, Q1, Median, Q3, max"), "mean +/- sd"), yjust = 0, lty=1, col=base::c(col_box, "red"), bty="n", cex = amplif_legend, y.intersp=1.25, fill = , border = "black", lwd = , pch = , angle = 45, density = , bg = graphics::par("bg", no.readonly = FALSE), box.lwd = graphics::par("lwd", no.readonly = FALSE), box.lty = graphics::par("lty", no.readonly = FALSE), box.col = graphics::par("fg", no.readonly = FALSE), pt.bg = NA, pt.cex = , pt.lwd = , xjust = 0, x.intersp = 1, adj = base::c(0, 0.5), text.width = NULL, text.col = graphics::par("col", no.readonly = FALSE), text.font = NULL, merge = , trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd = , title.col = , title.adj = 0.5, title.cex = , title.font = , seg.len = 2) # fill and density written empty: graphics::legend() draws fill boxes when one of them is not missing (default value NULL not copied)
            }
        }
        graphics::par(xpd = FALSE, xaxs = base::ifelse(test = std_x_range, yes = "i", no = "r"), yaxs = base::ifelse(test = std_y_range, yes = "i", no = "r"), no.readonly = FALSE)
        graphics::hist(x = base::as.vector(x = data, mode = "any"), main = "", xlim = graph_xlim, xlab = "Value", ylab="Density", col = grDevices::grey(level = 0.25, alpha = )) # removed: breaks = base::seq(base::min(base::as.vector(data), na.rm = TRUE), base::max(base::as.vector(data), na.rm = TRUE), length.out = base::length(base::as.vector(data)) / 10)
        graphics::abline(h = graphics::par(no.readonly = FALSE)$usr[3], a = NULL, b = NULL, v = NULL, reg = NULL, coef = NULL, untf = FALSE)
        fun_rug()
        if(interval_scale_disp == TRUE){
            fun_interval_scale_display(data_f = data)
        }
        fun_add_cut(data_f = data)
        graphics::par(xaxs = base::ifelse(test = std_x_range, yes = "i", no = "r"), no.readonly = FALSE)
        graphics::stripchart(x = base::rank(x = sampled_data, na.last = TRUE, ties.method = base::c("average", "first", "last", "random", "max", "min")), method="stack", vertical=FALSE, ylim=base::c(0.99, 1.3), group.names = "", xlab = "Rank of values", ylab="", pch=1, cex = cex_pt / 0.2)
        fun_rug(y_nb_inter_tick_f = 0)
        x_text <- graphics::par("usr", no.readonly = FALSE)[2] + (graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("plt", no.readonly = FALSE)[2] - graphics::par("plt", no.readonly = FALSE)[1]) * (1 - graphics::par("plt", no.readonly = FALSE)[2]) / 2
        y_text <- (graphics::par("usr", no.readonly = FALSE)[4] + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("plt", no.readonly = FALSE)[4] - graphics::par("plt", no.readonly = FALSE)[3])) * (1 - graphics::par("plt", no.readonly = FALSE)[4]) + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / ((graphics::par(no.readonly = FALSE)$omd[4] / 2) * ((graphics::par("plt", no.readonly = FALSE)[4] - graphics::par("plt", no.readonly = FALSE)[3])))) * (1 - graphics::par("omd", no.readonly = FALSE)[4])) # BEWARE. Here in "(graphics::par()$omd[4] / 2", division by two because there are 2 graphs staked on the y axis, and not one
        graphics::par(xpd=NA, no.readonly = FALSE)
        graphics::text(x = x_text, y = y_text, base::paste0(corner_text, collapse = NULL, recycle0 = FALSE), adj=base::c(1, 1.1), cex = corner_text_size) # text at the topright corner
        graphics::par(xpd=FALSE, no.readonly = FALSE)
        graphics::par(xaxs = base::ifelse(test = std_x_range, yes = "i", no = "r"), yaxs = base::ifelse(test = std_y_range, yes = "i", no = "r"), no.readonly = FALSE)
        stats::qqnorm(y = base::as.vector(x = sampled_data, mode = "any"), main = "", datax = TRUE, ylab = "Value", pch = 1, col = "red", cex = cex_pt / 0.2)
        fun_rug()
        if(base::diff(x = stats::quantile(x = base::as.vector(x = data, mode = "any"), probs = base::c(0.25, 0.75), na.rm = TRUE)) != 0){ # otherwise, error generated
            stats::qqline(y = base::as.vector(x = data, mode = "any"), datax = TRUE, distribution = stats::qnorm, probs = base::c(0.25, 0.75), qtype = 7)
        }
        if(interval_scale_disp == TRUE){
            fun_interval_scale_display(data_f = data)
        }
        fun_add_cut(data_f = data)
    }

    #### end main code

    #### warning output
    #### end warning output

    #### output
    if(trim_return == TRUE){
        base::return(output)
    }
    #### end output
}
