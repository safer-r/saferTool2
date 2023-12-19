#' @title segmentation
#' @description
#' If data1 is a data frame corresponding to the data set of a scatterplot (with a x column for x-axis values and a y column for the y-axis column), then fun_segmentation() delimits a frame around the dots cloud using a sliding window set by x.range.split and x.step.factor to frame the top and bottom part of the cloud, and set by y.range.split and y.step.factor to frame the left and right part of the cloud.
#' 
#' If a second data frame is provided, corresponding to the data set of a scatterplot (with a x column for x-axis values and a y column for the y-axis column), then fun_segmentation() defines the dots of this data frame, outside of the frame of the first data frame.
#' @param data1 A data frame containing a column of x-axis values and a column of y-axis values.
#' @param x1 Single character string of the data1 column name for x-axis (first column of data1 by default).
#' @param y1 Single character string of the data1 column name for y-axis (second column of data1 by default).
#' @param x.range.split Single positive non null numeric value giving the number of interval on the x value range. if x.range is the range of the dots on the x-axis, then abs(diff(x.range) / x.range.split) gives the window size. Window size decreases when range.split increases. In unit of x-axis. Write NULL if not required. At least one of the x.range.split and y.range.split must be non NULL.
#' @param x.step.factor Single positive non null numeric value giving the shift step of the window. If x.step.factor = 1, no overlap during the sliding (when the window slides from position n to position n+1, no overlap between the two positions). If x.step.factor = 2, 50% of overlap (when the window slides from position n to position n+1, the window on position n+1 overlap 50% of the window when it was on position n).
#' @param y.range.split Same as x.range.split for the y-axis. At least one of the x.range.split and y.range.split must be non NULL.
#' @param y.step.factor Same as x.step.factor for the y-axis.
#' @param error Single proportion value (from 0 to 1) of false positives (i.e., proportion of dots from data1 outside of the frame). 0.05 means 5% of the dots from data1 outside of the frame.
#' @param data2 A data frame containing a column of x-axis values and a column of y-axis values, for which outside dots of the data1 cloud has to be determined. Write NULL if not required.
#' @param x2 Single character string of the data2 column name for x-axis (first column of data2 by default). Ignored if data2 is NULL.
#' @param y2 Single character string of the data2 column name for y-axis (second column of data2 by default). Ignored if data2 is NULL.
#' @param data2.pb.dot Unknown dots are explain in the warning section above. If "signif", then the unknown dots are finally considered as significant (outside the frame). If "not.signif", then the unknown dots are finally considered as non significant (inside the frame). If "unknown", no conclusion are drawn from these dots. See the examples below. Ignored if data2 is NULL.
#' @param xy.cross.kind If data2 is non null and if both x.range.split and y.range.split are non null, which dots are finally significants? Write "&" for intersection of outside dots on x and on y. Write "|" for union of outside dots on x and on y. See the examples below.
#' @param plot Logical. Print graphs that check the frame? Ignored if data2 or x.range.split or y.range.split is NULL.
#' @param graph.in.file Single logical value. Graphs sent into a graphic device already opened? If FALSE, GUI are opened for each graph. If TRUE, no GUI are opended. The graphs are displayed on the current active graphic device. Ignored if plot is FALSE.
#' @param raster Single logical value. Dots in raster mode? If FALSE, dots from each geom_point from geom argument are in vectorial mode (bigger pdf and long to display if millions of dots). If TRUE, dots from each geom_point from geom argument are in matricial mode (smaller pdf and easy display if millions of dots, but long to generate the layer). If TRUE, the region plot will be square to avoid a bug in fun_gg_point_rast(). If TRUE, solve the transparency problem with some GUI. Ignored if plot is FALSE.
#' @param warn.print Single logical value. Print warnings at the end of the execution? No print if no warning messages.
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @returns
#' Several graphs if plot is TRUE.
#' 
#' A list containing:
#' - $data1.removed.row.nb: which rows have been removed due to NA; NaN, -Inf or Inf detection in x1 or y1 columns (NULL if no row removed).
#' - $data1.removed.rows: removed rows (NULL if no row removed).
#' - $data2.removed.row.nb: which rows have been removed due to NA; NaN, -Inf or Inf detection in x2 or y2 columns (NULL if no row removed).
#' - $data2.removed.rows: removed rows (NULL if no row removed).
#' - $hframe: x and y coordinates of the bottom and top frames for frame plotting (frame1 for the left step and frame2 for the right step).
#' - $vframe: x and y coordinates of the left and right frames for frame plotting (frame1 for the down step and frame2 for the top step).
#' - $data1.signif.dot: the significant dots of data1 (i.e., dots outside the frame). A good segmentation should not have any data1.signif.dot.
#' - $data1.non.signif.dot: the non significant dots of data1 (i.e., dots inside the frame).
#' - $data1.inconsistent.dot: see the warning section above.
#' - $data2.signif.dot: the significant dots of data2 if non NULL (i.e., dots outside the frame).
#' - $data2.non.signif.dot: the non significant dots of data2 (i.e., dots inside the frame).
#' - $data2.unknown.dot: the problematic dots of data2 (i.e., data2 dots outside of the range of data1, or data2 dots in a sliding window without data1 dots). Is systematically NULL except if argument data2.pb.dot = "unknown" and some data2 dots are in such situation. Modifying the segmentation x.range.split, x.step.factor, y.range.split, y.step.factor arguments can solve this problem.
#' - $data2.inconsistent.dot: see the warning section above.
#' - $axes: the x-axis and y-axis info.
#' - $warn: the warning messages. Use cat() for proper display. NULL if no warning.
#' @details 
#' REQUIRED PACKAGES
#' 
#' ggplot2 if plot is TRUE.
#' 
#' 
#' REQUIRED FUNCTIONS FROM THE cute PACKAGE
#' 
#' fun_check()
#' 
#' 
#' if plot is TRUE:
#' 
#' fun_pack()
#' 
#' fun_open()
#' 
#' fun_gg_palette()
#' 
#' fun_gg_scatter()
#' 
#' fun_gg_empty_graph()
#' 
#' fun_close()
#' 
#' 
#' WARNINGS
#' 
#' If dots from data2 look significant on the graph (outside the frame) but are not (not black on the last figure), this is probably because the frame is flat on the zero coordinate (no volume inside the frame at this position). Thus, no way to conclude that data2 dots here are significant. These dots are refered to as "unknown". The pb.dot argument deals with such dots.
#' 
#' Dots that are sometimes inside and outside the frame, depending on the sliding window, are treated differently: they are removed. Such dots are neither classified as "signif", "non signif" or "unknown", but as "inconsistent".
#' 
#' Unknown dots are treated as finally significant, not significant, or unknown (data2.pb.dot argument) for each x-axis and y-axis separately. Then, the union or intersection of significant dots is performed (argument xy.cross.kind). See the example section.
#' @examples
#' # example explaining the unknown and inconsistent dots, and the cross 
#' 
#' set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500), stringsAsFactors = TRUE) ; data1[5:7, 2] <- NA ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2), stringsAsFactors = TRUE) ; data2[11:13, 1] <- Inf ; set.seed(NULL) ; fun_segmentation(data1 = data1, x1 = names(data1)[1], y1 = names(data1)[2], x.range.split = 20, x.step.factor = 10, y.range.split = 23, y.step.factor = 10, error = 0, data2 = data2, x2 = names(data2)[1], y2 = names(data2)[2], data2.pb.dot = "not.signif", xy.cross.kind = "|", plot = TRUE, graph.in.file = FALSE, raster = FALSE, lib.path = NULL)
#' 
#' set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500), stringsAsFactors = TRUE) ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2), stringsAsFactors = TRUE) ; set.seed(NULL) ; fun_segmentation(data1 = data1, x1 = names(data1)[1], y1 = names(data1)[2], x.range.split = NULL, x.step.factor = 10, y.range.split = 23, y.step.factor = 10, error = 0, data2 = data2, x2 = names(data2)[1], y2 = names(data2)[2], data2.pb.dot = "unknown", xy.cross.kind = "|", plot = TRUE, graph.in.file = FALSE, raster = FALSE, lib.path = NULL)
#' 
#' set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500), stringsAsFactors = TRUE) ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2), stringsAsFactors = TRUE) ; set.seed(NULL) ; fun_segmentation(data1 = data1, x1 = names(data1)[1], y1 = names(data1)[2], x.range.split = 20, x.step.factor = 10, y.range.split = NULL, y.step.factor = 10, error = 0, data2 = data2, x2 = names(data2)[1], y2 = names(data2)[2], data2.pb.dot = "unknown", xy.cross.kind = "&", plot = TRUE, graph.in.file = FALSE, raster = FALSE, lib.path = NULL)
#' @export
fun_segmentation <- function(
    data1, 
    x1, 
    y1, 
    x.range.split = NULL, 
    x.step.factor = 10, 
    y.range.split = NULL, 
    y.step.factor = 10, 
    error = 0, 
    data2 = NULL, 
    x2 = NULL, 
    y2 = NULL, 
    data2.pb.dot = "unknown", 
    xy.cross.kind = "&", 
    plot = FALSE, 
    graph.in.file = FALSE, 
    raster = TRUE, 
    warn.print = FALSE, 
    lib.path = NULL
){
    # DEBUGGING
    # set.seed(1) ; data1 = data.frame(x = rnorm(50), y = rnorm(50), stringsAsFactors = TRUE) ; data1[5:7, 2] <- NA ; x1 = names(data1)[1] ; y1 = names(data1)[2] ; x.range.split = 5 ; x.step.factor = 10 ; y.range.split = 5 ; y.step.factor = 10 ; error = 0 ; data2 = data.frame(x = rnorm(50, 0, 2), y = rnorm(50, 0, 2), stringsAsFactors = TRUE) ; set.seed(NULL) ; x2 = names(data2)[1] ; y2 = names(data2)[2] ; data2.pb.dot = "unknown" ; xy.cross.kind = "|" ; plot = TRUE ; graph.in.file = FALSE ; raster = FALSE ; warn.print = TRUE ; lib.path = NULL
    # set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500), stringsAsFactors = TRUE) ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2), stringsAsFactors = TRUE) ; set.seed(NULL) ; x1 = names(data1)[1] ; y1 = names(data1)[2] ; x.range.split = 20 ; x.step.factor = 10 ; y.range.split = 23 ; y.step.factor = 10 ; error = 0 ; x2 = names(data2)[1] ; y2 = names(data2)[2] ; data2.pb.dot = "not.signif" ; xy.cross.kind = "|" ; plot = TRUE ; graph.in.file = FALSE ; raster = FALSE ; warn.print = TRUE ; lib.path = NULL
    # set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500), stringsAsFactors = TRUE) ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2), stringsAsFactors = TRUE) ; set.seed(NULL) ; x1 = names(data1)[1] ; y1 = names(data1)[2] ; x.range.split = 20 ; x.step.factor = 10 ; y.range.split = NULL ; y.step.factor = 10 ; error = 0 ; x2 = names(data2)[1] ; y2 = names(data2)[2] ; data2.pb.dot = "unknown" ; xy.cross.kind = "&" ; plot = TRUE ; graph.in.file = FALSE ; raster = FALSE ; warn.print = TRUE ; lib.path = NULL
    # package name
    package.name <- "cuteTool2"
    # end package name
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # package checking
    # check of lib.path
    if( ! is.null(lib.path)){
        if( ! all(typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end check of lib.path
    # check of cuteDev package using an internal function of the cuteTool2 package
    .cuteDev_package_check(lib.path = lib.path)
    # end check of cuteDev package using an internal function of the cuteTool2 package
    # cuteDev required function checking
    if(length(find("fun_pack", mode = "function")) == 0L){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nfun_pack() FUNCTION IS MISSING IN THE cuteDev PACKAGE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end other required function checking
    # check of other required packages
    fun_pack(req.package = c(
        "ggplot2"
    ), load = TRUE, lib.path = lib.path) # load = TRUE because otherwise, the "# check of the required function from the required packages" section does not work
    # end check of other required packages
    # end package checking
    # check of the required function from the required packages
    req.function <- c(
        "fun_open",
        "fun_gg_palette",
        "fun_gg_empty_graph",
        "fun_gg_scatter",
        "fun_close"
    )
    tempo <- NULL
    for(i1 in req.function){
        if(length(find(i1, mode = "function")) == 0L){
            tempo <- c(tempo, i1)
        }
    }
    if( ! is.null(tempo)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nREQUIRED cute FUNCTION", ifelse(length(tempo) > 1, "S ARE", " IS"), " MISSING IN THE R ENVIRONMENT:\n", paste0(tempo, collapse = "()\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end check of the required function from the required packages
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "data1", 
        "x1", 
        "y1"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with fun_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = data1, class = "data.frame", na.contain = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = x1, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = y1, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(x.range.split)){
        tempo <- fun_check(data = x.range.split, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(y.range.split)){
        tempo <- fun_check(data = y.range.split, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    }
    tempo <- fun_check(data = x.step.factor, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = y.step.factor, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = error, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(data2)){
        tempo <- fun_check(data = data2, class = "data.frame", na.contain = TRUE, fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(x2)){
        tempo <- fun_check(data = x2, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(y2)){
        tempo <- fun_check(data = y2, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(data2)){
        tempo <- fun_check(data = data2.pb.dot, options = c("signif", "not.signif", "unknown"), length = 1, fun.name = function.name) ; eval(ee)
    }
    if( ! (is.null(data2) | is.null(x.range.split) | is.null(y.range.split))){
        tempo <- fun_check(data = xy.cross.kind, options = c("&", "|"), length = 1, fun.name = function.name) ; eval(ee)
    }
    tempo <- fun_check(data = plot, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = warn.print, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if(plot == TRUE){
        tempo <- fun_check(data = raster, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
        tempo <- fun_check(data = graph.in.file, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    }
    # inactivated because already checked above
    # if( ! is.null(lib.path)){
    # tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    # }
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with fun_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-c(
        "data1", 
        "x1", 
        "y1", 
        # x.range.split = NULL, # inactivated because can be null
        "x.step.factor", 
        # y.range.split = NULL, # inactivated because can be null
        "y.step.factor", 
        "error", 
        # data2 = NULL, # inactivated because can be null
        # x2 = NULL, # inactivated because can be null
        # y2 = NULL, # inactivated because can be null
        "data2.pb.dot", 
        "xy.cross.kind", 
        "plot", 
        "graph.in.file", 
        "raster", 
        "warn.print", 
        # lib.path = NULL # inactivated because can be null
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    # end warning initiation
    
    # argument checking
    arg.check2 <- NULL #
    text.check2 <- NULL #
    ee2 <- expression(arg.check2 <- c(arg.check2, tempo$problem) , text.check2 <- c(text.check2, tempo$text))
    if(length(data1) < 2){
        tempo.cat <- paste0("ERROR IN ", function.name, ": data1 ARGUMENT MUST BE A DATA FRAME OF AT LEAST 2 COLUMNS")
        text.check2 <- c(text.check2, tempo.cat)
        arg.check2 <- c(arg.check2, TRUE)
    }
    if( ! (x1 %in% names(data1))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": x1 ARGUMENT MUST BE A COLUMN NAME OF data1")
        text.check2 <- c(text.check2, tempo.cat)
        arg.check2 <- c(arg.check2, TRUE)
    }else if(x1 %in% names(data1)){
        tempo <- fun_check(data = data1[, x1], data.name = "x1 COLUMN OF data1", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee2)
    }  
    if( ! (y1 %in% names(data1))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": y1 ARGUMENT MUST BE A COLUMN NAME OF data1")
        text.check2 <- c(text.check2, tempo.cat)
        arg.check2 <- c(arg.check2, TRUE)
    }else if(y1 %in% names(data1)){
        tempo <- fun_check(data = data1[, y1], data.name = "y1 COLUMN OF data1", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee2)
    }
    if(is.null(x.range.split) & is.null(y.range.split)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": AT LEAST ONE OF THE x.range.split AND y.range.split ARGUMENTS MUST BE NON NULL")
        text.check2 <- c(text.check2, tempo.cat)
        arg.check2 <- c(arg.check2, TRUE)
    }
    if( ! is.null(x.range.split)){
        if(x.range.split < 1){
            tempo.cat <- paste0("ERROR IN ", function.name, ": x.range.split ARGUMENT CANNOT BE LOWER THAN 1")
            text.check2 <- c(text.check2, tempo.cat)
            arg.check2 <- c(arg.check2, TRUE)
        }
    }
    if( ! is.null(y.range.split)){
        if(y.range.split < 1){
            tempo.cat <- paste0("ERROR IN ", function.name, ": y.range.split ARGUMENT CANNOT BE LOWER THAN 1")
            text.check2 <- c(text.check2, tempo.cat)
            arg.check2 <- c(arg.check2, TRUE)
        }
    }
    if(x.step.factor < 1){
        tempo.cat <- paste0("ERROR IN ", function.name, ": x.step.factor ARGUMENT CANNOT BE LOWER THAN 1")
        text.check2 <- c(text.check2, tempo.cat)
        arg.check2 <- c(arg.check2, TRUE)
    }
    if(y.step.factor < 1){
        tempo.cat <- paste0("ERROR IN ", function.name, ": y.step.factor ARGUMENT CANNOT BE LOWER THAN 1")
        text.check2 <- c(text.check2, tempo.cat)
        arg.check2 <- c(arg.check2, TRUE)
    }
    if( ! is.null(data2)){
        if(is.null(x2) | is.null(y2)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": x2 AND y2 ARGUMENTS CANNOT BE NULL IF data2 ARGUMENT IS NON NULL")
            text.check2 <- c(text.check2, tempo.cat)
            arg.check2 <- c(arg.check2, TRUE)
        }
        if(length(data2) < 2){
            tempo.cat <- paste0("ERROR IN ", function.name, ": data2 ARGUMENT MUST BE A DATA FRAME OF AT LEAST 2 COLUMNS")
            text.check2 <- c(text.check2, tempo.cat)
            arg.check2 <- c(arg.check2, TRUE)
        }
        if( ! is.null(x2)){
            if( ! (x2 %in% names(data2))){
                tempo.cat <- paste0("ERROR IN ", function.name, ": x2 ARGUMENT MUST BE A COLUMN NAME OF data2")
                text.check2 <- c(text.check2, tempo.cat)
                arg.check2 <- c(arg.check2, TRUE)
            }else{
                tempo <- fun_check(data = data2[, x2], data.name = "x2 COLUMN OF data2", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee2)
            }
        }
        if( ! is.null(y2)){
            if( ! (y2 %in% names(data2))){
                tempo.cat <- paste0("ERROR IN ", function.name, ": y2 ARGUMENT MUST BE A COLUMN NAME OF data2")
                text.check2 <- c(text.check2, tempo.cat)
                arg.check2 <- c(arg.check2, TRUE)
            }else{
                tempo <- fun_check(data = data2[, y2], data.name = "y2 COLUMN OF data2", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee2)
            }
        }
    }
    if(plot == TRUE){
        if(graph.in.file == TRUE & is.null(dev.list())){
            tempo.cat <- paste0("ERROR IN ", function.name, ": \ngraph.in.file PARAMETER SET TO TRUE BUT NO ACTIVE GRAPHIC DEVICE DETECTED")
            text.check2 <- c(text.check, tempo.cat)
            arg.check2 <- c(arg.check, TRUE)
        }else if(graph.in.file == TRUE & ! is.null(dev.list())){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") GRAPHS PRINTED IN THE CURRENT DEVICE (TYPE ", toupper(names(dev.cur())), ")")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    if( ! is.null(arg.check2)){
        if(any(arg.check2, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check2[arg.check2], collapse = "\n"), "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) #
        }
    }
    # end argument checking
    
    # reserved word checking
    # end reserved word checking
    # end second round of checking and data preparation
    
    # main code
    # na and Inf detection and removal (done now to be sure of the correct length of categ)
    data1.removed.row.nb <- NULL
    data1.removed.rows <- NULL
    data2.removed.row.nb <- NULL
    data2.removed.rows <- NULL
    if(any(is.na(data1[, c(x1, y1)])) | any(is.infinite(data1[, x1]), na.rm = TRUE) | any(is.infinite(data1[, y1]), na.rm = TRUE)){
        tempo.na <- unlist(lapply(lapply(c(data1[c(x1, y1)]), FUN = is.na), FUN = which))
        tempo.inf <- unlist(lapply(lapply(c(data1[c(x1, y1)]), FUN = is.infinite), FUN = which))
        data1.removed.row.nb <- sort(unique(c(tempo.na, tempo.inf)))
        if(length(data1.removed.row.nb) > 0){
            data1.removed.rows <- data1[data1.removed.row.nb, ]
        }
        if(length(data1.removed.row.nb) == nrow(data1)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": AT LEAST ONE NA, NaN, -Inf OR Inf DETECTED IN EACH ROW OF data1. FUNCTION CANNOT BE USED ON EMPTY DATA FRAME")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(length(data1.removed.row.nb) > 0){
            data1 <- data1[-data1.removed.row.nb, ]
        }
        if(nrow(data1) == 0L){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 1")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") NA, NaN, -Inf OR Inf DETECTED IN COLUMN ", paste(c(x1, y1), collapse = " "), " OF data1 AND CORRESPONDING ROWS REMOVED (SEE $data1.removed.row.nb AND $data1.removed.rows)")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }else{
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") NO NA, NaN, -Inf OR Inf DETECTED IN COLUMN ", paste(c(x1, y1), collapse = " "), " OF data1. NO ROW REMOVED")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    if( ! is.null(data2)){
        if(any(is.na(data2[, c(x2, y2)])) | any(is.infinite(data2[, x2])) | any(is.infinite(data2[, y2]))){
            tempo.na <- unlist(lapply(lapply(c(data2[c(x2, y2)]), FUN = is.na), FUN = which))
            tempo.inf <- unlist(lapply(lapply(c(data2[c(x2, y2)]), FUN = is.infinite), FUN = which))
            data2.removed.row.nb <- sort(unique(c(tempo.na, tempo.inf)))
            if(length(data2.removed.row.nb) > 0){
                data2.removed.rows <- data2[data2.removed.row.nb, ]
            }
            if(length(data2.removed.row.nb) == nrow(data2)){
                tempo.cat <- paste0("ERROR IN ", function.name, ": AT LEAST ONE NA, NaN, -Inf OR Inf DETECTED IN EACH ROW OF data2. FUNCTION CANNOT BE USED ON EMPTY DATA FRAME")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            if(length(data2.removed.row.nb) > 0){
                data2 <- data2[-data2.removed.row.nb, ]
            }
            if(nrow(data2) == 0L){
                tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 2")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") NA, NaN, -Inf OR Inf DETECTED IN COLUMN ", paste(c(x2, y2), collapse = " "), " OF data2 AND CORRESPONDING ROWS REMOVED (SEE $data2.removed.row.nb AND $data2.removed.rows)")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }else{
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") NO NA, NaN, -Inf OR Inf DETECTED IN COLUMN ", paste(c(x2, y2), collapse = " "), " OF data2. NO ROW REMOVED")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # end na and Inf detection and removal (done now to be sure of the correct length of categ)
    # row annotation (dot number)
    # data1 <- data1[ ! duplicated(data1[, c(x1, y1)]), ] # do not remove the dots that have same x and y values, because they will have different dot number -> not the same position on the matrices (so true for symmetric matrices)
    data1 <- cbind(data1, DOT_NB = 1:nrow(data1), stringsAsFactors = TRUE)
    if( ! is.null(data2)){
        # data2 <- data2[ ! duplicated(data2[, c(x2, y2)]), ] # do not remove the dots that have same x and y values, because they will have different dot number -> not the same position on the matrices (so true for symmetric matrices)
        data2 <- cbind(data2, DOT_NB = 1:nrow(data2), stringsAsFactors = TRUE)
    }
    # end row annotation (dot number)
    
    
    
    
    # Method using x unit interval 
    # may be create vector of each column to increase speed
    x.data1.l <- NULL # x coord of the y upper and lower limits defined on the data1 cloud for left step line
    x.data1.r <- NULL # x coord of the y upper and lower limits defined on the data1 cloud for right step line
    y.data1.down.limit.l <- NULL # lower limit of the data1 cloud for left step line
    y.data1.top.limit.l <- NULL # upper limit of the data1 cloud for left step line
    y.data1.down.limit.r <- NULL # lower limit of the data1 cloud for right step line
    y.data1.top.limit.r <- NULL # upper limit of the data1 cloud for left step line
    if(any(data1[, x1] %in% c(Inf, -Inf))){
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") THE data1 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE x1 COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    x.range <- range(data1[, x1], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    if(suppressWarnings(any(x.range %in% c(Inf, -Inf)))){
        tempo.cat <- paste0("ERROR IN ", function.name, " COMPUTED x.range CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(any(data1[, y1] %in% c(Inf, -Inf))){
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") THE data1 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE y1 COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    y.range <- range(data1[, y1], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    if(suppressWarnings(any(x.range %in% c(Inf, -Inf)))){
        tempo.cat <- paste0("ERROR IN ", function.name, " COMPUTED y.range CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    x.range.plot <- range(data1[, x1], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    y.range.plot <- range(data1[, y1], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    if( ! is.null(data2)){
        if(any(data2[, x2] %in% c(Inf, -Inf))){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") THE data2 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE x2 COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        x.range.plot <- range(data1[, x1], data2[, x2], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
        if(any(data2[, y2] %in% c(Inf, -Inf))){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") THE data2 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE y2 COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        y.range.plot <- range(data1[, y1], data2[, y2], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    if(suppressWarnings(any(x.range.plot %in% c(Inf, -Inf)))){
        tempo.cat <- paste0("ERROR IN ", function.name, " COMPUTED x.range.plot CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 (AND data2?) ARGUMENTS ARE NA OR Inf ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(suppressWarnings(any(y.range.plot %in% c(Inf, -Inf)))){
        tempo.cat <- paste0("ERROR IN ", function.name, " COMPUTED y.range.plot CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 (AND data2?) ARGUMENTS ARE NA OR Inf ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! is.null(x.range.split)){
        # data.frame ordering to slide the window from small to big values + sliding window definition
        data1 <- data1[order(data1[, x1], na.last = TRUE), ]
        if( ! is.null(data2)){
            data2 <- data2[order(data2[, x2], na.last = TRUE), ]
        }
        x.win.size <- abs(diff(x.range) / x.range.split) # in unit of x-axis
        step <- x.win.size / x.step.factor
        # end data.frame ordering to slide the window from small to big values + sliding window definition
        # x-axis sliding and y-axis limits of the data1 cloud -> y significant data2
        loop.nb <- ceiling((diff(x.range) - x.win.size) / step) # x.win.size + n * step covers the x range if x.win.size + n * step >= diff(x.range), thus if n >= (diff(x.range) - x.win.size) / step 
        y.outside.data1.dot.nb <- integer() # vector that will contain the selected rows numbers of data1 that are upper or lower than the frame
        y.inside.data1.dot.nb <- integer() # vector that will contain the selected rows numbers of data1 that are not upper or lower than the frame
        y.data1.median <- median(data1[, y1], na.rm = TRUE) # will be used for sliding windows without data1 in it
        if( ! is.null(data2)){
            y.outside.data2.dot.nb <- integer() # vector that will contain the selected 1D coordinates (i.e., dots) of data2 that are upper or lower than the data1 frame
            y.inside.data2.dot.nb <- integer() # vector that will contain the 1D coordinates (i.e., dots) of data2 that are not upper or lower than the data1 frame
            y.unknown.data2.dot.nb <- integer() # vector that will contain the 1D coordinates (i.e., dots) of data2 that are problematic: data2 dots outside of the range of data1, or data2 dots in a sliding window without data1 dots
            # recover data2 dots outside the range of data1
            if(any(data2[, x2] < x.range[1], na.rm = TRUE)){
                y.unknown.data2.dot.nb <- c(y.unknown.data2.dot.nb, data2$DOT_NB[data2[, x2] < x.range[1]])
                #tempo.warn & indicate the interval
            }
            if(any(data2[, x2] > x.range[2], na.rm = TRUE)){
                y.unknown.data2.dot.nb <- c(y.unknown.data2.dot.nb, data2$DOT_NB[data2[, x2] > x.range[2]])
                #tempo.warn & indicate the interval
            }
            # end recover data2 dots outside the range of data1
        }
        # loop.ini.time <- as.numeric(Sys.time())
        for(i1 in 0:(loop.nb + 1)){
            min.pos <- x.range[1] + step * i1 # lower position of the sliding window in data1
            max.pos <- min.pos + x.win.size # upper position of the sliding window in data1
            x.data1.l <- c(x.data1.l, min.pos, min.pos + step) # min.pos + step to make the steps
            x.data1.r <- c(x.data1.r, max.pos, max.pos + step) # max.pos + step to make the steps
            x.data1.dot.here <- data1[, x1] >= min.pos & data1[, x1] < max.pos # is there data1 dot present in the sliding window, considering the x axis?
            if( ! is.null(data2)){
                x.data2.dot.here <- data2[, x2] >= min.pos & data2[, x2] < max.pos # is there data2 dot present in the sliding window, considering the x axis?
            }
            # recover the data1 dots outside the frame
            if(any(x.data1.dot.here == TRUE, na.rm = TRUE)){
                tempo.y.data1.top.limit <- quantile(data1[x.data1.dot.here, y1], probs = 1 - error, na.rm = TRUE)
                tempo.y.data1.down.limit <- quantile(data1[x.data1.dot.here, y1], probs = 0 + error, na.rm = TRUE)
                y.data1.top.limit.l <- c(y.data1.top.limit.l, tempo.y.data1.top.limit, tempo.y.data1.top.limit)
                y.data1.down.limit.l <- c(y.data1.down.limit.l, tempo.y.data1.down.limit, tempo.y.data1.down.limit)
                y.data1.top.limit.r <- c(y.data1.top.limit.r, tempo.y.data1.top.limit, tempo.y.data1.top.limit)
                y.data1.down.limit.r <- c(y.data1.down.limit.r, tempo.y.data1.down.limit, tempo.y.data1.down.limit)
                y.data1.dot.signif <- ( ! ((data1[, y1] <= tempo.y.data1.top.limit) & (data1[, y1] >= tempo.y.data1.down.limit))) & x.data1.dot.here # is there data1 dot present in the sliding window, above or below the data1 limits, considering the y axis?
                y.data1.dot.not.signif <- x.data1.dot.here & ! y.data1.dot.signif
                y.outside.data1.dot.nb <- c(y.outside.data1.dot.nb, data1$DOT_NB[y.data1.dot.signif]) # recover the row number of data1
                y.outside.data1.dot.nb <- unique(y.outside.data1.dot.nb)
                y.inside.data1.dot.nb <- c(y.inside.data1.dot.nb, data1$DOT_NB[y.data1.dot.not.signif])
                y.inside.data1.dot.nb <- unique(y.inside.data1.dot.nb)
            }else{
                y.data1.top.limit.l <- c(y.data1.top.limit.l, y.data1.median, y.data1.median)
                y.data1.down.limit.l <- c(y.data1.down.limit.l, y.data1.median, y.data1.median)
                y.data1.top.limit.r <- c(y.data1.top.limit.r, y.data1.median, y.data1.median)
                y.data1.down.limit.r <- c(y.data1.down.limit.r, y.data1.median, y.data1.median)
            }
            # end recover the data1 dots outside the frame
            # recover the data2 dots outside the frame
            if( ! is.null(data2)){
                if(any(x.data1.dot.here == TRUE, na.rm = TRUE) & any(x.data2.dot.here == TRUE, na.rm = TRUE)){ 
                    y.data2.dot.signif <- ( ! ((data2[, y2] <= tempo.y.data1.top.limit) & (data2[, y2] >= tempo.y.data1.down.limit))) & x.data2.dot.here # is there data2 dot present in the sliding window, above or below the data1 limits, considering the y axis?
                    y.data2.dot.not.signif <- x.data2.dot.here & ! y.data2.dot.signif
                    y.outside.data2.dot.nb <- c(y.outside.data2.dot.nb, data2$DOT_NB[y.data2.dot.signif])
                    y.outside.data2.dot.nb <- unique(y.outside.data2.dot.nb)
                    y.inside.data2.dot.nb <- c(y.inside.data2.dot.nb, data2$DOT_NB[y.data2.dot.not.signif])
                    y.inside.data2.dot.nb <- unique(y.inside.data2.dot.nb)
                }else if(any(x.data1.dot.here == FALSE, na.rm = TRUE) & any(x.data2.dot.here == TRUE, na.rm = TRUE)){ # problem: data2 dots in the the window but no data1 dots to generates the quantiles
                    y.unknown.data2.dot.nb <- c(y.unknown.data2.dot.nb, data2$DOT_NB[x.data2.dot.here])
                    y.unknown.data2.dot.nb <- unique(y.unknown.data2.dot.nb)
                    #tempo.warn & indicate the interval
                    
                    
                    
                    
                    # tempo.warn <- paste0("FROM FUNCTION ", function.name, ": THE [", round(min.pos, 3), " ; ", round(max.pos, 3), "] INTERVAL DOES NOT CONTAIN data1 X VALUES BUT CONTAINS data2 X VALUES WHICH CANNOT BE EVALUATED.\nTHE CONCERNED data2 ROW NUMBERS ARE:\n", paste(which(x.data1.dot.here == FALSE & x.data2.dot.here == TRUE), collapse = "\n"))
                    # warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
            # end recover the data2 dots outside the frame
            # if(any(i1 == seq(1, loop.nb, 500))){
            # loop.fin.time <- as.numeric(Sys.time()) # time of process end
            # cat(paste0("COMPUTATION TIME OF LOOP ", i1, " / ", loop.nb, ": ", as.character(lubridate::seconds_to_period(round(loop.fin.time - loop.ini.time))), "\n"))
            # }
        }
        if(max.pos < x.range[2]){
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE SLIDING WINDOW HAS NOT REACHED THE MAX VALUE OF data1 ON THE X-AXIS: ", max.pos, " VERSUS ", x.range[2])
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        y.incon.data1.dot.nb.final <- unique(c(y.outside.data1.dot.nb[y.outside.data1.dot.nb %in% y.inside.data1.dot.nb], y.inside.data1.dot.nb[y.inside.data1.dot.nb %in% y.outside.data1.dot.nb])) # inconsistent dots: if a row number of y.inside.data1.dot.nb is present in y.outside.data1.dot.nb (and vice versa), it means that during the sliding, a dot has been sometime inside, sometime outside -> removed from the outside list
        y.outside.data1.dot.nb.final <- y.outside.data1.dot.nb[ ! (y.outside.data1.dot.nb %in% y.incon.data1.dot.nb.final)] # inconsistent dots removed from the outside list
        y.inside.data1.dot.nb.final <- y.inside.data1.dot.nb[ ! (y.inside.data1.dot.nb %in% y.incon.data1.dot.nb.final)] # inconsistent dots removed from the inside list
        if( ! is.null(data2)){
            # if some unknown dots are also inside, and/or outside, they are put in the inside and/or outside. Ok, because then the intersection between inside and outside is treated -> inconsistent dots
            tempo.unknown.out <- y.unknown.data2.dot.nb[y.unknown.data2.dot.nb %in% y.outside.data2.dot.nb]
            y.outside.data2.dot.nb <- unique(c(y.outside.data2.dot.nb, tempo.unknown.out)) # if a row number of y.unknown.data2.dot.nb is present in y.outside.data2.dot.nb, it is put into outside
            tempo.unknown.in <- y.unknown.data2.dot.nb[y.unknown.data2.dot.nb %in% y.inside.data2.dot.nb]
            y.inside.data2.dot.nb <- unique(c(y.inside.data2.dot.nb, tempo.unknown.in)) # if a row number of y.unknown.data2.dot.nb is present in y.inside.data2.dot.nb, it is put into inside
            y.unknown.data2.dot.nb.final <- y.unknown.data2.dot.nb[ ! (y.unknown.data2.dot.nb %in% c(y.outside.data2.dot.nb, y.inside.data2.dot.nb))] # then dots also in inside and outside are remove from unknown
            y.incon.data2.dot.nb.final <- unique(c(y.outside.data2.dot.nb[y.outside.data2.dot.nb %in% y.inside.data2.dot.nb], y.inside.data2.dot.nb[y.inside.data2.dot.nb %in% y.outside.data2.dot.nb])) # inconsistent dots: if a row number of y.inside.data2.dot.nb is present in y.outside.data2.dot.nb (and vice versa), it means that during the sliding, a dot has been sometime inside, sometime outside -> removed from the outside list
            y.outside.data2.dot.nb.final <- y.outside.data2.dot.nb[ ! (y.outside.data2.dot.nb %in% y.incon.data2.dot.nb.final)] # inconsistent dots removed from the outside list
            y.inside.data2.dot.nb.final <- y.inside.data2.dot.nb[ ! (y.inside.data2.dot.nb %in% y.incon.data2.dot.nb.final)] # inconsistent dots removed from the inside list
        }
        # end x-axis sliding and y-axis limits of the data1 cloud -> y significant data2
    }
    # end Method using x unit interval 
    
    
    
    
    # Method using y unit interval 
    y.data1.d <- NULL # y coord of the x upper and lower limits defined on the data1 cloud for down step line
    y.data1.t <- NULL # y coord of the x upper and lower limits defined on the data1 cloud for top step line
    x.data1.left.limit.d <- NULL # left limit of the data1 cloud for down step line
    x.data1.right.limit.d <- NULL # right limit of the data1 cloud for down step line
    x.data1.left.limit.t <- NULL # left limit of the data1 cloud for top step line
    x.data1.right.limit.t <- NULL # right limit of the data1 cloud for top step line
    if( ! is.null(y.range.split)){
        # data.frame ordering to slide the window from small to big values + sliding window definition
        data1 <- data1[order(data1[, y1], na.last = TRUE), ]
        if( ! is.null(data2)){
            data2 <- data2[order(data2[, y2], na.last = TRUE), ]
        }
        y.win.size <- abs(diff(y.range) / y.range.split) # in unit of y-axis
        step <- y.win.size / y.step.factor
        # end data.frame ordering to slide the window from small to big values + sliding window definition
        # y-axis sliding and x-axis limits of the data1 cloud -> x significant data2
        loop.nb <- ceiling((diff(y.range) - y.win.size) / step) # y.win.size + n * step covers the y range if y.win.size + n * step >= diff(y.range), thus if n >= (diff(y.range) - y.win.size) / step 
        x.outside.data1.dot.nb <- integer() # vector that will contain the selected rows numbers of data1 that are upper or lower than the frame
        x.inside.data1.dot.nb <- integer() # vector that will contain the selected rows numbers of data1 that are not upper or lower than the frame
        x.data1.median <- median(data1[, x1], na.rm = TRUE) # will be used for sliding window without data1 in it
        if( ! is.null(data2)){
            x.outside.data2.dot.nb <- integer() # vector that will contain the selected 1D coordinates (i.e., dots) of data2 that are upper or lower than the data1 frame
            x.inside.data2.dot.nb <- integer() # vector that will contain the 1D coordinates (i.e., dots) of data2 that are not upper or lower than the data1 frame
            x.unknown.data2.dot.nb <- integer() # vector that will contain the 1D coordinates (i.e., dots) of data2 that are problematic: data2 dots outside of the range of data1, or data2 dots in a sliding window without data1 dots
            # recover data2 dots outside the range of data1
            if(any(data2[, y2] < y.range[1], na.rm = TRUE)){
                x.unknown.data2.dot.nb <- c(x.unknown.data2.dot.nb, data2$DOT_NB[data2[, y2] < y.range[1]])
            }
            if(any(data2[, y2] > y.range[2], na.rm = TRUE)){
                x.unknown.data2.dot.nb <- c(x.unknown.data2.dot.nb, data2$DOT_NB[data2[, y2] > y.range[2]])
            }
            # end recover data2 dots outside the range of data1
        }
        # loop.ini.time <- as.numeric(Sys.time())
        for(i1 in 0:(loop.nb + 1)){
            min.pos <- y.range[1] + step * i1 # lower position of the sliding window in data1
            max.pos <- min.pos + y.win.size # upper position of the sliding window in data1
            y.data1.d <- c(y.data1.d, min.pos, min.pos + step) # min.pos + step to make the steps
            y.data1.t <- c(y.data1.t, max.pos, max.pos + step) # max.pos + step to make the steps
            y.data1.dot.here <- data1[, y1] >= min.pos & data1[, y1] < max.pos # is there data1 dot present in the sliding window, considering the y axis?
            if( ! is.null(data2)){
                y.data2.dot.here <- data2[, y2] >= min.pos & data2[, y2] < max.pos # is there data2 dot present in the sliding window, considering the y axis?
            }
            # recover the data1 dots outside the frame
            if(any(y.data1.dot.here == TRUE, na.rm = TRUE)){
                tempo.x.data1.right.limit <- quantile(data1[y.data1.dot.here, x1], probs = 1 - error, na.rm = TRUE)
                tempo.x.data1.left.limit <- quantile(data1[y.data1.dot.here, x1], probs = 0 + error, na.rm = TRUE)
                x.data1.right.limit.d <- c(x.data1.right.limit.d, tempo.x.data1.right.limit, tempo.x.data1.right.limit)
                x.data1.left.limit.d <- c(x.data1.left.limit.d, tempo.x.data1.left.limit, tempo.x.data1.left.limit)
                x.data1.right.limit.t <- c(x.data1.right.limit.t, tempo.x.data1.right.limit, tempo.x.data1.right.limit)
                x.data1.left.limit.t <- c(x.data1.left.limit.t, tempo.x.data1.left.limit, tempo.x.data1.left.limit)
                x.data1.dot.signif <- ( ! ((data1[, x1] <= tempo.x.data1.right.limit) & (data1[, x1] >= tempo.x.data1.left.limit))) & y.data1.dot.here # is there data2 dot present in the sliding window, above or below the data1 limits, considering the x axis?
                x.data1.dot.not.signif <- y.data1.dot.here & ! x.data1.dot.signif
                x.outside.data1.dot.nb <- c(x.outside.data1.dot.nb, data1$DOT_NB[x.data1.dot.signif]) # recover the row number of data1
                x.outside.data1.dot.nb <- unique(x.outside.data1.dot.nb)
                x.inside.data1.dot.nb <- c(x.inside.data1.dot.nb, data1$DOT_NB[x.data1.dot.not.signif])
                x.inside.data1.dot.nb <- unique(x.inside.data1.dot.nb)
            }else{
                x.data1.right.limit.d <- c(x.data1.right.limit.d, x.data1.median, x.data1.median)
                x.data1.left.limit.d <- c(x.data1.left.limit.d, x.data1.median, x.data1.median)
                x.data1.right.limit.t <- c(x.data1.right.limit.t, x.data1.median, x.data1.median)
                x.data1.left.limit.t <- c(x.data1.left.limit.t, x.data1.median, x.data1.median)
            }
            # end recover the data1 dots outside the frame
            # recover the data2 dots outside the frame
            if( ! is.null(data2)){
                if(any(y.data1.dot.here == TRUE, na.rm = TRUE) & any(y.data2.dot.here == TRUE, na.rm = TRUE)){ 
                    x.data2.dot.signif <- ( ! ((data2[, x2] <= tempo.x.data1.right.limit) & (data2[, x2] >= tempo.x.data1.left.limit))) & y.data2.dot.here # is there data2 dot present in the sliding window, above or below the data1 limits, considering the x axis?
                    x.data2.dot.not.signif <- y.data2.dot.here & ! x.data2.dot.signif
                    x.outside.data2.dot.nb <- c(x.outside.data2.dot.nb, data2$DOT_NB[x.data2.dot.signif])
                    x.outside.data2.dot.nb <- unique(x.outside.data2.dot.nb)
                    x.inside.data2.dot.nb <- c(x.inside.data2.dot.nb, data2$DOT_NB[x.data2.dot.not.signif])
                    x.inside.data2.dot.nb <- unique(x.inside.data2.dot.nb)
                }else if(any(y.data1.dot.here == FALSE, na.rm = TRUE) & any(y.data2.dot.here == TRUE, na.rm = TRUE)){ # recover the data2 dots outside the range of the data1 cloud
                    x.unknown.data2.dot.nb <- c(x.unknown.data2.dot.nb, data2$DOT_NB[y.data2.dot.here])
                    x.unknown.data2.dot.nb <- unique(x.unknown.data2.dot.nb)
                    
                    
                    
                    # tempo.warn <- paste0("FROM FUNCTION ", function.name, ": THE [", round(min.pos, 3), " ; ", round(max.pos, 3), "] INTERVAL DOES NOT CONTAIN data1 Y VALUES BUT CONTAINS data2 Y VALUES WHICH CANNOT BE EVALUATED.\nTHE CONCERNED data2 ROW NUMBERS ARE:\n", paste(which(y.data1.dot.here == FALSE & y.data2.dot.here == TRUE), collapse = "\n"))
                    # warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
            # end recover the data2 dots outside the frame
            # if(any(i1 == seq(1, loop.nb, 500))){
            # loop.fin.time <- as.numeric(Sys.time()) # time of process end
            # cat(paste0("COMPUTATION TIME OF LOOP ", i1, " / ", loop.nb, ": ", as.character(lubridate::seconds_to_period(round(loop.fin.time - loop.ini.time))), "\n"))
            # }
        }
        if(max.pos < y.range[2]){
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE SLIDING WINDOW HAS NOT REACHED THE MAX VALUE OF data1 ON THE Y-AXIS: ", max.pos, " VERSUS ", y.range[2])
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        x.incon.data1.dot.nb.final <- unique(c(x.outside.data1.dot.nb[x.outside.data1.dot.nb %in% x.inside.data1.dot.nb], x.inside.data1.dot.nb[x.inside.data1.dot.nb %in% x.outside.data1.dot.nb])) # inconsistent dots: if a row number of x.inside.data1.dot.nb is present in x.outside.data1.dot.nb (and vice versa), it means that during the sliding, a dot has been sometime inside, sometime outside -> removed from the outside list
        x.outside.data1.dot.nb.final <- x.outside.data1.dot.nb[ ! (x.outside.data1.dot.nb %in% x.incon.data1.dot.nb.final)] # inconsistent dots removed from the outside list
        x.inside.data1.dot.nb.final <- x.inside.data1.dot.nb[ ! (x.inside.data1.dot.nb %in% x.incon.data1.dot.nb.final)] # inconsistent dots removed from the inside list
        if( ! is.null(data2)){
            # if some unknown dots are also inside, and/or outside, they are put in the inside and/or outside. Ok, because then the intersection between inside and outside is treated -> inconsistent dots
            tempo.unknown.out <- x.unknown.data2.dot.nb[x.unknown.data2.dot.nb %in% x.outside.data2.dot.nb]
            x.outside.data2.dot.nb <- unique(c(x.outside.data2.dot.nb, tempo.unknown.out)) # if a row number of x.unknown.data2.dot.nb is present in x.outside.data2.dot.nb, it is put into outside
            tempo.unknown.in <- x.unknown.data2.dot.nb[x.unknown.data2.dot.nb %in% x.inside.data2.dot.nb]
            x.inside.data2.dot.nb <- unique(c(x.inside.data2.dot.nb, tempo.unknown.in)) # if a row number of x.unknown.data2.dot.nb is present in x.inside.data2.dot.nb, it is put into inside
            x.unknown.data2.dot.nb.final <- x.unknown.data2.dot.nb[ ! (x.unknown.data2.dot.nb %in% c(x.outside.data2.dot.nb, x.inside.data2.dot.nb))] # then dots also in inside and outside are remove from unknown
            x.incon.data2.dot.nb.final <- unique(c(x.outside.data2.dot.nb[x.outside.data2.dot.nb %in% x.inside.data2.dot.nb], x.inside.data2.dot.nb[x.inside.data2.dot.nb %in% x.outside.data2.dot.nb])) # inconsistent dots: if a row number of x.inside.data2.dot.nb is present in x.outside.data2.dot.nb (and vice versa), it means that during the sliding, a dot has been sometime inside, sometime outside -> removed from the outside list
            x.outside.data2.dot.nb.final <- x.outside.data2.dot.nb[ ! (x.outside.data2.dot.nb %in% x.incon.data2.dot.nb.final)] # inconsistent dots removed from the outside list
            x.inside.data2.dot.nb.final <- x.inside.data2.dot.nb[ ! (x.inside.data2.dot.nb %in% x.incon.data2.dot.nb.final)] # inconsistent dots removed from the inside list
        }
        # end y-axis sliding and x-axis limits of the data1 cloud -> x significant data2
    }
    # end Method using y unit interval 
    
    
    
    # recovering the frame coordinates
    hframe = rbind(
        data.frame(
            x = if(is.null(x.data1.l)){NULL}else{x.data1.l}, 
            y = if(is.null(x.data1.l)){NULL}else{y.data1.down.limit.l}, 
            kind = if(is.null(x.data1.l)){NULL}else{"down.frame1"},
            stringsAsFactors = TRUE
        ), 
        data.frame(
            x = if(is.null(x.data1.r)){NULL}else{x.data1.r}, 
            y = if(is.null(x.data1.r)){NULL}else{y.data1.down.limit.r}, 
            kind = if(is.null(x.data1.r)){NULL}else{"down.frame2"},
            stringsAsFactors = TRUE
        ), 
        data.frame(
            x = if(is.null(x.data1.l)){NULL}else{x.data1.l}, 
            y = if(is.null(x.data1.l)){NULL}else{y.data1.top.limit.l}, 
            kind = if(is.null(x.data1.l)){NULL}else{"top.frame1"},
            stringsAsFactors = TRUE
        ), 
        data.frame(
            x = if(is.null(x.data1.r)){NULL}else{x.data1.r}, 
            y = if(is.null(x.data1.r)){NULL}else{y.data1.top.limit.r}, 
            kind = if(is.null(x.data1.r)){NULL}else{"top.frame2"},
            stringsAsFactors = TRUE
        ), 
        stringsAsFactors = TRUE
    )
    vframe = rbind(
        data.frame(
            x = if(is.null(y.data1.d)){NULL}else{x.data1.left.limit.d}, 
            y = if(is.null(y.data1.d)){NULL}else{y.data1.d}, 
            kind = if(is.null(y.data1.d)){NULL}else{"left.frame1"},
            stringsAsFactors = TRUE
        ), 
        data.frame(
            x = if(is.null(y.data1.t)){NULL}else{x.data1.left.limit.t}, 
            y = if(is.null(y.data1.t)){NULL}else{y.data1.t}, 
            kind = if(is.null(y.data1.t)){NULL}else{"left.frame2"},
            stringsAsFactors = TRUE
        ), 
        data.frame(
            x = if(is.null(y.data1.d)){NULL}else{x.data1.right.limit.d}, 
            y = if(is.null(y.data1.d)){NULL}else{y.data1.d}, 
            kind = if(is.null(y.data1.d)){NULL}else{"right.frame1"},
            stringsAsFactors = TRUE
        ),
        data.frame(
            x = if(is.null(y.data1.t)){NULL}else{x.data1.right.limit.t}, 
            y = if(is.null(y.data1.t)){NULL}else{y.data1.t}, 
            kind = if(is.null(y.data1.t)){NULL}else{"right.frame2"},
            stringsAsFactors = TRUE
        ), 
        stringsAsFactors = TRUE
    )
    # end recovering the frame coordinates
    # recovering the dot coordinates
    data1.signif.dot <- NULL
    data1.non.signif.dot <- NULL
    data1.incon.dot <- NULL
    data2.signif.dot <- NULL
    data2.non.signif.dot <- NULL
    data2.unknown.dot <- NULL
    data2.incon.dot <- NULL
    if(( ! is.null(x.range.split)) & ( ! is.null(y.range.split))){
        # inconsistent dots recovery 
        if(length(unique(c(x.incon.data1.dot.nb.final, y.incon.data1.dot.nb.final))) > 0){
            data1.incon.dot <- data1[data1$DOT_NB %in% unique(c(x.incon.data1.dot.nb.final, y.incon.data1.dot.nb.final)), ] # if a dot in inconsistent in x or y -> classified as inconsistent (so unique() used)
            # removal of the inconsistent dot in the other classifications
            x.inside.data1.dot.nb.final <- x.inside.data1.dot.nb.final[ ! x.inside.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
            y.inside.data1.dot.nb.final <- y.inside.data1.dot.nb.final[ ! y.inside.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
            x.outside.data1.dot.nb.final <- x.outside.data1.dot.nb.final[ ! x.outside.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
            y.outside.data1.dot.nb.final <- y.outside.data1.dot.nb.final[ ! y.outside.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
            x.unknown.data1.dot.nb.final <- x.unknown.data1.dot.nb.final[ ! x.unknown.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
            y.unknown.data1.dot.nb.final <- y.unknown.data1.dot.nb.final[ ! y.unknown.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
            # end removal of the inconsistent dot in the other classifications
        }
        if( ! is.null(data2)){
            if(length(unique(c(x.incon.data2.dot.nb.final, y.incon.data2.dot.nb.final))) > 0){
                data2.incon.dot <- data2[data2$DOT_NB %in% unique(c(x.incon.data2.dot.nb.final, y.incon.data2.dot.nb.final)), ]
                # removal of the inconsistent dot in the other classifications
                x.inside.data2.dot.nb.final <- x.inside.data2.dot.nb.final[ ! x.inside.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
                y.inside.data2.dot.nb.final <- y.inside.data2.dot.nb.final[ ! y.inside.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
                x.outside.data2.dot.nb.final <- x.outside.data2.dot.nb.final[ ! x.outside.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
                y.outside.data2.dot.nb.final <- y.outside.data2.dot.nb.final[ ! y.outside.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
                x.unknown.data2.dot.nb.final <- x.unknown.data2.dot.nb.final[ ! x.unknown.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
                y.unknown.data2.dot.nb.final <- y.unknown.data2.dot.nb.final[ ! y.unknown.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
                # end removal of the inconsistent dot in the other classifications
            }
        }
        # end inconsistent dots recovery 
        # unknown dots recovery 
        if( ! is.null(data2)){
            if(data2.pb.dot == "signif"){
                x.outside.data2.dot.nb.final <- unique(c(x.outside.data2.dot.nb.final, x.unknown.data2.dot.nb.final))
                x.inside.data2.dot.nb.final <- x.inside.data2.dot.nb.final[ ! x.inside.data2.dot.nb.final %in% x.unknown.data2.dot.nb.final] # remove x.unknown.data2.dot.nb.final from x.inside.data2.dot.nb.final
                y.outside.data2.dot.nb.final <- unique(c(y.outside.data2.dot.nb.final, y.unknown.data2.dot.nb.final))
                y.inside.data2.dot.nb.final <- y.inside.data2.dot.nb.final[ ! y.inside.data2.dot.nb.final %in% y.unknown.data2.dot.nb.final] # remove y.unknown.data2.dot.nb.final from y.inside.data2.dot.nb.final
                x.unknown.data2.dot.nb.final <- NULL
                y.unknown.data2.dot.nb.final <- NULL
                data2.unknown.dot <- NULL
            }else if(data2.pb.dot == "not.signif"){
                x.inside.data2.dot.nb.final <- unique(c(x.inside.data2.dot.nb.final, x.unknown.data2.dot.nb.final))
                x.outside.data2.dot.nb.final <- x.outside.data2.dot.nb.final[ ! x.outside.data2.dot.nb.final %in% x.unknown.data2.dot.nb.final] # remove x.unknown.data2.dot.nb.final from x.outside.data2.dot.nb.final
                y.inside.data2.dot.nb.final <- unique(c(y.inside.data2.dot.nb.final, y.unknown.data2.dot.nb.final))
                y.outside.data2.dot.nb.final <- y.outside.data2.dot.nb.final[ ! y.outside.data2.dot.nb.final %in% y.unknown.data2.dot.nb.final] # remove y.unknown.data2.dot.nb.final from y.outside.data2.dot.nb.final
                x.unknown.data2.dot.nb.final <- NULL
                y.unknown.data2.dot.nb.final <- NULL
                data2.unknown.dot <- NULL
            }else if(data2.pb.dot == "unknown"){
                if(length(unique(c(x.unknown.data2.dot.nb.final, y.unknown.data2.dot.nb.final))) > 0){
                    data2.unknown.dot <- data2[data2$DOT_NB %in% unique(c(x.unknown.data2.dot.nb.final, y.unknown.data2.dot.nb.final)), ] # if a dot in unknown in x or y -> classified as unknown (so unique() used)
                    x.outside.data2.dot.nb.final <- x.outside.data2.dot.nb.final[ ! x.outside.data2.dot.nb.final %in% data2.unknown.dot$DOT_NB] # remove x.unknown.data2.dot.nb.final from x.outside.data2.dot.nb.final
                    x.inside.data2.dot.nb.final <- x.inside.data2.dot.nb.final[ ! x.inside.data2.dot.nb.final %in% data2.unknown.dot$DOT_NB] # remove x.unknown.data2.dot.nb.final from x.inside.data2.dot.nb.final
                    y.outside.data2.dot.nb.final <- y.outside.data2.dot.nb.final[ ! y.outside.data2.dot.nb.final %in% data2.unknown.dot$DOT_NB] # remove y.unknown.data2.dot.nb.final from y.outside.data2.dot.nb.final
                    y.inside.data2.dot.nb.final <- y.inside.data2.dot.nb.final[ ! y.inside.data2.dot.nb.final %in% data2.unknown.dot$DOT_NB] # remove y.unknown.data2.dot.nb.final from y.inside.data2.dot.nb.final
                }
            }else{
                tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 3")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
        # end unknown dots recovery 
        # sign and non sign dot recovery
        if(xy.cross.kind == "|"){ # here the problem is to deal with significant dots depending on x and y. Thus I start with that, recover dots finally non significant in outside and put them in inside (when &), and remove from inside the dots in outside
            if(length(unique(c(x.outside.data1.dot.nb.final, y.outside.data1.dot.nb.final))) > 0){
                tempo.outside <- unique(c(x.outside.data1.dot.nb.final, y.outside.data1.dot.nb.final)) # union so unique() used
                tempo.inside <- unique(c(x.inside.data1.dot.nb.final, y.inside.data1.dot.nb.final))
                tempo.inside <- tempo.inside[ ! tempo.inside %in% tempo.outside]
                data1.signif.dot <- data1[data1$DOT_NB %in% tempo.outside, ]
                data1.non.signif.dot <- data1[data1$DOT_NB %in% tempo.inside, ]
            }else{
                data1.non.signif.dot <- data1[unique(c(x.inside.data1.dot.nb.final, y.inside.data1.dot.nb.final)), ] # if no outside dots, I recover all the inside dots and that's it
            }
        }else if(xy.cross.kind == "&"){
            if(sum(x.outside.data1.dot.nb.final %in% y.outside.data1.dot.nb.final) > 0){ # that is intersection
                tempo.outside <- unique(x.outside.data1.dot.nb.final[x.outside.data1.dot.nb.final %in% y.outside.data1.dot.nb.final]) # intersection
                tempo.outside.removed <- unique(c(x.outside.data1.dot.nb.final, y.outside.data1.dot.nb.final))[ ! unique(c(x.outside.data1.dot.nb.final, y.outside.data1.dot.nb.final)) %in% tempo.outside]
                tempo.inside <- unique(c(x.inside.data1.dot.nb.final, y.inside.data1.dot.nb.final))
                data1.signif.dot <- data1[data1$DOT_NB %in% tempo.outside, ]
                data1.non.signif.dot <- data1[data1$DOT_NB %in% tempo.inside, ]
            }else{
                data1.non.signif.dot <- data1[unique(c(x.inside.data1.dot.nb.final, y.inside.data1.dot.nb.final)), ] # if no outside dots, I recover all the inside dots and that's it
            }
        }else{
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 4")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if( ! is.null(data2)){
            if(xy.cross.kind == "|"){ # here the problem is to deal with significant dots depending on x and y. Thus I start with that, recover dots finally non significant in outside and put them in inside (when &), and remove from inside the dots in outside
                if(length(unique(c(x.outside.data2.dot.nb.final, y.outside.data2.dot.nb.final))) > 0){
                    tempo.outside <- unique(c(x.outside.data2.dot.nb.final, y.outside.data2.dot.nb.final)) # union so unique() used
                    tempo.inside <- unique(c(x.inside.data2.dot.nb.final, y.inside.data2.dot.nb.final))
                    tempo.inside <- tempo.inside[ ! tempo.inside %in% tempo.outside]
                    data2.signif.dot <- data2[data2$DOT_NB %in% tempo.outside, ]
                    data2.non.signif.dot <- data2[data2$DOT_NB %in% tempo.inside, ]
                }else{
                    data2.non.signif.dot <- data2[unique(c(x.inside.data2.dot.nb.final, y.inside.data2.dot.nb.final)), ] # if no outside dots, I recover all the inside dots and that's it
                }
            }else if(xy.cross.kind == "&"){
                if(sum(x.outside.data2.dot.nb.final %in% y.outside.data2.dot.nb.final) > 0){ # that is intersection
                    tempo.outside <- unique(x.outside.data2.dot.nb.final[x.outside.data2.dot.nb.final %in% y.outside.data2.dot.nb.final]) # intersection
                    tempo.outside.removed <- unique(c(x.outside.data2.dot.nb.final, y.outside.data2.dot.nb.final))[ ! unique(c(x.outside.data2.dot.nb.final, y.outside.data2.dot.nb.final)) %in% tempo.outside]
                    tempo.inside <- unique(c(x.inside.data2.dot.nb.final, y.inside.data2.dot.nb.final))
                    data2.signif.dot <- data2[data2$DOT_NB %in% tempo.outside, ]
                    data2.non.signif.dot <- data2[data2$DOT_NB %in% tempo.inside, ]
                }else{
                    data2.non.signif.dot <- data2[unique(c(x.inside.data2.dot.nb.final, y.inside.data2.dot.nb.final)), ] # if no outside dots, I recover all the inside dots and that's it
                }
            }else{
                tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 5")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
        # end sign and non sign dot recovery
    }else if(( ! is.null(x.range.split)) & is.null(y.range.split)){
        # inconsistent dots recovery 
        if(length(y.incon.data1.dot.nb.final) > 0){
            data1.incon.dot <- data1[data1$DOT_NB %in% y.incon.data1.dot.nb.final, ]
        }
        if( ! is.null(data2)){
            if(length(y.incon.data2.dot.nb.final) > 0){
                data2.incon.dot <- data2[data2$DOT_NB %in% y.incon.data2.dot.nb.final, ]
            }
        }# end inconsistent dots recovery 
        # unknown dots recovery 
        if( ! is.null(data2)){
            if(data2.pb.dot == "signif"){
                y.outside.data2.dot.nb.final <- unique(c(y.outside.data2.dot.nb.final, y.unknown.data2.dot.nb.final))
            }else if(data2.pb.dot == "not.signif"){
                y.inside.data2.dot.nb.final <- unique(c(y.inside.data2.dot.nb.final, y.unknown.data2.dot.nb.final))
            }else if(data2.pb.dot == "unknown"){
                if(length(y.unknown.data2.dot.nb.final) > 0){
                    data2.unknown.dot <- data2[data2$DOT_NB %in% y.unknown.data2.dot.nb.final, ]
                }
            }else{
                tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 6")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
        # end unknown dots recovery 
        # sign and non sign dot recovery
        if(length(y.outside.data1.dot.nb.final) > 0){
            data1.signif.dot <- data1[data1$DOT_NB %in% y.outside.data1.dot.nb.final, ]
        }
        if(length(y.inside.data1.dot.nb.final) > 0){
            data1.non.signif.dot <- data1[data1$DOT_NB %in% y.inside.data1.dot.nb.final, ]
        }
        if( ! is.null(data2)){
            if(length(y.outside.data2.dot.nb.final) > 0){
                data2.signif.dot <- data2[data2$DOT_NB %in% y.outside.data2.dot.nb.final, ]
            }
            if(length(y.inside.data2.dot.nb.final) > 0){
                data2.non.signif.dot <- data2[data2$DOT_NB %in% y.inside.data2.dot.nb.final, ]
            }
        }
        # end sign and non sign dot recovery
    }else if(is.null(x.range.split) & ( ! is.null(y.range.split))){
        # inconsistent dots recovery 
        if(length(x.incon.data1.dot.nb.final) > 0){
            data1.incon.dot <- data1[data1$DOT_NB %in% x.incon.data1.dot.nb.final, ]
        }
        if( ! is.null(data2)){
            if(length(x.incon.data2.dot.nb.final) > 0){
                data2.incon.dot <- data2[data2$DOT_NB %in% x.incon.data2.dot.nb.final, ]
            }
        }# end inconsistent dots recovery 
        # unknown dots recovery 
        if( ! is.null(data2)){
            if(data2.pb.dot == "signif"){
                x.outside.data2.dot.nb.final <- unique(c(x.outside.data2.dot.nb.final, x.unknown.data2.dot.nb.final))
            }else if(data2.pb.dot == "not.signif"){
                x.inside.data2.dot.nb.final <- unique(c(x.inside.data2.dot.nb.final, x.unknown.data2.dot.nb.final))
            }else if(data2.pb.dot == "unknown"){
                if(length(x.unknown.data2.dot.nb.final) > 0){
                    data2.unknown.dot <- data2[data2$DOT_NB %in% x.unknown.data2.dot.nb.final, ]
                }
            }else{
                tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 7")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
        # end unknown dots recovery 
        # sign and non sign dot recovery
        if(length(x.outside.data1.dot.nb.final) > 0){
            data1.signif.dot <- data1[data1$DOT_NB %in% x.outside.data1.dot.nb.final, ]
        }
        if(length(x.inside.data1.dot.nb.final) > 0){
            data1.non.signif.dot <- data1[data1$DOT_NB %in% x.inside.data1.dot.nb.final, ]
        }
        if( ! is.null(data2)){
            if(length(x.outside.data2.dot.nb.final) > 0){
                data2.signif.dot <- data2[data2$DOT_NB %in% x.outside.data2.dot.nb.final, ]
            }
            if(length(x.inside.data2.dot.nb.final) > 0){
                data2.non.signif.dot <- data2[data2$DOT_NB %in% x.inside.data2.dot.nb.final, ]
            }
        }
        # end sign and non sign dot recovery
    }
    # end recovering the dot coordinates
    # verif
    if(any(data1.signif.dot$DOT_NB %in% data1.non.signif.dot$DOT_NB)){
        tempo.cat <- paste0("ERROR IN ", FUNCTION.NAME, ": CODE INCONSISTENCY 8")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(any(data1.non.signif.dot$DOT_NB %in% data1.signif.dot$DOT_NB)){
        tempo.cat <- paste0("ERROR IN ", FUNCTION.NAME, ": CODE INCONSISTENCY 9")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(any(data1.signif.dot$DOT_NB %in% data1.incon.dot$DOT_NB)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 10")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(any(data1.incon.dot$DOT_NB %in% data1.signif.dot$DOT_NB)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 11")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(any(data1.non.signif.dot$DOT_NB %in% data1.incon.dot$DOT_NB)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 12")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(any(data1.incon.dot$DOT_NB %in% data1.non.signif.dot$DOT_NB)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 13")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! is.null(data2)){
        if(any(data2.signif.dot$DOT_NB %in% data2.non.signif.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 14")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.non.signif.dot$DOT_NB %in% data2.signif.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 15")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.signif.dot$DOT_NB %in% data2.unknown.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 16")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.unknown.dot$DOT_NB %in% data2.signif.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 17")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.signif.dot$DOT_NB %in% data2.incon.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 18")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.incon.dot$DOT_NB %in% data2.signif.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 19")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.non.signif.dot$DOT_NB %in% data2.unknown.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 20")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.unknown.dot$DOT_NB %in% data2.non.signif.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 21")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.non.signif.dot$DOT_NB %in% data2.incon.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 22")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.incon.dot$DOT_NB %in% data2.non.signif.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 23")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.unknown.dot$DOT_NB %in% data2.incon.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 24")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(data2.incon.dot$DOT_NB %in% data2.unknown.dot$DOT_NB)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 25")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end verif
    # plot
    # recovering the axes data whatever plot or not
    if(is.null(data2)){
        axes <- fun_gg_scatter(data1 = list(data1), x = list(x1), y = list(y1), categ = list(NULL), color = list(fun_gg_palette(2)[2]), geom = list("geom_point"), alpha = list(0.5), x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, plot = FALSE, return = TRUE)$axes
    }else{
        axes <- fun_gg_scatter(data1 = list(data1, data2), x = list(x1, x2), y = list(y1, y2), categ = list(NULL, NULL), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1]), geom = list("geom_point", "geom_point"), alpha = list(0.5, 0.5), x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, plot = FALSE, return = TRUE)$axes
    }
    # end recovering the axes data whatever plot or not
    if(plot == TRUE){
        # add a categ for plot legend
        tempo.df.name <- c("data1", "data1.signif.dot", "data1.incon.dot", "data2", "data2.signif.dot", "data2.unknown.dot", "data2.incon.dot")
        tempo.class.name <- c("data1", "data1", "data1", "data2", "data2", "data2", "data2")
        for(i2 in 1:length(tempo.df.name)){
            if( ! is.null(get(tempo.df.name[i2], env = sys.nframe(), inherit = FALSE))){
                assign(tempo.df.name[i2], data.frame(get(tempo.df.name[i2], env = sys.nframe(), inherit = FALSE), kind = tempo.class.name[i2]),
                       stringsAsFactors = TRUE)
            }
        }
        # end add a categ for plot legend
        if(( ! is.null(x.range.split)) & ( ! is.null(y.range.split))){
            if(graph.in.file == FALSE){
                fun_open(pdf = FALSE)
            }
            tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, vframe), x = list(x1, "x", "x"), y = list(y1, "y", "y"), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5), title = "DATA1", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
            if( ! is.null(tempo.graph$warn)){
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            if( ! is.null(data1.signif.dot)){
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, vframe, data1.signif.dot), x = list(x1, "x", "x", x1), y = list(y1, "y", "y", y1), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME" , "VERT FRAME", "SIGNIF DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2), "black"), geom = list("geom_point", "geom_path", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA1 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                if( ! is.null(tempo.graph$warn)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }else{
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA1 SIGNIFICANT DOTS")
            }
            if( ! is.null(data1.incon.dot)){
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, vframe, data1.incon.dot), x = list(x1, "x", "x", x1), y = list(y1, "y", "y", y1), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME" , "VERT FRAME", "INCONSISTENT DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2), fun_gg_palette(7)[6]), geom = list("geom_point", "geom_path", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA1 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                if( ! is.null(tempo.graph$warn)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }else{
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1\nINCONSISTENT DOTS", text.size = 8, title = "DATA1 + DATA1 INCONSISTENT DOTS")
            }
            if( ! is.null(data2)){
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, hframe , vframe), x = list(x1, x2, "x", "x"), y = list(y1, y2, "y", "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                if( ! is.null(tempo.graph$warn)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
                if( ! is.null(data2.signif.dot)){
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.signif.dot, hframe , vframe), x = list(x1, x2, x2, "x", "x"), y = list(y1, y2, y2, "y", "y"), categ = list("kind", "kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "SIGNIF DOTS", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], "black", rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                    if( ! is.null(tempo.graph$warn)){
                        warn.count <- warn.count + 1
                        tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else{
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS")
                }
                if( ! is.null(data2.incon.dot)){
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.incon.dot, hframe , vframe), x = list(x1, x2, x2, "x", "x"), y = list(y1, y2, y2, "y", "y"), categ = list("kind", "kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "INCONSISTENT DOTS", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[6], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                    if( ! is.null(tempo.graph$warn)){
                        warn.count <- warn.count + 1
                        tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else{
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nINCONSISTENT DOTS", text.size = 8, title = "DATA2 + DATA2 INCONSISTENT DOTS")
                }
                if( ! is.null(data2.unknown.dot)){
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.unknown.dot, hframe , vframe), x = list(x1, x2, x2, "x", "x"), y = list(y1, y2, y2, "y", "y"), categ = list("kind", "kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "UNKNOWN DOTS", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[5], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 UNKNOWN DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                    
                    if( ! is.null(tempo.graph$warn)){
                        warn.count <- warn.count + 1
                        tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else{
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nUNKNOWN DOTS", text.size = 12, title = "DATA2 + DATA2 UNKNOWN DOTS")
                }
            }
        }else if(( ! is.null(x.range.split)) & is.null(y.range.split)){
            if(graph.in.file == FALSE){
                fun_open(pdf = FALSE)
            }
            tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe), x = list(x1, "x"), y = list(y1, "y"), categ = list("kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_path"), alpha = list(0.5, 0.5), title = "DATA1", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
            if( ! is.null(tempo.graph$warn)){
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            if( ! is.null(data1.signif.dot)){
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, data1.signif.dot), x = list(x1, "x", x1), y = list(y1, "y", y1), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME", "SIGNIF DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), "black"), geom = list("geom_point", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA1 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                if( ! is.null(tempo.graph$warn)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }else{
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA1 SIGNIFICANT DOTS")
            }
            if( ! is.null(data1.incon.dot)){
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, data1.incon.dot), x = list(x1, "x", x1), y = list(y1, "y", y1), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME", "INCONSISTENT DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), fun_gg_palette(7)[6]), geom = list("geom_point", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA1 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                if( ! is.null(tempo.graph$warn)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }else{
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1\nINCONSISTENT DOTS", text.size = 8, title = "DATA1 + DATA1 INCONSISTENT DOTS")
            }
            if( ! is.null(data2)){
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, hframe), x = list(x1, x2, "x"), y = list(y1, y2, "y"), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA2", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                if( ! is.null(tempo.graph$warn)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
                if( ! is.null(data2.signif.dot)){
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.signif.dot, hframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "SIGNIF DOTS", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], "black", rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                    if( ! is.null(tempo.graph$warn)){
                        warn.count <- warn.count + 1
                        tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else{
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS")
                }
                if( ! is.null(data2.incon.dot)){
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.incon.dot, hframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "INCONSISTENT DOTS", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[6], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                    if( ! is.null(tempo.graph$warn)){
                        warn.count <- warn.count + 1
                        tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else{
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nINCONSISTENT DOTS", text.size = 8, title = "DATA2 + DATA2 INCONSISTENT DOTS")
                }
                if( ! is.null(data2.unknown.dot)){
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.unknown.dot, hframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "UNKNOWN DOTS", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[5], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 UNKNOWN DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                    if( ! is.null(tempo.graph$warn)){
                        warn.count <- warn.count + 1
                        tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else{
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nUNKNOWN DOTS", text.size = 8, title = "DATA2 + DATA2 UNKNOWN DOTS")
                }
            }
        }else if(is.null(x.range.split) & ( ! is.null(y.range.split))){
            if(graph.in.file == FALSE){
                fun_open(pdf = FALSE)
            }
            tempo.graph <- fun_gg_scatter(data1 = list(data1, vframe), x = list(x1, "x"), y = list(y1, "y"), categ = list("kind", "kind"), legend.name = list("DATASET", "VERT FRAME"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_path"), alpha = list(0.5, 0.5), title = "DATA1", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
            if( ! is.null(tempo.graph$warn)){
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            if( ! is.null(data1.signif.dot)){
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                tempo.graph <- fun_gg_scatter(data1 = list(data1, vframe, data1.signif.dot), x = list(x1, "x", x1), y = list(y1, "y", y1), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "VERT FRAME", "SIGNIF DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2), "black"), geom = list("geom_point", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA1 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                if( ! is.null(tempo.graph$warn)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }else{
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA1 SIGNIFICANT DOTS")
            }
            if( ! is.null(data1.incon.dot)){
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                tempo.graph <- fun_gg_scatter(data1 = list(data1, vframe, data1.incon.dot), x = list(x1, "x", x1), y = list(y1, "y", y1), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "VERT FRAME", "INCONSISTENT DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2), fun_gg_palette(7)[6]), geom = list("geom_point", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA1 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                if( ! is.null(tempo.graph$warn)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }else{
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1\nINCONSISTENT DOTS", text.size = 8, title = "DATA1 + DATA1 INCONSISTENT DOTS")
            }
            if( ! is.null(data2)){
                if(graph.in.file == FALSE){
                    fun_open(pdf = FALSE)
                }
                tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, vframe), x = list(x1, x2, "x"), y = list(y1, y2, "y"), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA2", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                if( ! is.null(tempo.graph$warn)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
                if( ! is.null(data2.signif.dot)){
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.signif.dot, vframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "SIGNIF DOTS", "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], "black", rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                    if( ! is.null(tempo.graph$warn)){
                        warn.count <- warn.count + 1
                        tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else{
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS")
                }
                if( ! is.null(data2.incon.dot)){
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.incon.dot, vframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "INCONSISTENT DOTS", "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[6], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                    if( ! is.null(tempo.graph$warn)){
                        warn.count <- warn.count + 1
                        tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else{
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nINCONSISTENT DOTS", text.size = 8, title = "DATA2 + DATA2 INCONSISTENT DOTS")
                }
                if( ! is.null(data2.unknown.dot)){
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.unknown.dot, vframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "UNKNOWN DOTS", "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[5], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 UNKNOWN DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
                    if( ! is.null(tempo.graph$warn)){
                        warn.count <- warn.count + 1
                        tempo.warn <- paste0("(", warn.count,") FROM fun_gg_scatter():\n", tempo.graph$warn)
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else{
                    if(graph.in.file == FALSE){
                        fun_open(pdf = FALSE)
                    }
                    fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nUNKNOWN DOTS", text.size = 8, title = "DATA2 + DATA2 UNKNOWN DOTS")
                }
            }
        }
    }
    # end plot
    if(warn.print == TRUE & ! is.null(warn)){
        options(warning.length = 8170)
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    on.exit(exp = options(warning.length = ini.warning.length), add = TRUE)
    tempo.list <- list(data1.removed.row.nb = data1.removed.row.nb, data1.removed.rows = data1.removed.rows, data2.removed.row.nb = data2.removed.row.nb, data2.removed.rows = data2.removed.rows, hframe = hframe, vframe = vframe, data1.signif.dot = data1.signif.dot, data1.non.signif.dot = data1.non.signif.dot, data1.inconsistent.dot = data1.incon.dot, data2.signif.dot = data2.signif.dot, data2.non.signif.dot = data2.non.signif.dot, data2.unknown.dot = data2.unknown.dot, data2.inconsistent.dot = data2.incon.dot, axes = axes, warn = warn)
    # end main code
    # output
    # warning output
    # warning output
    if(warn.print == TRUE & ! is.null(warn)){
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    on.exit(exp = options(warning.length = ini.warning.length), add = TRUE)
    # end warning output
    
    # end warning output
    return(tempo.list)
    # end output
}
