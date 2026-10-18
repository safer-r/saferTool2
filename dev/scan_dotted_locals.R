setwd("C:/Users/gmillot/Documents/Git_projects/safer-r/saferTool2")
# dotted identifiers that are base-R functions/arguments, strings, or public API (list element names) - never rename
excl <- c(
  "na.rm", "use.names", "ignore.case", "keep.source", "width.cutoff", "include.site",
  "expand.dots", "no.readonly", "all.tests", "big.mark", "pairwise.complete.obs",
  "normal.kind", "sample.kind", "warning.length", "na.last", "col.axis", "lwd.ticks",
  "cex.lab", "cex.axis", "y.intersp", "group.names", "stringsAsFactors", "double_as_integer_allowed",
  "data.frame", "do.call", "seq.int", "as.integer", "as.vector", "as.numeric", "as.double",
  "as.POSIXct", "Sys.time", "Sys.getpid", "is.na", "is.null", "is.numeric", "is.integer",
  "is.finite", "dir.exists", "match.fun", "new.env", "sys.nframe", "on.exit", "set.seed",
  "dev.list", "sample.int", "which.max", "suppressWarnings", "logical", "formatC", "rbind",
  # string artifacts and file names
  "I.E.", "mean.sd", "github.com", "fun_slide_parall_log.txt", "n.HERE", "Rplots.pdf",
  # .Random.seed base object (scan artifact "Random.seed")
  "Random.seed",
  # public API: trim() output list element names
  "real.trim.cutoffs", "trimmed.values", "kept.values",
  # stale message fragments referencing already-renamed ARGUMENTS (pre-existing, not locals)
  "cor.limit", "window.size"
)
files <- c("codon2aa.R", "codon_finder.R", "permut.R", "slide.R", "trim.R")
for(f in files){
  txt <- readLines(file.path("R", f), warn = FALSE)
  code <- sub("#.*", "", txt) # strip comments (crude)
  code.join <- paste(code, collapse = "\n")
  # dotted identifiers assigned with <- or = at least once
  m <- gregexpr("(?<![A-Za-z0-9_.])[A-Za-z][A-Za-z0-9_]*\\.[A-Za-z][A-Za-z0-9_.]*\\s*(<-|=[^=])", code.join, perl = TRUE)
  ids <- regmatches(code.join, m)[[1]]
  ids <- unique(sub("\\s*(<-|=[^=])$", "", ids))
  ids <- setdiff(ids, excl)
  cat("\n==== ", f, " ====\n", sep = "")
  if(length(ids) == 0){
    cat("  (no dotted assigned local variable)\n")
  }
  for(id in ids){
    und <- gsub("\\.", "_", id)
    rx <- paste0("(?<![A-Za-z0-9_.])", und, "(?![A-Za-z0-9_])")
    n.und <- length(regmatches(code.join, gregexpr(rx, code.join, perl = TRUE))[[1]])
    n.dot <- length(regmatches(code.join, gregexpr(paste0("(?<![A-Za-z0-9_.])", gsub("\\.", "\\\\.", id), "(?![A-Za-z0-9_.])"), code.join, perl = TRUE))[[1]])
    status <- if(n.und > 0) "COLLISION -> SKIP" else "OK TO RENAME"
    cat(sprintf("  %-28s dot:%4d und:%4d  %s\n", id, n.dot, n.und, status))
  }
}
