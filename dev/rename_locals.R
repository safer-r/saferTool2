# rename dotted local variables to underscores in R files (collision-checked)
setwd("C:/Users/gmillot/Documents/Git_projects/safer-r/saferTool2")

# per-file target lists (collisions already excluded after scan:
# tempo.cat, tempo.log (codon_finder/slide/trim), par.ini, ini.date, ini.time,
# function.name; USE.NAMES is a base argument, never a target)
map <- list(
  "permut.R" = c(
    "tempo.random.seed", "ini.pos", "tempo.pos", "pos.selec.seq.max", "tempo.cor",
    "print.count.loop", "count.loop", "tempo.date.loop", "tempo.time.loop", "tempo.time",
    "tempo.lapse", "final.loop", "final.exp", "cor.ini", "neg.cor", "smallest.cor.dec",
    "count.est", "first.round", "cor.dec.per.loop", "loop.nb.est", "cor.est.ini", "cor.est",
    "tempo.pos.est", "tempo.cor.est", "tempo.cor.dec.per.loop", "tempo.pos.secu", "count.secu",
    "tempo.cor.secu", "tempo.cor.loop"
  ),
  "slide.R" = c(
    "right.last.wind", "env.name", "tempo.message", "left.log", "right.log", "tempo.thread.nb",
    "cluster.list", "paral.output.list", "process.id", "end.date", "end.time", "total.lapse",
    "print.count.loop", "tempo.time", "tempo.lapse", "final.loop", "final.exp"
  ),
  "trim.R" = c(
    "tempo.random.seed", "na.nb", "color.cut", "col.mean", "col.quantile", "quantiles.selection",
    "inter.tick.unit", "sampled.data", "graph.xlim", "output.tempo", "x.text", "y.text"
  )
)

for(f in names(map)){
  path <- file.path("R", f)
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  total <- 0
  for(id in map[[f]]){
    und <- gsub("\\.", "_", id)
    rx <- paste0("(?<![A-Za-z0-9_.])", gsub("\\.", "\\\\.", id), "(?![A-Za-z0-9_.])")
    n <- length(regmatches(txt, gregexpr(rx, txt, perl = TRUE))[[1]])
    # safety: the underscore form must not exist
    if(length(regmatches(txt, gregexpr(paste0("(?<![A-Za-z0-9_.])", und, "(?![A-Za-z0-9_])"), txt, perl = TRUE))[[1]]) > 0){
      stop("COLLISION for ", id, " in ", f)
    }
    txt <- gsub(rx, und, txt, perl = TRUE)
    total <- total + n
  }
  # string-label fragments preceded by literal \n (lookbehind cannot match there)
  labels <- c(
    "\\ncor.est.ini: ", "\\ntempo.cor.est: ", "\\nloop.nb.est: ", "\\ncor.ini: ",
    "\\ntempo.cor: ", "\\ncor.limit: ", "\\ncor.dec.per.loop: ", "\\nwindow.size: "
  )
  for(lb in labels){
    lb.und <- gsub("\\.", "_", lb)
    if(grepl(lb, txt, fixed = TRUE)){
      txt <- gsub(lb, lb.und, txt, fixed = TRUE)
      total <- total + 1
    }
  }
  txt <- gsub("ENVIRONMENT env\\.name ALREADY EXISTS", "ENVIRONMENT env_name ALREADY EXISTS", txt, perl = TRUE)
  con <- file(path, open = "wb")
  writeLines(txt, con, sep = "\r\n")
  close(con)
  cat(f, ":", total, "replacements\n")
}

# documentation: roxygen example in R/permut.R and man/vignette snippets
docs <- c("man/permut.Rd", "dev/articles/permut.Rmd", "vignettes/articles/permut.Rmd")
for(d in docs){
  path <- d
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  total <- 0
  for(id in c("ini.pos", "tempo.pos", "tempo.time", "tempo.lapse", "ini.time")){
    und <- gsub("\\.", "_", id)
    rx <- paste0("(?<![A-Za-z0-9_.])", gsub("\\.", "\\\\.", id), "(?![A-Za-z0-9_.])")
    n <- length(regmatches(txt, gregexpr(rx, txt, perl = TRUE))[[1]])
    if(length(regmatches(txt, gregexpr(paste0("(?<![A-Za-z0-9_.])", und, "(?![A-Za-z0-9_])"), txt, perl = TRUE))[[1]]) > 0){
      next # do not create mixed forms when the underscore form already exists in the doc
    }
    txt <- gsub(rx, und, txt, perl = TRUE)
    total <- total + n
  }
  con <- file(path, open = "wb")
  writeLines(txt, con, sep = "\r\n")
  close(con)
  cat(d, ":", total, "replacements\n")
}
