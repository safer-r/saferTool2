setwd("C:/Users/gmillot/Documents/Git_projects/safer-r/saferTool2")
targets <- c(
  "tempo.random.seed", "ini.pos", "tempo.pos", "pos.selec.seq.max", "tempo.cor",
  "print.count.loop", "count.loop", "tempo.date.loop", "tempo.time.loop", "tempo.time",
  "tempo.lapse", "final.loop", "final.exp", "cor.ini", "neg.cor", "smallest.cor.dec",
  "count.est", "first.round", "cor.dec.per.loop", "loop.nb.est", "cor.est.ini", "cor.est",
  "tempo.pos.est", "tempo.cor.est", "tempo.cor.dec.per.loop", "tempo.pos.secu", "count.secu",
  "tempo.cor.secu", "tempo.cor.loop", "right.last.wind", "env.name", "tempo.message",
  "left.log", "right.log", "tempo.thread.nb", "cluster.list", "paral.output.list", "process.id",
  "end.date", "end.time", "total.lapse", "na.nb", "color.cut", "col.mean", "col.quantile",
  "quantiles.selection", "inter.tick.unit", "sampled.data", "graph.xlim", "output.tempo",
  "x.text", "y.text", "cor.limit", "window.size"
)
files <- c("permut.R", "slide.R", "trim.R", "codon_finder.R", "codon2aa.R")
for(f in files){
  lines <- readLines(file.path("R", f), warn = FALSE)
  for(i in seq_along(lines)){
    for(t in targets){
      rx <- paste0("(?<![A-Za-z0-9_.])", gsub("\\.", "\\\\.", t), "(?![A-Za-z0-9_.])")
      if(grepl(rx, lines[i], perl = TRUE)){
        cat(sprintf("%s:%d [%s] ...%s...\n", f, i, t, substr(lines[i], max(1, regmatches(lines[i], gregexpr(rx, lines[i], perl = TRUE))[[1]][1] - 15), min(nchar(lines[i]), regmatches(lines[i], gregexpr(rx, lines[i], perl = TRUE))[[1]][1] + nchar(t) + 15))))
      }
    }
  }
}
