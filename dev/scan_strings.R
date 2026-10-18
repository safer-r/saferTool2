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
  "x.text", "y.text", "par.ini", "ini.date", "ini.time", "function.name", "tempo.log", "tempo.cat"
)
for(f in c("permut.R", "slide.R", "trim.R", "codon_finder.R")){
  pd <- getParseData(parse(file.path("R", f), keep.source = TRUE))
  str <- pd[pd[["token"]] == "STR_CONST", ]
  for(t in targets){
    rx <- paste0("(?<![A-Za-z0-9_.])", gsub("\\.", "\\\\.", t), "(?![A-Za-z0-9_.])")
    hit <- grepl(rx, str[["text"]], perl = TRUE)
    if(any(hit)){
      cat(f, "| IN STRING:", t, "->", trimws(substr(str[["text"]][hit][1], 1, 90)), "\n")
    }
  }
}
cat("string scan done\n")
