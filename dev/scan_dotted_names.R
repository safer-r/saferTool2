setwd("C:/Users/gmillot/Documents/Git_projects/safer-r/saferTool2")
for(f in list.files("R", full.names = TRUE)){
  lines <- readLines(f, warn = FALSE)
  for(i in seq_along(lines)){
    ln <- lines[i]
    code <- sub("#.*", "", ln) # strip comments
    m <- gregexpr("[A-Za-z][A-Za-z0-9_]*\\.([A-Za-z][A-Za-z0-9_.]*)", code)
    hits <- regmatches(code, m)[[1]]
    hits <- hits[ ! hits %in% c("Rplots.pdf")]
    if(length(hits) > 0){
      cat(basename(f), i, ":", paste(unique(hits), collapse = ", "), "\n")
    }
  }
}
