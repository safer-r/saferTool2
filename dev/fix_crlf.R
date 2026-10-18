setwd("C:/Users/gmillot/Documents/Git_projects/safer-r/saferTool2")
files <- c(
  "R/permut.R", "R/slide.R", "R/trim.R",
  "man/permut.Rd", "dev/articles/permut.Rmd", "vignettes/articles/permut.Rmd"
)
for(f in files){
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  con <- file(f, open = "wb")
  # split on \n and rejoin with CRLF so every line terminator is CRLF
  parts <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  writeLines(parts, con, sep = "\r\n")
  close(con)
  cat(f, "normalized to CRLF,", length(parts), "lines\n")
}
