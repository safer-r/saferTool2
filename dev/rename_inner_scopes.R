setwd("C:/Users/gmillot/Documents/Git_projects/safer-r/saferTool2")

# 1) trim.R: rename par.ini -> par_ini inside the three helper closures only (lines 669-737)
p <- "R/trim.R"
txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
n.replace <- 0
for(i in 669:737){
  if(grepl("(?<![A-Za-z0-9_.])par\\.ini(?![A-Za-z0-9_.])", lines[i], perl = TRUE)){
    lines[i] <- gsub("(?<![A-Za-z0-9_.])par\\.ini(?![A-Za-z0-9_.])", "par_ini", lines[i], perl = TRUE)
    n.replace <- n.replace + 1
  }
}
txt <- paste(lines, collapse = "\n")
con <- file(p, open = "wb")
writeLines(strsplit(txt, "\n", fixed = TRUE)[[1]], con, sep = "\r\n")
close(con)
cat("trim.R par.ini:", n.replace, "lines touched\n")

# 2) slide.R: ini.date -> ini_date, ini.time -> ini_time inside the parallel fun only (lines 785-815)
p <- "R/slide.R"
txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
n.replace <- 0
for(i in 785:815){
  if(grepl("(?<![A-Za-z0-9_.])ini\\.date(?![A-Za-z0-9_.])", lines[i], perl = TRUE)){
    lines[i] <- gsub("(?<![A-Za-z0-9_.])ini\\.date(?![A-Za-z0-9_.])", "ini_date", lines[i], perl = TRUE)
    n.replace <- n.replace + 1
  }
  if(grepl("(?<![A-Za-z0-9_.])ini\\.time(?![A-Za-z0-9_.])", lines[i], perl = TRUE)){
    lines[i] <- gsub("(?<![A-Za-z0-9_.])ini\\.time(?![A-Za-z0-9_.])", "ini_time", lines[i], perl = TRUE)
    n.replace <- n.replace + 1
  }
}
# 3) slide.R: function.name -> function_name (formal, body, call site, message string)
for(i in seq_along(lines)){
  if(grepl("(?<![A-Za-z0-9_.])function\\.name(?![A-Za-z0-9_.])", lines[i], perl = TRUE)){
    lines[i] <- gsub("(?<![A-Za-z0-9_.])function\\.name(?![A-Za-z0-9_.])", "function_name", lines[i], perl = TRUE)
    n.replace <- n.replace + 1
  }
}
txt <- paste(lines, collapse = "\n")
con <- file(p, open = "wb")
writeLines(strsplit(txt, "\n", fixed = TRUE)[[1]], con, sep = "\r\n")
close(con)
cat("slide.R touched lines:", n.replace, "\n")
