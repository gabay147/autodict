problem_lines <- lapply(list.files("R", full.names = TRUE), function(file) {
  lines <- readLines(file, warn = FALSE)
  grep("^\\s*([<-]|=)", lines, value = TRUE)
})

problem_lines <- unlist(problem_lines)
if (length(problem_lines) == 0) {
  message("No suspicious assignment lines found")
} else {
  print(problem_lines)
}

