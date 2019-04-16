cog_rbindobs <- function(path, Regex_pattern){
  files <- list.files(path = path, pattern = Regex_pattern)
  df <- files %>%
    # read in all the files, appending the path before the filename
    map(~ read_csv(file.path(path, file = .))) %>%
    # remove Task unfinished
    .[lapply(., colnames) != "Task unfinished!"] %>%
    reduce(rbind)
  return(df)
}
