cog_rbindobs <- function(path, Task){
  if (Task == "SRTCRT"){
    Regex_pattern <- ".*SRT_CRT.*.csv"
  }
  else if (Task == "MA"){
    Regex_pattern <- ".*Memory_of_Association_[^Object].*.csv"
  }
  else if (Task == "MAS"){
    Regex_pattern <- ".*Memory_of_Association_Object.*.csv"
  }
  else{
    Regex_pattern <- Task
  }
  files <- list.files(path = path, pattern = Regex_pattern)
  df <- files %>%
    # read in all the files, appending the path before the filename
    map(~ read_csv(file.path(path, file = .))) %>%
    # remove Task unfinished
    .[lapply(., colnames) != "Task unfinished!"] %>%
    reduce(rbind)
  return(df)
}
