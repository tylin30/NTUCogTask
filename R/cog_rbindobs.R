cog_rbindobs <- function(path, Task){
  if (Task == "SRTCRT"){
    Regex_pattern <- ".*SRT_CRT.*.csv"
  }
  else if (Task == "MA"){
    Regex_pattern <- ".*Memory_of_Association_[^Object].*.csv"
  }
  else if (Task == "MAO"){
    Regex_pattern <- ".*Memory_of_Association_Object.*.csv"
  }
  else if (Task == "DMS"){
    Regex_pattern <- ".*Delayed_Matching_to_Sample.*.csv"
  }
  else if (Task == "DR"){
    Regex_pattern <- ".*Delayed_Response_Task.*.csv"
  }
  else if (Task == "SM"){
    Regex_pattern <- ".*Spatial_Memory_Task.*.csv"
  }
  else if (Task == "RMS"){
    Regex_pattern <- ".*Running_Memory_of_Symbols.*.csv"
  }
  else if (Task == "RMO"){
    Regex_pattern <- ".*Running_Memory_of_Objects.*.csv"
  }
  else if (Task == "RML"){
    Regex_pattern <- ".*Running_Memory_of_Locations.*.csv"
  }
  else if (Task == "SST"){
    Regex_pattern <- ".*Stop_Signal_Task.*.csv"
  }
  else if (Task == "Sp"){
    Regex_pattern <- ".*Stroop.*.csv"
  }
  else if (Task == "As"){
    Regex_pattern <- ".*Antisaccade_Arrow.*.csv"
  }
  else if (Task == "CTT"){
    Regex_pattern <- ".*Color_Trail_Test.*.csv"
  }
  else if (Task == "Fg"){
    Regex_pattern <- ".*Figure_Task.*.csv"
  }
  else if (Task == "HF"){
    Regex_pattern <- ".*Hearts_and_Flowers.*.csv"
  }
  else if (Task == "RS"){
    Regex_pattern <- ".*Rotation_Span_Task.*.csv"
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
