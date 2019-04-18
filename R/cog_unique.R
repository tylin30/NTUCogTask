cog_unique <- function(df, Task){
  if (Task == "SRTCRT"){
    summary_string <- "summarise(.,
    SRT_mrt = unique(SRT_mrt),
    SRT_vrt = unique(SRT_vrt),
    CRT_mrt = unique(CRT_mrt),
    CRT_vrt = unique(CRT_vrt),
    SRT_mac = unique(SRT_mac),
    SRT_vac = unique(SRT_vac),
    CRT_mac = unique(CRT_mac),
    CRT_vac = unique(CRT_vac)
    )"
  }
  else if (Task == "DMS"){
    summary_string <- "summarise(.,
    delay0ACC = unique(delay0ACC),
    delay0RT = unique(delay0RT),
    delay5ACC = unique(delay5ACC),
    delay5RT = unique(delay5RT),
    delay10ACC = unique(delay10ACC),
    delay10RT = unique(delay10RT)
    )"
  }

  tmpdf <- df %>%
    group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
    eval(parse(text = summary_string), envir = .)

  return(tmpdf)
}
