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
    summary_string <- paste0("summarise(.,",
    Task, "_0rt = unique(delay0RT), ",
    Task, "_0ac = unique(delay0ACC), ",
    Task, "_5rt = unique(delay5RT), ",
    Task, "_0ac = unique(delay0ACC), ",
    Task, "_10rt = unique(delay10RT), ",
    Task, "_10ac = unique(delay10ACC))"
    )
  }
  else if (Task == "MA" || Task == "MAO"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac, ), ",
                             Task, "_ca = unique(", Task, "_ca))"
                             )
  }

  tmpdf <- df %>%
    group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
    eval(parse(text = summary_string), envir = .)

  return(tmpdf)
}
