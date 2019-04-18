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
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_ca = unique(", Task, "_ca))"
                             )
  }
  else if (Task == "SM"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_ca = unique(", Task, "_ca))"
    )
  }
  else if (Task == "RMS" || Task  == "RMO"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_ca = unique(", Task, "_ca))"
    )
  }

  else if (Task == "RML"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_ca = unique(", Task, "_ca))"
    )
  }
  else if (Task == "SST"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_int = unique(", Task, "_int), ",
                             Task, "_ac = unique(", Task, "_ac))"
    )
  }
  else if (Task == "Sp"){
    summary_string <- paste0("summarise(.,",
                             Task, "_ICrt = unique(ICRT), ",
                             Task, "_ICac = unique(ICACC), ",
                             Task, "_Nrt = unique(NRT), ",
                             Task, "_Nac = unique(NACC), ",
                             Task, "_ICNrt = unique(", Task, "_ICrt), ",
                             Task, "_ICNac = unique(", Task, "_ICac))"
    )
  }

  else if (Task == "As"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac))"
    )
  }

  tmpdf <- df %>%
    group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
    eval(parse(text = summary_string), envir = .)

  return(tmpdf)
}
