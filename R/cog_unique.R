cog_unique <- function(df, Task){
  ## SRTCRT ----
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

  ## MA/MAO ----
  else if (Task == "MA" || Task == "MAO"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_ca = unique(", Task, "_ca))"
                             )
  }

  ## DMS ----
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

  ## DR ----
  else if (Task == "DR"){
    summary_string <- paste0("summarise(.,",
                             Task, "_11rt = unique(", Task, "_11rt), ",
                             Task, "_11ac = unique(", Task, "_11ac), ",
                             Task, "_14rt = unique(", Task, "_14rt), ",
                             Task, "_14ac = unique(", Task, "_14ac), ",
                             Task, "_41rt = unique(", Task, "_41rt), ",
                             Task, "_41ac = unique(", Task, "_41ac), ",
                             Task, "_44rt = unique(", Task, "_44rt), ",
                             Task, "_44ac = unique(", Task, "_44ac), ",

                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",

                             Task, "_Coc = unique(", Task, "_Coc), ",
                             Task, "_Pac = unique(", Task, "_Pac)) "

    )
  }

  ## SM ----
  else if (Task == "SM"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_ca = unique(", Task, "_ca))"
    )
  }


  ## RMS/RMO ----
  else if (Task == "RMS" || Task  == "RMO"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_ca = unique(", Task, "_ca))"
    )
  }

  ## RML ----
  else if (Task == "RML"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_ca = unique(", Task, "_ca))"
    )
  }
  ## SST ----
  else if (Task == "SST"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_int = unique(", Task, "_int), ",
                             Task, "_ac = unique(", Task, "_ac))"
    )
  }
  ## Sp ----
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
  ## As ----
  else if (Task == "As"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mac = unique(", Task, "_mac))"
    )
  }

  ## CTT ----
  else if (Task == "CTT"){
    summary_string <- paste0("summarise(.,",
                             Task, "_sart = unique(", Task, "_sart), ",
                             Task, "_saac = unique(", Task, "_saac), ",
                             Task, "_swrt = unique(", Task, "_swrt), ",
                             Task, "_swac = unique(", Task, "_swac))"
    )
  }

  ## Fg ----
  else if (Task == "Fg"){
    summary_string <- paste0("summarise(.,",
                             Task, "_rprt = unique(", Task, "_rprt), ",
                             Task, "_rpac = unique(", Task, "_rpac), ",
                             Task, "_swrt = unique(", Task, "_swrt), ",
                             Task, "_swac = unique(", Task, "_swac), ",
                             Task, "_cort = unique(", Task, "_cort), ",
                             Task, "_coac = unique(", Task, "_coac))"
    )
  }

  ## HF ----
  else if (Task == "HF"){
    summary_string <- paste0("summarise(.,",
                             Task, "_pcrt = unique(", Task, "_pcrt), ",
                             Task, "_pcac = unique(", Task, "_pcac), ",
                             Task, "_picrt = unique(", Task, "_picrt), ",
                             Task, "_picac = unique(", Task, "_picac), ",
                             Task, "_icrt = unique(", Task, "_icrt), ",
                             Task, "_icac = unique(", Task, "_icac), ",

                             Task, "_Hgrt = unique(", Task, "_Hgrt), ",
                             Task, "_Hgac = unique(", Task, "_Hgac), ",
                             Task, "_Fgrt = unique(", Task, "_Fgrt), ",
                             Task, "_Fgac = unique(", Task, "_Fgac), ",

                             #Block -> 0:practice, 1:pure heart, 2:pure flower, 3:mix
                             #Condition -> 1:pure heart 2:pure flower
                             #             3:heart in mix block  4:flower in mix block
                             Task, "_Hmrt = unique(", Task, "_Hmrt), ",
                             Task, "_Hmac = unique(", Task, "_Hmac), ",
                             Task, "_Fmrt = unique(", Task, "_Fmrt), ",
                             Task, "_Fmac = unique(", Task, "_Fmac), ",

                             Task, "_costrt = unique(", Task, "_costrt), ",
                             Task, "_costac = unique(", Task, "_costac))"

    )
  }

  ## RS ----
  else if (Task == "RS"){
    summary_string <- paste0("summarise(.,",
                             Task, "_ca = unique(", Task, "_ca), ",
                             Task, "_armrt = unique(", Task, "_armrt))"
    )
  }


  if (Task != "SST"){
    tmpdf <- df %>%
      group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
      eval(parse(text = summary_string), envir = .)
  }else{
    tmpdf <- df %>%
      group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
      filter(., Block != "p" & Block != 0) %>%
      eval(parse(text = summary_string), envir = .)
  }

  return(tmpdf)
}
