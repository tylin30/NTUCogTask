cog_unique <- function(df, Task){
  ## SRTCRT ----
  if (Task == "SRTCRT"){
    summary_string <- "summarise(.,
    SRT_mrt = unique(SRT_mrt),
    SRT_mrt_ro = unique(SRT_mrt_ro),
    SRT_vrt = unique(SRT_vrt),
    CRT_mrt = unique(CRT_mrt),
    CRT_mrt_ro = unique(CRT_mrt_ro),
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
                             Task, "_0rt_ro = unique(delay0RT_ro), ",
                             Task, "_0ac = unique(delay0ACC), ",
                             Task, "_5rt = unique(delay5RT), ",
                             Task, "_5rt_ro = unique(delay5RT_ro), ",
                             Task, "_5ac = unique(delay5ACC), ",
                             Task, "_0ac = unique(delay0ACC), ",
                             Task, "_10rt = unique(delay10RT), ",
                             Task, "_10rt_ro = unique(delay10RT_ro), ",
                             Task, "_10ac = unique(delay10ACC))"
    )
  }

  ## DR ----
   else if (Task == "DR"){
    summary_string <- paste0("summarise(.,",
                             Task, "_11rt = unique(", Task, "_11rt), ",
                             Task, "_11rt_ro = unique(", Task, "_11rt_ro), ",
                             Task, "_11ac = unique(", Task, "_11ac), ",
                             Task, "_14rt = unique(", Task, "_14rt), ",
                             Task, "_14rt_ro = unique(", Task, "_14rt_ro), ",
                             Task, "_14ac = unique(", Task, "_14ac), ",
                             Task, "_41rt = unique(", Task, "_41rt), ",
                             Task, "_41rt_ro = unique(", Task, "_41rt_ro), ",
                             Task, "_41ac = unique(", Task, "_41ac), ",
                             Task, "_44rt = unique(", Task, "_44rt), ",
                             Task, "_44rt_ro = unique(", Task, "_44rt_ro), ",
                             Task, "_44ac = unique(", Task, "_44ac), ",

                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mrt_ro = unique(", Task, "_mrt_ro), ",
                             Task, "_mac = unique(", Task, "_mac), ",

                             Task, "_Coca = unique(", Task, "_Coca), ",
                             Task, "_Paca = unique(", Task, "_Paca)) "

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
  else if(Task == "SST"){
    summary_string <- paste0("summarise(.,",
                             Task, "_pgoRT = unique(pgoRT), ",
                             Task, "_goRT = unique(goRT), ",
                             Task, "_goRT_ro = unique(goRT_ro), ",
                             Task, "_ssd = unique(", Task, "_ssd), ",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mrt_ro = unique(", Task, "_mrt_ro), ",
                             Task, "_mac = unique(", Task, "_mac), ",
                             Task, "_int = unique(", Task, "_int), ",
                             Task, "_stopac = unique(", Task, "_stopac), ",
                             Task, "_stopac_no05 = unique(", Task, "_stopac_no05), " ,
                             Task, "_last30rt = unique(", Task, "_last30rt), " ,
                             Task, "_int_accadjust = unique(", Task, "_int_accadjust), " ,
                             Task, "_int_accadjust_no05 = unique(", Task, "_int_accadjust_no05), " ,
                             Task, "_index = unique(", Task, "_index), " ,
                             Task, "_index_no05 = unique(", Task, "_index_no05), " ,
                             Task, "_last10_stopACC = unique(", Task, "_last10_stopACC) " ,
                             ")"
    )
  }
  ## Sp ----
  else if (Task == "Sp"){
    summary_string <- paste0("summarise(.,",

                             Task, "_ICrt = unique(ICRT), ",
                             Task, "_ICrt_ro = unique(ICRT_ro), ",
                             Task, "_ICac = unique(ICACC), ",
                             Task, "_Nrt = unique(NRT), ",
                             Task, "_Nrt_ro = unique(NRT_ro), ",
                             Task, "_Nac = unique(NACC), ",
                             Task, "_ICNrt = unique(", Task, "_ICNrt), ",
                             Task, "_ICNrt_ro = unique(", Task, "_ICNrt_ro), ",
                             Task, "_ICNac = unique(", Task, "_ICNac))"
    )
  }
  ## As ----
  else if (Task == "As"){
    summary_string <- paste0("summarise(.,",
                             Task, "_mrt = unique(", Task, "_mrt), ",
                             Task, "_mrt_ro = unique(", Task, "_mrt_ro), ",
                             Task, "_mac = unique(", Task, "_mac))"
    )
  }

  ## CTT ----
  else if (Task == "CTT"){
    summary_string <- paste0("summarise(.,",
                             Task, "_nonswrt = unique(", Task, "_nonswrt), ",
                             Task, "_nonswac = unique(", Task, "_nonswac), ",
                             Task, "_swrt = unique(", Task, "_swrt), ",
                             Task, "_swac = unique(", Task, "_swac))"

                             # Task, "_sart = unique(", Task, "_sart), ",
                             # Task, "_saac = unique(", Task, "_saac), ",
                             # Task, "_swrt = unique(", Task, "_swrt), ",
                             # Task, "_swac = unique(", Task, "_swac))"
    )
  }


  ## Fg ----
  else if (Task == "Fg"){
    summary_string <- paste0("summarise(.,",
                             Task, "_shapert_ro = unique(", Task, "_shapert_ro), ",
                             Task, "_shapeac = unique(", Task, "_shapeac), ",
                             Task, "_colorrt_ro = unique(", Task, "_colorrt_ro), ",
                             Task, "_colorac = unique(", Task, "_colorac), ",
                             Task, "_pure_rt_ro = unique(", Task, "_pure_rt_ro), ",
                             Task, "_pure_rt_condition_ro = unique(", Task, "_pure_rt_condition_ro), ",
                             Task, "_pure_ac = unique(", Task, "_pure_ac), ",
                             Task, "_rep_shapert_ro = unique(", Task, "_rep_shapert_ro), ",
                             Task, "_rep_shapeac = unique(", Task, "_rep_shapeac), ",
                             Task, "_rep_colorrt_ro = unique(", Task, "_rep_colorrt_ro), ",
                             Task, "_rep_colorac = unique(", Task, "_rep_colorac), ",
                             Task, "_rep_rt_ro = unique(", Task, "_rep_rt_ro), ",
                             Task, "_rep_rt_condition_ro = unique(", Task, "_rep_rt_condition_ro), ",
                             Task, "_rep_ac = unique(", Task, "_rep_ac), ",
                             Task, "_swi_shapert_ro = unique(", Task, "_swi_shapert_ro), ",
                             Task, "_swi_shapeac = unique(", Task, "_swi_shapeac), ",
                             Task, "_swi_colorrt_ro = unique(", Task, "_swi_colorrt_ro), ",
                             Task, "_swi_colorac = unique(", Task, "_swi_colorac),",
                             Task, "_swi_rt_ro = unique(", Task, "_swi_rt_ro),",
                             Task, "_swi_rt_condition_ro = unique(", Task, "_swi_rt_condition_ro),",
                             Task, "_swi_ac = unique(", Task, "_swi_ac),",
                             
                             Task, "_GcostRT_ro = unique(", Task, "_GcostRT_ro), ",
                             Task, "_GcostAC = unique(", Task, "_GcostAC),",
                             Task, "_LcostRT_ro = unique(", Task, "_LcostRT_ro), ",
                             Task, "_LcostAC = unique(", Task, "_LcostAC))"

                             # Task, "_rprt = unique(", Task, "_rprt), ",
                             # Task, "_rprt_ro = unique(", Task, "_rprt_ro), ",
                             # Task, "_rpac = unique(", Task, "_rpac), ",
                             # Task, "_swrt = unique(", Task, "_swrt), ",
                             # Task, "_swrt_ro = unique(", Task, "_swrt_ro), ",
                             # Task, "_swac = unique(", Task, "_swac), ",
                             # Task, "_cort = unique(", Task, "_cort), ",
                             # Task, "_cort_ro = unique(", Task, "_cort_ro), ",
                             # Task, "_coac = unique(", Task, "_coac))"
    )
  }

  ## HF ----
   else if (Task == "HF"){
    summary_string <- paste0("summarise(.,",
                             Task, "_pheartrt_ro = unique(", Task, "_pheartrt_ro), ",
                             Task, "_pheartac = unique(", Task, "_pheartac), ",
                             Task, "_pflowerrt_ro = unique(", Task, "_pflowerrt_ro), ",
                             Task, "_pflowerac = unique(", Task, "_pflowerac), ",
                             
                             Task, "_pure_RT_ro = unique(", Task, "_pure_RT_ro), ",
                             Task, "_pure_RT_condition_ro = unique(", Task, "_pure_RT_condition_ro), ",
                             Task, "_pure_ac = unique(", Task, "_pure_ac), ",
                             
                             Task, "_mixheartrt_ro = unique(", Task, "_mixheartrt_ro), ",
                             Task, "_mixheartac = unique(", Task, "_mixheartac), ",
                             Task, "_mixflowerrt_ro = unique(", Task, "_mixflowerrt_ro), ",
                             Task, "_mixflowerac = unique(", Task, "_mixflowerac),",
                             
                             Task, "_mixRT_ro = unique(", Task, "_mixRT_ro), ",
                             Task, "_mix_RT_condition_ro = unique(", Task, "_mix_RT_condition_ro), ",
                             Task, "_mix_ac = unique(", Task, "_mix_ac),",

                             Task, "_GcosthRT_ro = unique(", Task, "_GcosthRT_ro),",
                             Task, "_GcosthAC = unique(", Task, "_GcosthAC),",
                             Task, "_GcostfRT_ro = unique(", Task, "_GcostfRT_ro),",
                             Task, "_GcostfAC = unique(", Task, "_GcostfAC),",
                             Task, "_GcostRT_ro = unique(", Task, "_GcostRT_ro),",
                             Task, "_GcostAC = unique(", Task, "_GcostAC)"
                             ,")"

                             # Task, "_pcrt = unique(", Task, "_pcrt), ",
                             # Task, "_pcrt_ro = unique(", Task, "_pcrt_ro), ",
                             # Task, "_pcac = unique(", Task, "_pcac), ",
                             # Task, "_picrt = unique(", Task, "_picrt), ",
                             # Task, "_picrt_ro = unique(", Task, "_picrt_ro), ",
                             # Task, "_picac = unique(", Task, "_picac), ",
                             # Task, "_icrt = unique(", Task, "_icrt), ",
                             # Task, "_icrt_ro = unique(", Task, "_icrt_ro), ",
                             # Task, "_icac = unique(", Task, "_icac), ",

                             # Task, "_Hgrt = unique(", Task, "_Hgrt), ",
                             # Task, "_Hgrt_ro = unique(GcosthRT_ro), ",
                             # Task, "_Hgac = unique(", Task, "_Hgac), ",
                             # Task, "_Fgrt = unique(", Task, "_Fgrt), ",
                             # Task, "_Fgrt_ro = unique(GcostfRT_ro), ",
                             # Task, "_Fgac = unique(", Task, "_Fgac), ",
                             # Task, "_Gcostrt = unique(", Task, "_Gcostrt), ",
                             # Task, "_Gcostrt_ro = unique(GcostRT_ro), ",
                             # Task, "_Gcostac = unique(", Task, "_Gcostac), ",

                             #Block -> 0:practice, 1:pure heart, 2:pure flower, 3:mix
                             #Condition -> 1:pure heart 2:pure flower
                             #             3:heart in mix block  4:flower in mix block
                             # Task, "_Hmrt = unique(", Task, "_Hmrt), ",
                             # Task, "_Hmrt_ro = unique(", Task, "_Hmrt_ro), ",
                             # Task, "_Hmac = unique(", Task, "_Hmac), ",
                             # Task, "_Fmrt = unique(", Task, "_Fmrt), ",
                             # Task, "_Fmrt_ro = unique(", Task, "_Fmrt_ro), ",
                             # Task, "_Fmac = unique(", Task, "_Fmac), ",
                             #
                             # Task, "_costrt = unique(", Task, "_costrt), ",
                             # Task, "_costrt_ro = unique(", Task, "_costrt_ro), ",
                             # Task, "_costac = unique(", Task, "_costac))"

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

# cog_unique <- function(df, Task){
#   ## SRTCRT ----
#   if (Task == "SRTCRT"){
#     summary_string <- "summarise(.,
#     SRT_mrt = unique(SRT_mrt),
#     SRT_vrt = unique(SRT_vrt),
#     CRT_mrt = unique(CRT_mrt),
#     CRT_vrt = unique(CRT_vrt),
#     SRT_mac = unique(SRT_mac),
#     SRT_vac = unique(SRT_vac),
#     CRT_mac = unique(CRT_mac),
#     CRT_vac = unique(CRT_vac)
#     )"
#   }
#
#   ## MA/MAO ----
#   else if (Task == "MA" || Task == "MAO"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_mrt = unique(", Task, "_mrt), ",
#                              Task, "_mac = unique(", Task, "_mac), ",
#                              Task, "_ca = unique(", Task, "_ca))"
#                              )
#   }
#
#   ## DMS ----
#   else if (Task == "DMS"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_0rt = unique(delay0RT), ",
#                              Task, "_0ac = unique(delay0ACC), ",
#                              Task, "_5rt = unique(delay5RT), ",
#                              Task, "_5ac = unique(delay5ACC), ",
#                              Task, "_0ac = unique(delay0ACC), ",
#                              Task, "_10rt = unique(delay10RT), ",
#                              Task, "_10ac = unique(delay10ACC))"
#     )
#   }
#
#   ## DR ----
#   else if (Task == "DR"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_11rt = unique(", Task, "_11rt), ",
#                              Task, "_11ac = unique(", Task, "_11ac), ",
#                              Task, "_14rt = unique(", Task, "_14rt), ",
#                              Task, "_14ac = unique(", Task, "_14ac), ",
#                              Task, "_41rt = unique(", Task, "_41rt), ",
#                              Task, "_41ac = unique(", Task, "_41ac), ",
#                              Task, "_44rt = unique(", Task, "_44rt), ",
#                              Task, "_44ac = unique(", Task, "_44ac), ",
#
#                              Task, "_mrt = unique(", Task, "_mrt), ",
#                              Task, "_mac = unique(", Task, "_mac), ",
#
#                              Task, "_Coca = unique(", Task, "_Coca), ",
#                              Task, "_Paca = unique(", Task, "_Paca)) "
#
#     )
#   }
#
#   ## SM ----
#   else if (Task == "SM"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_mrt = unique(", Task, "_mrt), ",
#                              Task, "_mac = unique(", Task, "_mac), ",
#                              Task, "_ca = unique(", Task, "_ca))"
#     )
#   }
#
#
#   ## RMS/RMO ----
#   else if (Task == "RMS" || Task  == "RMO"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_mrt = unique(", Task, "_mrt), ",
#                              Task, "_mac = unique(", Task, "_mac), ",
#                              Task, "_ca = unique(", Task, "_ca))"
#     )
#   }
#
#   ## RML ----
#   else if (Task == "RML"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_mrt = unique(", Task, "_mrt), ",
#                              Task, "_mac = unique(", Task, "_mac), ",
#                              Task, "_ca = unique(", Task, "_ca))"
#     )
#   }
#   ## SST ----
#   else if (Task == "SST"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_mrt = unique(", Task, "_mrt), ",
#                              Task, "_mac = unique(", Task, "_mac), ",
#                              Task, "_int = unique(", Task, "_int), ",
#                              Task, "_ac = unique(", Task, "_ac))"
#     )
#   }
#   ## Sp ----
#   else if (Task == "Sp"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_ICrt = unique(ICRT), ",
#                              Task, "_ICac = unique(ICACC), ",
#                              Task, "_Nrt = unique(NRT), ",
#                              Task, "_Nac = unique(NACC), ",
#                              Task, "_ICNrt = unique(", Task, "_ICNrt), ",
#                              Task, "_ICNac = unique(", Task, "_ICNac))"
#     )
#   }
#   ## As ----
#   else if (Task == "As"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_mrt = unique(", Task, "_mrt), ",
#                              Task, "_mac = unique(", Task, "_mac))"
#     )
#   }
#
#   ## CTT ----
#   else if (Task == "CTT"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_sart = unique(", Task, "_sart), ",
#                              Task, "_saac = unique(", Task, "_saac), ",
#                              Task, "_swrt = unique(", Task, "_swrt), ",
#                              Task, "_swac = unique(", Task, "_swac))"
#     )
#   }
#
#   ## Fg ----
#   else if (Task == "Fg"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_rprt = unique(", Task, "_rprt), ",
#                              Task, "_rpac = unique(", Task, "_rpac), ",
#                              Task, "_swrt = unique(", Task, "_swrt), ",
#                              Task, "_swac = unique(", Task, "_swac), ",
#                              Task, "_cort = unique(", Task, "_cort), ",
#                              Task, "_coac = unique(", Task, "_coac))"
#     )
#   }
#
#   ## HF ----
#   else if (Task == "HF"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_pcrt = unique(", Task, "_pcrt), ",
#                              Task, "_pcac = unique(", Task, "_pcac), ",
#                              Task, "_picrt = unique(", Task, "_picrt), ",
#                              Task, "_picac = unique(", Task, "_picac), ",
#                              Task, "_icrt = unique(", Task, "_icrt), ",
#                              Task, "_icac = unique(", Task, "_icac), ",
#
#                              Task, "_Hgrt = unique(", Task, "_Hgrt), ",
#                              Task, "_Hgac = unique(", Task, "_Hgac), ",
#                              Task, "_Fgrt = unique(", Task, "_Fgrt), ",
#                              Task, "_Fgac = unique(", Task, "_Fgac), ",
#                              Task, "_Gcostrt = unique(", Task, "_Gcostrt), ",
#                              Task, "_Gcostac = unique(", Task, "_Gcostac), ",
#
#                              #Block -> 0:practice, 1:pure heart, 2:pure flower, 3:mix
#                              #Condition -> 1:pure heart 2:pure flower
#                              #             3:heart in mix block  4:flower in mix block
#                              Task, "_Hmrt = unique(", Task, "_Hmrt), ",
#                              Task, "_Hmac = unique(", Task, "_Hmac), ",
#                              Task, "_Fmrt = unique(", Task, "_Fmrt), ",
#                              Task, "_Fmac = unique(", Task, "_Fmac), ",
#
#                              Task, "_costrt = unique(", Task, "_costrt), ",
#                              Task, "_costac = unique(", Task, "_costac))"
#
#     )
#   }
#
#   ## RS ----
#   else if (Task == "RS"){
#     summary_string <- paste0("summarise(.,",
#                              Task, "_ca = unique(", Task, "_ca), ",
#                              Task, "_armrt = unique(", Task, "_armrt))"
#     )
#   }
#
#
#   if (Task != "SST"){
#     tmpdf <- df %>%
#       group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
#       eval(parse(text = summary_string), envir = .)
#   }else{
#     tmpdf <- df %>%
#       group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
#       filter(., Block != "p" & Block != 0) %>%
#       eval(parse(text = summary_string), envir = .)
#   }
#
#   return(tmpdf)
# }
