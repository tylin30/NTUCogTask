cog_mutate <- function (df, Task)
{
  ## SRTCRT ----
  if (Task == "SRTCRT"){
    mutate_string <- "mutate(., SRT_mrt = mean(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),SRT_vrt = var(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),CRT_mrt = mean(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),CRT_vrt = var(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),SRT_mac = mean(Accuracy[StimulusLocation == 5 & Block != 0]),SRT_vac = var(Accuracy[StimulusLocation == 5 & Block != 0]),CRT_mac = mean(Accuracy[StimulusLocation != 5 & Block != 0]),CRT_vac = var(Accuracy[StimulusLocation != 5 & Block != 0]))"
  }

  ## DMS ----
  else if (Task == "DMS"){
    mutate_string <- "mutate(.)"
  }

  ## MA/MAO ----
  else if (Task == "MA" || Task == "MAO"){
    mutate_string <- paste0("mutate(., ",
                            Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ",
                            Task, "_mac = mean(Accuracy[Block != 0]), ",
                            Task, "_ca = yourSPAN) ")
  }
  ## SM ----
  else if (Task == "SM"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ",
                            Task, "_mac = mean(Accuracy[Block != 0]), ",
                            Task, "_ca = yourSPAN) ")
  }
  ## RMS/RMO ----
  else if (Task == "RMS" || Task == "RMO"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ",
                            Task, "_mac = mean(Accuracy[Block != 0]), ",
                            Task, "_ca = yourSPAN) ")
  }
  ## RML ----
  else if (Task == "RML"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ",
                            Task, "_mac = mean(Accuracy[Block != 0]), ",
                            Task, "_ca = yourSPAN) ")
  }
  ## SST ----
  else if (Task == "SST"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_mrt = goRT - pgoRT, ",
                            Task, "_mac = goACC - pgoACC, ",
                            Task, "_int = ThresholdStopTrials, ",
                            Task, "_ac = stopACC)")
  }
  ## Sp ----
  else if (Task == "Sp"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_ICNrt = ICRT - NRT, " ,
                            Task, "_ICNac = ICACC - NACC)"
                            )
  }

  ## As ----
  else if (Task == "As"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_mrt = MeanRT, " ,
                            Task, "_mac = MeanAC)"
    )
  }

  ## CTT ----
  else if (Task == "CTT"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_sart = SaRT, " ,
                            Task, "_saac = SaAC, " ,
                            Task, "_swrt = SwitchRT, " ,
                            Task, "_swac = SwitchAC)"
    )
  }

  ## Fg ----
  else if (Task == "Fg"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_rprt = repRT, " ,
                            Task, "_rpac = repACC, " ,
                            Task, "_swrt = swiRT, " ,
                            Task, "_swac = swiACC, " ,
                            Task, "_cort = (swiRT - repRT), " ,
                            Task, "_coac = (swiACC - repACC))"
    )
  }

  tmpdf <- df %>%
    group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
    eval(parse(text = mutate_string), envir = .)

  return(tmpdf)
}
