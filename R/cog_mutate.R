cog_mutate <- function (df, Task)
{
  ## SRTCRT ----
  if (Task == "SRTCRT"){
    mutate_string <- "mutate(., SRT_mrt = mean(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),SRT_vrt = var(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),CRT_mrt = mean(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),CRT_vrt = var(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),SRT_mac = mean(Accuracy[StimulusLocation == 5 & Block != 0]),SRT_vac = var(Accuracy[StimulusLocation == 5 & Block != 0]),CRT_mac = mean(Accuracy[StimulusLocation != 5 & Block != 0]),CRT_vac = var(Accuracy[StimulusLocation != 5 & Block != 0]))"
  }

  ## MA/MAO ----
  else if (Task == "MA" || Task == "MAO"){
    mutate_string <- paste0("mutate(., ",
                            Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ",
                            Task, "_mac = mean(Accuracy[Block != 0]), ",
                            Task, "_ca = yourSPAN) ")
  }

  ## DMS ----
  else if (Task == "DMS"){
    mutate_string <- "mutate(.)"
  }

  ## DR ----
  else if (Task == "DR"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_11rt = e1t1RT, ",
                            Task, "_11ac = e1t1ACC, ",
                            Task, "_14rt = e1t4RT, ",
                            Task, "_14ac = e1t4ACC, ",
                            Task, "_41rt = e4t1RT, ",
                            Task, "_41ac = e4t1ACC, ",
                            Task, "_44rt = e4t4RT, ",
                            Task, "_44ac = e4t4ACC, ",

                            Task, "_mrt = mean(RT[Block != 0 & ACC == 1]), ",
                            Task, "_mac = mean(ACC[Block != 0]), ",
                            Task, "_Coc = 4 * (e4t1ACC - (1 - mean(ACC[Block != 0 & Condition == 'Mismatch']))), ",
                            Task, "_Pac = 4 * (e4t4ACC - (1 - mean(ACC[Block != 0 & Condition == 'Mismatch'])))) "
                            )
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

  ## HF ----
  else if (Task == "HF"){
    mutate_string <- paste0("mutate(.,",
                            #pc = pure congruent
                            Task, "_pcrt = mean(RT[Block != 0 & Accuracy == 1 & Condition == 1]), " ,
                            Task, "_pcac = mean(Accuracy[Block != 0 & Condition == 1]), " ,
                            Task, "_picrt = mean(RT[Block != 0 & Accuracy == 1 & Condition == 2]), " ,
                            Task, "_picac = mean(Accuracy[Block != 0 & Condition == 2]), " ,
                            Task, "_icrt = mean(RT[Block != 0 & Accuracy == 1 & Condition == 2] - RT[Block != 0 & Accuracy == 1 & Condition == 1]), " ,
                            Task, "_icac = mean(Accuracy[Block != 0 & Condition == 2] - Accuracy[Block != 0 & Condition == 1]), " ,

                            Task, "_Hgrt = GcosthRT, " ,
                            Task, "_Hgac = GcosthAC, " ,
                            Task, "_Fgrt = GcostfRT, " ,
                            Task, "_Fgac = GcostfAC, " ,

                            #Block -> 0:practice, 1:pure heart, 2:pure flower, 3:mix
                            #Condition -> 1:pure heart 2:pure flower
                            #             3:heart in mix block  4:flower in mix block

                            Task, "_Hmrt = mean(RT[Block != 0 & Accuracy == 1 & (Condition %in% c(1,3) )]), " ,
                            Task, "_Hmac = mean(Accuracy[Block != 0 & (Condition %in% c(1,3) )]), " ,
                            Task, "_Fmrt = mean(RT[Block != 0 & Accuracy == 1 & (Condition %in% c(2,4) )]), " ,
                            Task, "_Fmac = mean(Accuracy[Block != 0 & (Condition %in% c(2,4) )]), " ,

                            Task, "_costrt = mean(RT[Block != 0 & Accuracy == 1 & (Condition %in% c(3,4) )] - Accuracy[Block != 0 & Accuracy == 1 & (Condition %in% c(1,2) )]), " ,
                            Task, "_costac = mean(Accuracy[Block != 0 & (Condition %in% c(3,4) )] - Accuracy[Block != 0 & (Condition %in% c(1,2) )]))"
    )
  }

  ## RS ----
  else if (Task == "RS"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_ca = yourSPAN, " ,
                            #this task no Trial
                            Task, "_armrt = mean(ArrowRT[Trial != 0 & OverallACC == 1 & Condition == 'Arrow']))"
    )
  }

  tmpdf <- df %>%
    group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
    eval(parse(text = mutate_string), envir = .)

  return(tmpdf)
}
