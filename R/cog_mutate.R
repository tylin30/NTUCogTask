cog_mutate <- function (df, Task)
{
  ## SRTCRT ----
  if (Task == "SRTCRT"){

    mutate_string <- "mutate(., SRT_mrt = mean(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),SRT_vrt = var(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),CRT_mrt = mean(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),CRT_vrt = var(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),SRT_mac = mean(Accuracy[StimulusLocation == 5 & Block != 0]),SRT_vac = var(Accuracy[StimulusLocation == 5 & Block != 0]),CRT_mac = mean(Accuracy[StimulusLocation != 5 & Block != 0]),CRT_vac = var(Accuracy[StimulusLocation != 5 & Block != 0]),SRT_mrt_ro = mean(RT_ro[StimulusLocation == 5 & Block != 0 & Accuracy == 1], na.rm=TRUE),SRT_vrt_ro = var(RT_ro[StimulusLocation == 5 & Block != 0 & Accuracy == 1], na.rm=TRUE),CRT_mrt_ro = mean(RT_ro[StimulusLocation != 5 & Block != 0 & Accuracy == 1], na.rm=TRUE),CRT_vrt_ro = var(RT_ro[StimulusLocation != 5 & Block != 0 & Accuracy == 1], na.rm=TRUE))"
  }

  ## MA/MAO ----
  else if (Task == "MA" || Task == "MAO"){
    mutate_string <- paste0("mutate(., ",
                            Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1], na.rm=TRUE), ",
                            Task, "_mac = mean(Accuracy[Block != 0], na.rm=TRUE), ",
                            Task, "_ca = yourSPAN) ")
  }

  ## DMS ----
  else if (Task == "DMS"){
    mutate_string <- paste0("mutate(.,",
                            "delay0RT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Delay == 0] , na.rm=TRUE), ",
                            "delay5RT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Delay == 5000] , na.rm=TRUE), ",
                            "delay10RT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Delay == 10000] , na.rm=TRUE))"
    )
  }

  ## DR ----
   else if (Task == "DR"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_11rt = e1t1RT, ",
                            Task, "_11rt_ro = mean(RT_ro[Block != 0 & Encode==1 & Test==1 & ACC == 1], na.rm=TRUE), ",
                            Task, "_11ac = e1t1ACC, ",
                            
                            Task, "_14rt = e1t4RT, ",
                            Task, "_14rt_ro = mean(RT_ro[Block != 0 & Encode==1 & Test==4 & ACC == 1], na.rm=TRUE), ",
                            Task, "_14ac = e1t4ACC, ",
                            
                            Task, "_41rt = e4t1RT, ",
                            Task, "_41rt_ro = mean(RT_ro[Block != 0 & Encode==4 & Test==1 & ACC == 1], na.rm=TRUE), ",
                            Task, "_41ac = e4t1ACC, ",
                            
                            Task, "_44rt = e4t4RT, ",
                            Task, "_44rt_ro = mean(RT_ro[Block != 0 & Encode==4 & Test==4 & ACC == 1], na.rm=TRUE), ",
                            Task, "_44ac = e4t4ACC, ",

                            Task, "_mrt = mean(RT[Block != 0 & ACC == 1], na.rm=TRUE), ",
                            Task, "_mrt_ro = mean(RT_ro[Block != 0 & ACC == 1], na.rm=TRUE), ",
                            Task, "_mac = mean(ACC[Block != 0], na.rm=TRUE), ",
                            Task, "_Coca = 4 * (e4t1ACC/100 - (1 - mean(ACC[Block != 0 & Condition == 'Mismatch'], na.rm=TRUE))), ",
                            Task, "_Paca = 4 * (e4t4ACC/100 - (1 - mean(ACC[Block != 0 & Condition == 'Mismatch'], na.rm=TRUE)))) "
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
                            Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1 & RT != 'NaN'], na.rm = TRUE), ",
                            Task, "_mac = mean(Accuracy[Block != 0]), ",
                            Task, "_ca = yourSPAN) ")
  }
  ## SST ----
  else if (Task == "SST"){
    mutate_string <- paste0("mutate(.,",
                            "goRT_ro = mean(RT_ro[Block != 0 & Block != 'p' & Accuracy == 1 & Condition == 'go'], na.rm = TRUE),",
                            "goRT_quantile50 = quantile(RT_ro[Block != 0 & Block != 'p' & Accuracy == 1 & Condition == 'go'], probs = 0.5, na.rm = TRUE))"
    )
    # quantile(unlist(test2["RT_ro"]), probs = 0.5, na.rm = TRUE)

    mutate_string <- paste0(mutate_string, "%>%",
                            "mutate(.,",
                            Task, "_ssd = goRT_quantile50 - goRT_ro, ",
                            Task, "_mrt = goRT - pgoRT, ",
                            Task, "_mrt_ro = goRT_ro - pgoRT, ",
                            Task, "_mac = goACC - pgoACC, ",
                            Task, "_int = ThresholdStopTrials, ",
                            Task, "_stopac = stopACC, ",
                            Task, "_stopac_no05 = mean(Accuracy[Block != 0 & Condition == 'stop' & Accuracy != 0.5], na.rm = TRUE) * 100",
                            ")"
    )

    formalgodf <- df %>% filter(Block == 1, Condition == 'go') %>% select(Trial, Condition, Block)
    last30_trial = as.numeric(tail(unique(formalgodf$Trial),30)[1])
    
    stopdf <- df %>% filter(Block == 1, Condition == 'stop') %>% select(Trial, Condition, Block)
    last10_trial = as.numeric(tail(unique(stopdf$Trial),10)[1])

    mutate_string <- paste0(mutate_string, "%>%",
                            "mutate(.,",
                            Task, "_last30rt = mean(RT_ro[Block == 1 & Accuracy == 1 & Condition == 'go' & as.numeric(Trial) >= last30_trial], na.rm = TRUE),",
                            Task, "_int_accadjust = (SST_int/(SST_stopac/0.5))*100, ",
                            Task, "_int_accadjust_no05 = (SST_int/(SST_stopac_no05/0.5))*100, ",
                            Task, "_index =", Task, "_last30rt", "/", Task, "_int_accadjust,",
                            Task, "_index_no05 =", Task, "_last30rt", "/", Task, "_int_accadjust_no05,",
                            Task, "_last10_stopACC = mean(Accuracy[Block == 1 & Condition == 'stop' & as.numeric(Trial) >= last10_trial])",
                            ")"
    )
  }

  ## Sp ----
  else if (Task == "Sp"){
    mutate_string <- paste0("mutate(.,",
                            "ICRT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 'IC'], na.rm = TRUE),",
                            "NRT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 'N'], na.rm = TRUE),",
                            Task, "_ICNrt = ICRT - NRT, " ,
                            Task, "_ICNrt_ro = ICRT_ro - NRT_ro,",
                            Task, "_ICNac = ICACC - NACC)"
    )
  }

  ## As ----
  else if (Task == "As"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_mrt = MeanRT, " ,
                            Task, "_mrt_ro = mean(RT_ro[Block != 0 & Accuracy == 1], na.rm=TRUE), " ,
                            Task, "_mac = MeanAC)"
    )
  }

  ## CTT ----
  else if (Task == "CTT"){
    mutate_string <- paste0("mutate(.,",
                            # Task, "_sart = SaRT, " ,
                            # Task, "_saac = SaAC, " ,
                            # Task, "_swrt = switchRT - nonSwitchRT, " ,
                            Task, "_nonswrt = nonSwitchRT, ",
                            Task, "_nonswac = nonSwitchACC, ",
                            Task, "_swrt = switchRT, " ,
                            # Task, "_swac = SwitchAC)"
                            Task, "_swac = switchACC)"
    )
  }

  #"_swrt = switchRT - nonSwitchRT, " due to app error

  ## Fg ----
  else if (Task == "Fg"){
    mutate_string <- paste0("mutate(.,",
                            Task, "_shapert_ro = mean(RT_ro[Block == 1 & Accuracy == 1], na.rm = TRUE), " ,
                            Task, "_shapeac = mean(BlockACC[Block == 1], na.rm = TRUE), " ,

                            Task, "_colorrt_ro = mean(RT_ro[Block == 2 & Accuracy == 1], na.rm = TRUE), " ,
                            Task, "_colorac = mean(BlockACC[Block == 2], na.rm = TRUE), " ,

                            Task, "_rep_shapert_ro = mean(RT_ro[Condition == 3 & Task == 'A' & Accuracy == 1], na.rm = TRUE), " ,
                            Task, "_rep_shapeac = mean(Accuracy[Condition == 3 & Task == 'A'], na.rm = TRUE), " ,

                            Task, "_rep_colorrt_ro = mean(RT_ro[Condition == 3 & Task == 'B' & Accuracy == 1], na.rm = TRUE), " ,
                            Task, "_rep_colorac = mean(Accuracy[Condition == 3 & Task == 'B'], na.rm = TRUE), " ,

                            Task, "_swi_shapert_ro = mean(RT_ro[Condition == 4 & Task == 'A' & Accuracy == 1], na.rm = TRUE), " ,
                            Task, "_swi_shapeac = mean(Accuracy[Condition == 4 & Task == 'A'], na.rm = TRUE), " ,

                            Task, "_swi_colorrt_ro = mean(RT_ro[Condition == 4 & Task == 'B' & Accuracy == 1], na.rm = TRUE), " ,
                            Task, "_swi_colorac = mean(Accuracy[Condition == 4 & Task == 'B'], na.rm = TRUE) ,",
                            
                            Task, "_GcostRT_ro = mean(RT_ro[(Block == 3 | Block == 4) & Condition == 3 & Accuracy == 1], na.rm = TRUE) - mean(RT_ro[(Block == 1 | Block == 2) & Accuracy == 1], na.rm = TRUE) ,",
                            Task, "_GcostAC = mean(Accuracy[(Block == 3 | Block == 4) & Condition == 3], na.rm = TRUE) - mean(Accuracy[(Block == 1 | Block == 2)], na.rm = TRUE),",
                            
                            Task, "_LcostRT_ro = mean(RT_ro[(Block == 3 | Block == 4) & Condition == '4' & Accuracy == 1], na.rm = TRUE) - mean(RT_ro[(Block == 3 | Block == 4) & Condition == '3' & Accuracy == 1], na.rm = TRUE),",
                            Task, "_LcostAC = mean(Accuracy[(Block == 3 | Block == 4) & Condition == '4'], na.rm = TRUE) - mean(Accuracy[(Block == 3 | Block == 4) & Condition == '3'], na.rm = TRUE))"

                            # Task, "_rprt = repRT, " ,
                            # Task, "_rprt_ro = mean(RT_ro[Block != 0 & Condition == 3 & Accuracy == 1], na.rm = TRUE), " ,
                            # Task, "_rpac = repACC, " ,
                            # Task, "_swrt = swiRT, " ,
                            # Task, "_swrt_ro = mean(RT_ro[Block != 0 & Condition == 4 & Accuracy == 1], na.rm = TRUE), " ,
                            # Task, "_swac = swiACC, " ,
                            # Task, "_cort = (swiRT - repRT), " ,
                            # Task, "_cort_ro = mean(RT_ro[Block != 0 & Condition == 4 & Accuracy == 1], na.rm = TRUE) - mean(RT_ro[Block != 0 & Condition == 3 & Accuracy == 1], na.rm = TRUE), " ,
                            # Task, "_coac = (swiACC - repACC))"
                            )

    # mutate_string <- paste0(mutate_string, "%>%",
    #                         "group_by(Subject, Block)", "%>%",
    #                         "mutate(.,",
    #                         "BlockRT_ro = mean(RT_ro[Accuracy == 1], na.rm = TRUE))")
    #
    # mutate_string <- paste0(mutate_string, "%>%",
    #                         "ungroup()", "%>%",
    #                         "group_by(Subject)", "%>%",
    #                         "mutate(.,",
    #                         "GcostRT_ro = Fg_rprt_ro - (mean(RT_ro[Block == 1 & Accuracy == 1], na.rm = TRUE) + mean(RT_ro[Block == 2 & Accuracy == 1], na.rm = TRUE))/2,",
    #                         "LcostRT_ro = Fg_swrt_ro - Fg_rprt_ro)")


  }

  ## HF ----
   else if (Task == "HF"){
    mutate_string <- paste0("mutate(.,",

                            Task, "_pheartrt_ro = mean(RT_ro[Block == 1 & Accuracy == 1], na.rm = TRUE), " ,
                            Task, "_pheartac = mean(Accuracy[Block == 1], na.rm = TRUE), " ,

                            Task, "_pflowerrt_ro = mean(RT_ro[Block == 2 & Accuracy == 1], na.rm = TRUE), " ,
                            Task, "_pflowerac = mean(Accuracy[Block == 2], na.rm = TRUE), " ,

                            Task, "_mixheartrt_ro = mean(RT_ro[Block == 3 & Accuracy == 1 & Condition == 3], na.rm = TRUE), " ,
                            Task, "_mixheartac = mean(Accuracy[Block == 3 & Condition == 3], na.rm = TRUE), " ,

                            Task, "_mixflowerrt_ro = mean(RT_ro[Block == 3 & Accuracy == 1 & Condition == 4], na.rm = TRUE), " ,
                            Task, "_mixflowerac = mean(Accuracy[Block == 3 & Condition == 4], na.rm = TRUE), ",
                            
                            Task, "_mixRT_ro = mean(RT_ro[Block == 3 & Accuracy == 1], na.rm = TRUE), " ,
                            Task, "_mixAC = mean(Accuracy[Block == 3 & Condition == 4], na.rm = TRUE), ",
                            
                            Task, "_GcosthRT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 3], na.rm = TRUE) - mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 1], na.rm = TRUE),",
                            Task, "_GcosthAC = mean(Accuracy[Block != 0 & Condition == 3], na.rm = TRUE) - mean(Accuracy[Block != 0 & Condition == 1], na.rm = TRUE),",

                            Task, "_GcostfRT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 4], na.rm = TRUE) - mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 2], na.rm = TRUE),",
                            Task, "_GcostfAC = mean(Accuracy[Block != 0 & Condition == 4], na.rm = TRUE) - mean(Accuracy[Block != 0 & Condition == 2], na.rm = TRUE),",

                            Task, "_GcostRT_ro = mean(RT_ro[Block == 3 & Accuracy == 1], na.rm = TRUE) - mean(RT_ro[(Block == 1 | Block == 2) & Accuracy == 1], na.rm = TRUE),",
                            Task, "_GcostAC = mean(Accuracy[Block == 3], na.rm = TRUE) - mean(Accuracy[Block == 1 | Block == 2], na.rm = TRUE))"

                            # "MixheartRT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 3], na.rm = TRUE),",
                            # "MixflowerRT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 4], na.rm = TRUE),",
                            # "GcosthRT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 3], na.rm = TRUE) - mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 1], na.rm = TRUE),",
                            # "GcostfRT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 4], na.rm = TRUE) - mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 2], na.rm = TRUE),",
                            # "GcostRT_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Block == 3], na.rm = TRUE) - ((mean(RT_ro[Block != 0 & Accuracy == 1 & Block == 1], na.rm = TRUE) + mean(RT_ro[Block != 0 & Accuracy == 1 & Block == 2], na.rm = TRUE))/2),",

                            #pc = pure congruent
                            # Task, "_pcrt = mean(RT[Block != 0 & Accuracy == 1 & Condition == 1], na.rm = TRUE), " ,
                            # Task, "_pcrt_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 1], na.rm = TRUE), " ,
                            # Task, "_pcac = mean(Accuracy[Block != 0 & Condition == 1], na.rm = TRUE), " ,
                            # Task, "_picrt = mean(RT[Block != 0 & Accuracy == 1 & Condition == 2], na.rm = TRUE), " ,
                            # Task, "_picrt_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 2], na.rm = TRUE), " ,
                            # Task, "_picac = mean(Accuracy[Block != 0 & Condition == 2], na.rm = TRUE), " ,
                            # Task, "_icrt = mean(RT[Block != 0 & Accuracy == 1 & Condition == 2] - RT[Block != 0 & Accuracy == 1 & Condition == 1], na.rm = TRUE), " ,
                            # Task, "_icrt_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & Condition == 2] - RT[Block != 0 & Accuracy == 1 & Condition == 1], na.rm = TRUE), " ,
                            # Task, "_icac = mean(Accuracy[Block != 0 & Condition == 2] - Accuracy[Block != 0 & Condition == 1], na.rm = TRUE), " ,
                            #
                            # Task, "_Hgrt = GcosthRT, " ,
                            # Task, "_Hgac = GcosthAC, " ,
                            # Task, "_Fgrt = GcostfRT, " ,
                            # Task, "_Fgac = GcostfAC, " ,
                            # Task, "_Gcostrt = GcostRT, " ,
                            # Task, "_Gcostac = GcostAC, " ,

                            #Block -> 0:practice, 1:pure heart, 2:pure flower, 3:mix
                            #Condition -> 1:pure heart 2:pure flower
                            #             3:heart in mix block  4:flower in mix block

                            # Task, "_Hmrt = mean(RT[Block != 0 & Accuracy == 1 & (Condition %in% c(1,3) )], na.rm = TRUE), " ,
                            # Task, "_Hmrt_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & (Condition %in% c(1,3) )], na.rm = TRUE), " ,
                            # Task, "_Hmac = mean(Accuracy[Block != 0 & (Condition %in% c(1,3) )], na.rm = TRUE), " ,
                            # Task, "_Fmrt = mean(RT[Block != 0 & Accuracy == 1 & (Condition %in% c(2,4) )], na.rm = TRUE), " ,
                            # Task, "_Fmrt_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & (Condition %in% c(2,4) )], na.rm = TRUE), " ,
                            # Task, "_Fmac = mean(Accuracy[Block != 0 & (Condition %in% c(2,4) )], na.rm = TRUE), " ,
                            #
                            # Task, "_costrt = mean(RT[Block != 0 & Accuracy == 1 & (Condition %in% c(3,4) )] - RT[Block != 0 & Accuracy == 1 & (Condition %in% c(1,2) )], na.rm = TRUE), " ,
                            # Task, "_costrt_ro = mean(RT_ro[Block != 0 & Accuracy == 1 & (Condition %in% c(3,4) )] - RT_ro[Block != 0 & Accuracy == 1 & (Condition %in% c(1,2) )], na.rm = TRUE), " ,
                            # Task, "_costac = mean(Accuracy[Block != 0 & (Condition %in% c(3,4) )] - Accuracy[Block != 0 & (Condition %in% c(1,2) )]))"
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
  print(mutate_string)
  return(tmpdf)
}

# cog_mutate <- function (df, Task)
# {
#   ## SRTCRT ----
#   if (Task == "SRTCRT"){
#
#     mutate_string <- "
#     mutate(.,
#     SRT_mrt = mean(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),
#     SRT_vrt = var(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),
#     CRT_mrt = mean(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),
#     CRT_vrt = var(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),
#     SRT_mac = mean(Accuracy[StimulusLocation == 5 & Block != 0]),
#     SRT_vac = var(Accuracy[StimulusLocation == 5 & Block != 0]),
#     CRT_mac = mean(Accuracy[StimulusLocation != 5 & Block != 0]),
#     CRT_vac = var(Accuracy[StimulusLocation != 5 & Block != 0]))"
#   }
#
#   ## MA/MAO ----
#   else if (Task == "MA" || Task == "MAO"){
#     mutate_string <- paste0("mutate(., ",
#                             Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ",
#                             Task, "_mac = mean(Accuracy[Block != 0]), ",
#                             Task, "_ca = yourSPAN) ")
#   }
#
#   ## DMS ----
#   else if (Task == "DMS"){
#     mutate_string <- "mutate(.)"
#   }
#
#   ## DR ----
#   else if (Task == "DR"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_11rt = e1t1RT, ",
#                             Task, "_11ac = e1t1ACC, ",
#                             Task, "_14rt = e1t4RT, ",
#                             Task, "_14ac = e1t4ACC, ",
#                             Task, "_41rt = e4t1RT, ",
#                             Task, "_41ac = e4t1ACC, ",
#                             Task, "_44rt = e4t4RT, ",
#                             Task, "_44ac = e4t4ACC, ",
#
#                             Task, "_mrt = mean(RT[Block != 0 & ACC == 1]), ",
#                             Task, "_mac = mean(ACC[Block != 0]), ",
#                             Task, "_Coca = 4 * (e4t1ACC/100 - (1 - mean(ACC[Block != 0 & Condition == 'Mismatch']))), ",
#                             Task, "_Paca = 4 * (e4t4ACC/100 - (1 - mean(ACC[Block != 0 & Condition == 'Mismatch'])))) "
#                             )
#   }
#
#   ## SM ----
#   else if (Task == "SM"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ",
#                             Task, "_mac = mean(Accuracy[Block != 0]), ",
#                             Task, "_ca = yourSPAN) ")
#   }
#   ## RMS/RMO ----
#   else if (Task == "RMS" || Task == "RMO"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ",
#                             Task, "_mac = mean(Accuracy[Block != 0]), ",
#                             Task, "_ca = yourSPAN) ")
#   }
#   ## RML ----
#   else if (Task == "RML"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ",
#                             Task, "_mac = mean(Accuracy[Block != 0]), ",
#                             Task, "_ca = yourSPAN) ")
#   }
#   ## SST ----
#   else if (Task == "SST"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_mrt = goRT - pgoRT, ",
#                             Task, "_mac = goACC - pgoACC, ",
#                             Task, "_int = ThresholdStopTrials, ",
#                             Task, "_ac = stopACC)")
#   }
#   ## Sp ----
#   else if (Task == "Sp"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_ICNrt = ICRT - NRT, " ,
#                             Task, "_ICNac = ICACC - NACC)"
#                             )
#   }
#
#   ## As ----
#   else if (Task == "As"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_mrt = MeanRT, " ,
#                             Task, "_mac = MeanAC)"
#     )
#   }
#
#   ## CTT ----
#   else if (Task == "CTT"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_sart = SaRT, " ,
#                             Task, "_saac = SaAC, " ,
#                             Task, "_swrt = switchRT - nonSwitchRT, " ,
#                             Task, "_swac = SwitchAC)"
#     )
#   }
#
#   #"_swrt = switchRT - nonSwitchRT, " due to app error
#
#   ## Fg ----
#   else if (Task == "Fg"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_rprt = repRT, " ,
#                             Task, "_rpac = repACC, " ,
#                             Task, "_swrt = swiRT, " ,
#                             Task, "_swac = swiACC, " ,
#                             Task, "_cort = (swiRT - repRT), " ,
#                             Task, "_coac = (swiACC - repACC))"
#     )
#   }
#
#   ## HF ----
#   else if (Task == "HF"){
#     mutate_string <- paste0("mutate(.,",
#                             #pc = pure congruent
#                             Task, "_pcrt = mean(RT[Block != 0 & Accuracy == 1 & Condition == 1]), " ,
#                             Task, "_pcac = mean(Accuracy[Block != 0 & Condition == 1]), " ,
#                             Task, "_picrt = mean(RT[Block != 0 & Accuracy == 1 & Condition == 2]), " ,
#                             Task, "_picac = mean(Accuracy[Block != 0 & Condition == 2]), " ,
#                             Task, "_icrt = mean(RT[Block != 0 & Accuracy == 1 & Condition == 2] - RT[Block != 0 & Accuracy == 1 & Condition == 1]), " ,
#                             Task, "_icac = mean(Accuracy[Block != 0 & Condition == 2] - Accuracy[Block != 0 & Condition == 1]), " ,
#
#                             Task, "_Hgrt = GcosthRT, " ,
#                             Task, "_Hgac = GcosthAC, " ,
#                             Task, "_Fgrt = GcostfRT, " ,
#                             Task, "_Fgac = GcostfAC, " ,
#                             Task, "_Gcostrt = GcostRT, " ,
#                             Task, "_Gcostac = GcostAC, " ,
#
#                             #Block -> 0:practice, 1:pure heart, 2:pure flower, 3:mix
#                             #Condition -> 1:pure heart 2:pure flower
#                             #             3:heart in mix block  4:flower in mix block
#
#                             Task, "_Hmrt = mean(RT[Block != 0 & Accuracy == 1 & (Condition %in% c(1,3) )]), " ,
#                             Task, "_Hmac = mean(Accuracy[Block != 0 & (Condition %in% c(1,3) )]), " ,
#                             Task, "_Fmrt = mean(RT[Block != 0 & Accuracy == 1 & (Condition %in% c(2,4) )]), " ,
#                             Task, "_Fmac = mean(Accuracy[Block != 0 & (Condition %in% c(2,4) )]), " ,
#
#                             Task, "_costrt = mean(RT[Block != 0 & Accuracy == 1 & (Condition %in% c(3,4) )] - Accuracy[Block != 0 & Accuracy == 1 & (Condition %in% c(1,2) )]), " ,
#                             Task, "_costac = mean(Accuracy[Block != 0 & (Condition %in% c(3,4) )] - Accuracy[Block != 0 & (Condition %in% c(1,2) )]))"
#     )
#   }
#
#   ## RS ----
#   else if (Task == "RS"){
#     mutate_string <- paste0("mutate(.,",
#                             Task, "_ca = yourSPAN, " ,
#                             #this task no Trial
#                             Task, "_armrt = mean(ArrowRT[Trial != 0 & OverallACC == 1 & Condition == 'Arrow']))"
#     )
#   }
#
#   tmpdf <- df %>%
#     group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
#     eval(parse(text = mutate_string), envir = .)
#
#   return(tmpdf)
# }
