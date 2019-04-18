cog_mutate <- function (df, Task)
{
  if (Task == "SRTCRT"){
    mutate_string <- "mutate(., SRT_mrt = mean(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),SRT_vrt = var(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),CRT_mrt = mean(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),CRT_vrt = var(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),SRT_mac = mean(Accuracy[StimulusLocation == 5 & Block != 0]),SRT_vac = var(Accuracy[StimulusLocation == 5 & Block != 0]),CRT_mac = mean(Accuracy[StimulusLocation != 5 & Block != 0]),CRT_vac = var(Accuracy[StimulusLocation != 5 & Block != 0]))"
  }
  else if (Task == "DMS"){
    mutate_string <- "mutate(.)"
  }

  else if (Task == "MA" || Task == "MAS"){
    mutate_string <- paste0("mutate(., ", Task, "_mrt = mean(RT[Block != 0 & Accuracy == 1]), ", Task, "_mac = mean(Accuracy[Block != 0]), ", Task, "_ca = yourSPAN) ")
  }

  tmpdf <- df %>%
    group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
    eval(parse(text = mutate_string), envir = .)

  return(tmpdf)
}
