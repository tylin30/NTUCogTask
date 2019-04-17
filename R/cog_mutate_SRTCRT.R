cog_mutate_SRTCRT <- function (df)
{
  tmpdf <- df %>%
    group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
    # filter(Block != 0) %>%
    mutate(
      SRT_mrt = mean(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),
      SRT_vrt = var(RT[StimulusLocation == 5 & Block != 0 & Accuracy == 1]),
      CRT_mrt = mean(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),
      CRT_vrt = var(RT[StimulusLocation != 5 & Block != 0 & Accuracy == 1]),
      SRT_mac = mean(Accuracy[StimulusLocation == 5 & Block != 0]),
      SRT_vac = var(Accuracy[StimulusLocation == 5 & Block != 0]),
      CRT_mac = mean(Accuracy[StimulusLocation != 5 & Block != 0]),
      CRT_vac = var(Accuracy[StimulusLocation != 5 & Block != 0])
    )
  return(tmpdf)
}
