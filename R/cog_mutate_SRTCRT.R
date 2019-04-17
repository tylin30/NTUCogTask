cog_mutate_SRTCRT <- function(df){
  tmp <- df %>%
    group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
    filter(Block != 0) %>%
    mutate(
      SRT_mac  = mean(Accuracy[StimulusLocation == 5]),
      SRT_vac = var(Accuracy[StimulusLocation == 5]),
      CRT_mac = mean(Accuracy[StimulusLocation != 5]),
      CRT_vac = var(Accuracy[StimulusLocation != 5])
    )
  return(tmp)
}
