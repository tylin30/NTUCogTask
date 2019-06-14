#new col, outlier = NaN
cog_addnooutlier_test <- function(df, Task, range = 2.5){
  if (Task == "SRTCRT"){
    tmpdf <- df %>%
      mutate(Condition = ifelse(StimulusLocation == 5, "SRT", "CRT")) %>%
      group_by(Subject, Condition) %>%
      filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }

  if(Task == "DMS"){
    tmpdf <- df %>%
      group_by(Subject, Delay) %>%
      filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }

  if(Task == "DR"){
    tmpdf <- df %>%
      group_by(Subject, Encode, Test) %>%
      filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[ACC == 1]) + range*sd(RT[ACC == 1]) |
          RT <= mean(RT[ACC == 1]) - range*sd(RT[ACC == 1]),
        NaN, RT
      )
      )
  }

  if(Task == "SST"){
    tmpdf <- df %>%
      group_by(Subject) %>%
      filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Accuracy == 1], na.rm = TRUE) + range*sd(RT[Accuracy == 1], na.rm = TRUE) |
          RT <= mean(RT[Accuracy == 1], na.rm = TRUE) - range*sd(RT[Accuracy == 1], na.rm = TRUE),
        NaN, RT
      )
      )
  }

  if(Task == "As"){
    tmpdf <- df %>%
      group_by(Subject) %>%
      filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }

  if(Task == "Fg"){
    tmpdf <- df %>%
      group_by(Subject, Block) %>%
      filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }

  if(Task == "HF"){
    tmpdf <- df %>%
      group_by(Subject, Block) %>%
      filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }

  if(Task == "Sp"){
    tmpdf <- df %>%
      group_by(Subject, Condition) %>%
      filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }

  return(tmpdf)
}
