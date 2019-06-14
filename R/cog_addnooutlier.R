#new col, outlier = NaN
cog_addnooutlier <- function(df, Task, range = 2.5){
  if (Task == "SRTCRT"){
    df %>% 
      mutate(Condition = ifelse(StimulusLocation == 5, "SRT", "CRT")) %>%
      group_by(Subject, Condition) %>%
      filter(Block != 0) %>%
      mutate(RT_removeoutlier = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }
  
  if(Task == "DMS"){
    df %>%
      group_by(Subject, Delay) %>%
      filter(Block != 0) %>%
      mutate(RT_removeoutlier = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }
  
  if(Task == "DR"){
    df %>%
      group_by(Subject, Encode, Test) %>%
      filter(Block != 0) %>%
      mutate(RT_removeoutlier = ifelse(
        RT >= mean(RT[ACC == 1]) + range*sd(RT[ACC == 1]) |
          RT <= mean(RT[ACC == 1]) - range*sd(RT[ACC == 1]),
        NaN, RT
      )
      )
  }
  
  if(Task == "SST"){
    df %>%
      group_by(Subject) %>%
      filter(Block != 0) %>%
      mutate(RT_removeoutlier = ifelse(
        RT >= mean(RT[Accuracy == 1], na.rm = TRUE) + range*sd(RT[Accuracy == 1], na.rm = TRUE) |
          RT <= mean(RT[Accuracy == 1], na.rm = TRUE) - range*sd(RT[Accuracy == 1], na.rm = TRUE),
        NaN, RT
      )
      )
  }
  
  if(Task == "As"){
    df %>%
      group_by(Subject) %>%
      filter(Block != 0) %>%
      mutate(RT_removeoutlier = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }
  
  if(Task == "Fg"){
    df %>%
      group_by(Subject, Block) %>%
      filter(Block != 0) %>%
      mutate(RT_removeoutlier = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }
  
  if(Task == "HF"){
    df %>%
      group_by(Subject, Block) %>%
      filter(Block != 0) %>%
      mutate(RT_removeoutlier = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }
  
  if(Task == "Sp"){
    df %>%
      group_by(Subject, Condition) %>%
      filter(Block != 0) %>%
      mutate(RT_removeoutlier = ifelse(
        RT >= mean(RT[Accuracy == 1]) + range*sd(RT[Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1]),
        NaN, RT
      )
      )
  }
  
  return(df)
  
}