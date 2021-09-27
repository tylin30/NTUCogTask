#new col, outlier = NA
cog_addnooutlier <- function(df, Task, range = 2.5){
  min_rt <- 150
  if (Task == "SRTCRT"){
    df <- df %>%
      mutate(Condition = ifelse(StimulusLocation == 5, "SRT", "CRT")) %>%
      group_by(Subject, Condition) %>%
      # filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Block != 0 & Accuracy == 1]) + range*sd(RT[Block != 0 & Accuracy == 1]) |
          RT <= mean(RT[Block != 0 & Accuracy == 1]) - range*sd(RT[Block != 0 & Accuracy == 1])|
          RT <= min_rt,
        NA, RT
      )
      )

  }

  if(Task == "DMS"){
    df <- df %>%
      group_by(Subject, Delay) %>%
      # filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Block != 0 & Accuracy == 1]) + range*sd(RT[Block != 0 & Accuracy == 1]) |
          RT <= mean(RT[Block != 0 & Accuracy == 1]) - range*sd(RT[Block != 0 & Accuracy == 1])|
          RT <= min_rt,
        NA, RT
      )
      )
  }

  if(Task == "DR"){
    df <- df %>%
      group_by(Subject, Encode, Test) %>%
      # filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Block != 0 & ACC == 1]) + range*sd(RT[Block != 0 & ACC == 1]) |
          RT <= mean(RT[Block != 0 & ACC == 1]) - range*sd(RT[Block != 0 & ACC == 1])|
          RT <= min_rt,
        NA, RT
      )
      )
  }

  if(Task == "SST"){
    df <- df %>%
      group_by(Subject, Block) %>%
      # filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Block != 0 & Accuracy == 1], na.rm = TRUE) + range*sd(RT[Block != 0 & Accuracy == 1], na.rm = TRUE) |
          RT <= mean(RT[Block != 0 & Accuracy == 1], na.rm = TRUE) - range*sd(RT[Block != 0 & Accuracy == 1], na.rm = TRUE),
        NA, RT
      )
      )
  }

  if(Task == "As"){
    df <- df %>%
      group_by(Subject) %>%
      # filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Block != 0 & Accuracy == 1]) + range*sd(RT[Block != 0 & Accuracy == 1]) |
          RT <= mean(RT[Block != 0 & Accuracy == 1]) - range*sd(RT[Block != 0 & Accuracy == 1])|
          RT <= min_rt,
        NA, RT
      )
      )
  }

  if(Task == "Fg"){
    df <- df %>%
      group_by(Subject, Block) %>%
      # filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Block != 0 & Accuracy == 1]) + range*sd(RT[Block != 0 & Accuracy == 1]) |
          RT <= mean(RT[Block != 0 & Accuracy == 1]) - range*sd(RT[Block != 0 & Accuracy == 1])|
          RT <= min_rt,
        NA, RT
      )
      )
  }

  if(Task == "HF"){
    df <- df %>%
      group_by(Subject, Block) %>%
      # filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Block != 0 & Accuracy == 1]) + range*sd(RT[Block != 0 & Accuracy == 1]) |
          RT <= mean(RT[Block != 0 & Accuracy == 1]) - range*sd(RT[Block != 0 & Accuracy == 1])|
          RT <= min_rt,
        NA, RT
      )
      )
  }
  
   # not duplicate, group by pure, mix
  if(Task == "HF"){
    df <- df %>%
      mutate(Group = ifelse((Condition == 1 | Condition == 2), "pure", ifelse((Condition == 3 | Condition == 4), 'mix', 'practice'))) %>%
      group_by(Subject, Group) %>%
      mutate(RT_condition_ro = ifelse(
        RT >= mean(RT[ Accuracy == 1]) + range*sd(RT[ Accuracy == 1]) |
          RT <= mean(RT[Accuracy == 1]) - range*sd(RT[Accuracy == 1])|
          RT <= min_rt,
        NA, RT
      )
      )
  }

  if(Task == "Sp"){
    df <- df %>%
      group_by(Subject, Condition) %>%
      # filter(Block != 0) %>%
      mutate(RT_ro = ifelse(
        RT >= mean(RT[Block != 0 & Accuracy == 1]) + range*sd(RT[Block != 0 & Accuracy == 1]) |
          RT <= mean(RT[Block != 0 & Accuracy == 1]) - range*sd(RT[Block != 0 & Accuracy == 1])|
          RT <= min_rt,
        NA, RT
      )
      )
  }

  return(df)
}
