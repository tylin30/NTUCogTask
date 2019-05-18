cog_tidy <- function(datapath, Task){
  #raw df
  raw_df <- cog_rbindobs(datapath, Task) %>%
    cog_datatype(., Task) %>%
    cog_mutate(., Task)

  #unique df
  unique_df <- raw_df %>% cog_unique(., Task)

  return(unique_df)

  # need to rewirte to save two RDS in saveRDS command
  # datalist = list(raw_df, unique_df)
  # saveRDS(datalist, file = paste0(Task,".RDS"))
}
