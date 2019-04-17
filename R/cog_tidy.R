cog_tidy <- function(datapath, pattern, Task){
  #raw df
  raw_df <- cog_rbindobs(datapath, pattern) %>%
    cog_datatype(., Task) %>%
    cog_mutate(., Task)

  #unique df
  unique_df <- raw_df %>% cog_unique(., Task)

  datalist = list(raw_df, unique_df)
  saveRDS(datalist, file = paste0(Task,".RDS"))
}
