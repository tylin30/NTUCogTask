cog_datatype <- function(df, Task){
  #which task
  if (Task == "SRTCRT"){
    factorlist <- list("Block", "Trial", "StimulusLocation", "ResponseLocation")
    numericlist <- list("Accuracy", "RT", "SRT", "CRT")
  }


  #common data datatype
  df <- transform(df,
                  Subject = as.factor(Subject),
                  Gender = as.factor(Gender),
                  Age = as.numeric(Age),
                  Education = as.factor(Education),
                  Hand = as.factor(Hand),
                  Seed = as.factor(Seed))

  #input factor list
  for (i in factorlist){
    df[[i]] <- factor(df[[i]])
  }

  #input numeric list
  for (i in numericlist){
    df[[i]] <- as.numeric(df[[i]])
  }

  return(df)
}
