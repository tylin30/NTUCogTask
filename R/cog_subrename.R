cog_subrename <- function(df) {
  sublevels <- levels(df$Subject)
  for (i in 1:length(sublevels)){
    if (grepl("^ncku\\d*[a-zA-Z]", levels(df$Subject)[i], ignore.case = TRUE)){

      levels(df$Subject)[i] <- substr(sublevels[i], 1, nchar(sublevels[i])-1)

    }
  }
  return(df)
}
