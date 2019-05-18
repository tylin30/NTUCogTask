cog_subrename <- function(df) {
  for (i in 1:nrow(df)){
    if (startsWith(as.character(df$Subject[i]), "ncku")){
      df$Subject[i] <- substr(df$Subject[i], 1, 7)
    }
  }
}