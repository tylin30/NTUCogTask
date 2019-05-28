cog_hist <- function (df, title_label = "") 
{
  if (title_label != "") {
    title_label <- paste0("(", title_label, ")")
  }
  for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]])) {
      plot <- ggplot(data=df, aes(unlist(df[,i]))) + 
        geom_histogram(aes(y =..density..), 
                       col="red", 
                       fill="green", 
                       alpha = .2) + 
        geom_density(col=2) + 
        labs(title=paste("Histogram",colnames(df)[i])) +
        labs(x=colnames(df)[i]) +
        theme(plot.title = element_text(hjust = 0.5))
      print(plot)
    }
  }
}