cog_qqplot <- function(df, title_label = ""){
  for (i in 1:ncol(df)){
    if (is.numeric(df[[i]])){
      plot <- ggplot(selected_rt, aes(sample=selected_rt[[i]])) +
        geom_qq(distribution = qnorm) +
        geom_qq_line(col = "blue") +
        ylab(names(selected_rt)[i]) +
        ggtitle(paste(names(selected_rt)[i], title_label,"Q-Qplot")) +
        theme(plot.title = element_text(hjust = 0.5))
      print(plot)
    }
  }
}
