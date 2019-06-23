cog_correlation <- function(cormatrix, alpha = 0.05, filename = "cor", path = "./"){
  cor <- as_tibble(cormatrix$r)
  cor_p <- as_tibble(cormatrix$P)
  cor_remove <- cor

  for( i in c(1:ncol(cor)) ){
    for ( j in c(1:nrow(cor))){
      if (is.na(cor_p[[i]][j])){

      }else if (cor_p[[i]][j] > alpha){
        cor_remove[[i]][j] <- ""
      }
    }
  }
  print(paste0(path, filename,".xlsx"))
  #output to excel file
  # Write the first data set in a new workbook
  write.xlsx2(cormatrix$r, file = paste0(path, "/", filename,".xlsx"),
              sheetName = "cor", append = FALSE)
  # Add a second data set in a new worksheet
  write.xlsx2(cormatrix$P, file = paste0(path, "/", filename,".xlsx"),
              sheetName="cor_p", append=TRUE)
  # Add a third data set
  write.xlsx2(cor_remove, file = paste0(path, "/", filename,".xlsx"),
              sheetName="cor_remove", append=TRUE)
}
