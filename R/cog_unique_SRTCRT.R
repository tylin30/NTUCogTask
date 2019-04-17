cog_unique_SRTCRT <- function(df){
  tmpdf <- df %>%
    group_by(Subject, Gender, Age, Education, Hand, Seed) %>%
    summarise(
      SRT_mrt = unique(SRT_mrt),
      SRT_vrt = unique(SRT_vrt),
      CRT_mrt = unique(CRT_mrt),
      CRT_vrt = unique(CRT_vrt),
      SRT_mac = unique(SRT_mac),
      SRT_vac = unique(SRT_vac),
      CRT_mac = unique(CRT_mac),
      CRT_vac = unique(CRT_vac)
    )
  return(tmpdf)
}
