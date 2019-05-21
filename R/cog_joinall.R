# cog_joinall <- function(dflist = list(srtcrt_df, ma_df, mao_df, dms_df, dr_df,
#                                       sm_df, rms_df, rmo_df, rml_df, sst_df,
#                                       as_df, ctt_df, fg_df, hf_df, rs_df,
#                                       sp_df)){
#
#   #temp method for join (due to typo error from experimenter)
#   delete <- c("Gender", "Age", "Education", "Hand", "Seed")
#   tmp <- lapply(dflist, function(x) {x <- x[, !(colnames(x) %in% delete), drop=FALSE] })
#
#   all_df <- multi_join(tmp, full_join)
# }
