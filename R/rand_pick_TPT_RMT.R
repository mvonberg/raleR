#' Randomly choose either the TPT or RMT
#'
#' @description Creates either a TPT or an RMT instance. The selection is random.
#'
#' @param TPT_with_welcome logical value indicating whether the TPT should include a welcome page.
#'
#' @param TPT_with_training logical value indicating whether the TPT should include a training phase.
#'
#' @param RMT_url_dir,RMT_src,RMT_N_items,RMT_targets,RMT_min,RMT_max,RMT_sliderLength RMT configuration parameters that are passed on to [RMT_battery].
#'
#' @export

rand_pick_TPT_RMT <- function(TPT_with_welcome=TRUE,
                              TPT_with_training=TRUE,
                              RMT_url_dir,
                              RMT_src="Git2_",
                              RMT_N_items=NULL,
                              RMT_targets=NULL,
                              RMT_min=0,
                              RMT_max=50,
                              RMT_sliderLength=20) {

  # overwrite "de" with "de_f" in the TPT dict because psyquest only supports the former
  stored.TPT_dict <- tptR::TPT_dict$as.data.frame()
  for (i in 1:nrow(stored.TPT_dict)) {
    tptR::TPT_dict$edit(key = stored.TPT_dict$key[i],
                        language = "de",
                        new = stored.TPT_dict$de_f[i])
  }

  psychTestR::join(psychTestR::conditional(include_for_participant("a"),
                                                   tptR::TPT(title = "TPT",with_welcome = TRUE,with_training = TRUE)),
                           psychTestR::conditional(include_for_participant("b"),
                                                   RMT_battery(label = "rmt", N_items = RMT_N_items, targets = RMT_targets,
                                                               stm_base = paste0(RMT_url_dir, RMT_src),
                                                               min = RMT_min, max = RMT_max,
                                                               sliderLength = RMT_sliderLength, dict=raleR::RALE_dict
                                                               )
                                                   )
                           )
}
