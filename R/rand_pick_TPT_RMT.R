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
  pick <- sample(c("TPT", "RMT"),1)
  if (pick=="TPT") {

    # some manual tweaks for the TPT dict
    tptR::TPT_dict$edit(key = "INSTRUCTIONS2_ITEM4",language = "de", new = "")
    tptR::TPT_dict$edit(key = "INSTRUCTIONS2_ITEM3",language = "de", new = "Spielen Sie zu Beginn des Blocks mit dem Slider, um zu testen wie sich der Klang ver\u00e4ndert.")
    tptR::TPT_dict$edit(key = "INSTRUCTIONS2_ITEM5",language = "de", new = "")
    tptR::TPT_dict$edit(key = "BEGIN_PRACTICE",language = "de", new = "Test starten")

    tptR::TPT(title = "TPT",with_welcome = TRUE,with_training = FALSE)
  } else {
    RMT_battery(label = "rmt", N_items = RMT_N_items, targets = RMT_targets, stm_base = paste0(RMT_url_dir, RMT_src),
                min = RMT_min, max = RMT_max, sliderLength = RMT_sliderLength, dict=raleR::RALE_dict
    )
  }
}
