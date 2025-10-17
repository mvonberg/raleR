#' RRT item battery
#'
#' @description Create a battery of items for the Room Recognition Test (RRT), including instructions, item battery, scoring and feedback.
#' Within the battery, \code{N_items} are randomly drawn from the supplied \code{range}. Items are administered in a random order.
#' Expects a web directory with audio files that are named after the pattern "\code{source}<Nr>_<itemNr>.<format>".
#' This function returns a \code{psychTestR::module()} object that can be used in a \code{psychTestR} test.
#'
#' @param N_items number of items in the test battery. If \code{targets} is provided, this argument is ignored.
#'
#' @param url_dir URL to the directory with the audio files.
#'
#' @param source source Identifier in the audio file names. Should include everything in the file name except for the source and stimulus number.
#'
#' @param N_source number of different source signals. If 3, all sounds A, B and X have different source signals. If 1, all have the same.
#' If 2, A and B have the same source and X is different.
#'
#' @param baseline Which stimulus item number should be equal in all items?
#'
#' @param range numeric vector with item numbers that can be used in the test battery. Length must be larger than or equal to \code{N_items}.
#'
#' @param audio_file_type file extension for the audio files in \code{url_dir}.
#'
#' @param label Label for the items in the test battery. Item numbers will be added automatically.
#'
#' @param labels_AB Labels for the buttons toggling the audio playback for sounds A and B.
#'
#' @param label_X Label for the button toggling the audio playback for sound X.
#'
#' @param dict \code{i18n} dictionary used for interationalisation.
#'
#' @param ... Other arguments passed on to \code{\link{RRT_abx_page}()}.
#'
#' @export

RRT_battery <- function(N_items=20,
                        url_dir,
                        source="RRTstim",
                        N_source=3,
                        baseline=0,
                        range=0:N_items,
                        audio_file_type="mp3",
                        label="RRT",
                        labels_AB=c("Play A","Play B"),
                        label_X="Play X",
                        dict=raleR::RALE_dict,
                        ...) {

  # get audio urls
  if (is.na(match(baseline,range))) {
    item_stimuli <- range
  } else {
    item_stimuli <- range[-match(baseline,range)]
  }
  item_url <- audioURLs_ABX(url_dir,source,N_source,item_stimuli,audio_file_type)

  base_url <- audioURLs_ABX(url_dir,source,N_source,baseline,audio_file_type)

  idx_item_test <- match(max(abs(item_stimuli-baseline)),abs(item_stimuli-baseline))
  url_A_test <- item_url[3,idx_item_test]
  url_B_test <- base_url[2,1]
  url_X_test <- c(base_url[1,1],item_url[1,idx_item_test])

  # randomize item order
  itemOrder <- 1:ncol(item_url)#sample(1:ncol(item_url),N_items)
  item_url <- item_url[,itemOrder]
  #print(item_url) # for debugging

  psychTestR::module(label,
                     psychTestR::join(
                       RRT_demo_page(url_A=url_A_test,
                                     url_B=url_B_test,
                                     url_X=url_X_test,
                                     labels_AB=labels_AB,
                                     label_X = label_X,
                                     dict=dict
                       ),
                       RRT_init_item_config,
                       item_battery <- psychTestR::randomise_at_run_time(
                         label=paste(label,"item_order",sep="_"),
                         Map(RRT_abx_page,
                             url_A=item_url,
                             label=label,
                             item_num=itemOrder,
                             MoreArgs=list(
                               url_B=base_url,
                               labels_AB=labels_AB,
                               label_X=label_X,
                               dict=dict,
                               ...))
                       ),
                       RRT_get_raw_score,
                       RRT_feedback(dict=dict),
                       psychTestR::elt_save_results_to_disk(complete=TRUE))
  )
}
