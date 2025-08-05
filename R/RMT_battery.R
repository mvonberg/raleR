#' RMT item battery
#'
#' @description Create a battery of items for the Reverberation Matching Test (RMT), including instructions, item battery,
#' scoring and feedback. Expects a web directory with audio files that are named after the pattern "\code{source}<Nr>_<itemNr>.<format>".
#' Targets for the items are either provided manually or generated randomily within specified margins ensuring that the target is never the end point of a slider range.
#' This function returns a \code{psychTestR::module()} object that can be used in a \code{psychTestR} test.
#'
#' @param N_items number of items in the test battery. If \code{targets} is provided, this argument is ignored.
#'
#' @param label label for the items in the test battery. Item numbers will be added automatically
#'
#' @param pool_sounds character vector with audio files that can be used in the test battery. If \code{NULL}, the function will
#' create a pool of sounds based on \code{stm_base}, \code{min}, \code{max} and \code{format_numbers}.
#'
#' @param stm_base "body" for audio files that are mapped onto the slider. If used, files must be named "body<Nr>.\code{file_ext}"
#' and \code{min} and \code{max} have to be defined. Mutually exclusive with \code{pool_sounds}.
#'
#' @param min minimum value for the slider. Only required if \code{stm_base} is used rather than \code{pool_sounds}.
#'
#' @param max maximum value for the slider. Only required if \code{stm_base} is used rather than \code{pool_sounds}.
#'
#' @param sliderLength length of the slider, i.e. number of sounds that can be selected. Default is the length of
#' \code{pool_sounds} so that each item presents the full scale.
#'
#' @param targets numeric vector with indices of sounds that should be used as targets in the test battery. If \code{NULL},
#' the function will sample \code{N_items} targets from \code{pool_sounds}.
#'
#' @param target_range numeric vector with indices of sounds that can be used as targets in the test battery. If \code{NULL},
#' the function will sample from the full range of \code{pool_sounds}.
#'
#' @param target_margins numeric value that defines the margins around the target that should be excluded from the sampled range.
#'
#' @param show_trial_number logical value that defines whether the trial number should be displayed in the prompt header.
#'
#' @param format_numbers format string for adding numbers to the base URL when building \code{pool_sounds}
#' (e.g. "%02d" for 2 digits integers).
#'
#' @param file_ext file extension for the audio files in \code{pool_sounds}. Default is "mp3".
#'
#' @param dict dictionary object used for internationalisation.
#'
#' @param ... additional arguments that are passed to \code{\link{audio_slider_page}()}.
#'
#' @export
RMT_battery <- function(N_items=NULL,
                        label="RMT",
                        pool_sounds=NULL,
                        stm_base=NULL,
                        min=NULL,
                        max=NULL,
                        sliderLength=length(pool_sounds),
                        targets=NULL,
                        target_range=NULL,
                        target_margins=3,
                        show_trial_number=TRUE,
                        format_numbers="%02d",
                        file_ext="mp3",
                        dict = raleR::RALE_dict,
                        ...
) {
  # make sure either TARGETS or N_ITEMS is provided
  if (is.null(targets) & is.null(N_items)) {
    stop("No number of test items provided. Please specify either `targets` or `N_items` argument.")
  }

  # create pool_sounds arrays if necessary
  if (is.null(pool_sounds)) {
    pool_sounds <- file_link_array(base=stm_base,min=min,max=max,format_numbers=format_numbers,file_ext=file_ext)
  }

  if(is.null(targets)) { # sample random targets if targets is empty
    if (is.null(target_range)) {target_range <- 1:length(pool_sounds)} # sample from full sound pool if target_range is empty
    targets <- constrained_sample(range=target_range,N=N_items,margin=target_margins)
  }
  #print(targets)

  if (is.numeric(targets)) {
    targets <- pool_sounds[targets]
  }

  itemOrder <- sample(1:length(targets))
  targets <- targets[itemOrder] # randomize item order

  sliderSounds <- lapply(targets,sample_range,vector_in=pool_sounds,range_length=sliderLength,target_margin=target_margins)

  item_labels <- paste0(label,itemOrder)

  RMT_item_battery <- mapply(audio_slider_page,
                             label=item_labels,
                             item_idx=paste0(1:length(item_labels),"/",length(item_labels)),
                             ref_src=targets,
                             sliderSounds=sliderSounds,
                             value=round(sliderLength/2),
                             MoreArgs=list(dict=dict,
                                           ...))

  psychTestR::module("RMT",
                     psychTestR::join(RMT_instructions(dict=dict),
                                      RMT_item_battery,
                                      RMT_get_score,
                                      RMT_feedback(dict=dict),
                                      psychTestR::elt_save_results_to_disk(complete=TRUE))
                     )
}
