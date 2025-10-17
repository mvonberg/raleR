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
#' @param sliderSounds list with one or multiple character vectors with audio files for the slider. If \code{NULL}, the function will
#' construct slider sounds based on \code{stm_base}, \code{min}, \code{max} and \code{format_numbers}.
#'
#' @param stm_base "body" for audio files that are mapped onto the slider. If used, files must be named "body<Nr>.\code{file_ext}"
#' and \code{min} and \code{max} have to be defined. Mutually exclusive with \code{sliderSounds}.
#'
#' @param min minimum value(s) for the slider. Only required if \code{stm_base} is used rather than \code{sliderSounds}.
#'
#' @param max maximum value(s) for the slider. Only required if \code{stm_base} is used rather than \code{sliderSounds}.
#'
#' @param sliderLength Length(s) of the slider, i.e. number of sounds that can be selected.
#'
#' @param targets numeric vector with indices of sounds that should be used as targets in the test battery. If \code{NULL},
#' the function will sample \code{N_items} targets from \code{sliderSounds}.
#'
#' @param target_range numeric vector with indices of sounds that can be used as targets in the test battery. If \code{NULL},
#' the function will sample from the full range of \code{sliderSounds}.
#'
#' @param randomize_range Should presented slider ranges be a random subset of \code{sliderSounds}? Requires \code{sliderLength} and \code{target_margins}.
#'
#' @param target_margins numeric value that defines the margins around the target that should be excluded from the sampled range.
#'
#' @param show_trial_number logical value that defines whether the trial number should be displayed in the prompt header.
#'
#' @param format_numbers format string for adding numbers to the base URL when building \code{sliderSounds}
#' (e.g. "%02d" for 2 digits integers).
#'
#' @param file_ext file extension for the audio files in \code{sliderSounds}. Default is "mp3".
#'
#' @param dict dictionary object used for internationalisation.
#'
#' @param ... additional arguments that are passed to \code{\link{audio_slider_page}()}.
#'
#' @export
RMT_battery <- function(N_items=NULL,
                        label="RMT",
                        sliderSounds=NULL,
                        stm_base=NULL,
                        min=NULL,
                        max=NULL,
                        sliderLength=NULL,
                        targets=NULL,
                        target_range=NULL,
                        randomize_range=FALSE,
                        target_margins=5,
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

  # create sliderSounds arrays if necessary
  if (is.null(sliderSounds)) {
    if (is.null(max)) max <- min + sliderLength-1
    sliderSounds <- mapply(file_link_array,base=stm_base,min=min,max=max,MoreArgs=list(format_numbers=format_numbers,file_ext=file_ext),SIMPLIFY=FALSE)
  }

  if(is.null(targets)) { # sample random targets if targets is empty
    if (is.null(target_range)) {target_range <- 1:length(sliderSounds)} # sample from full sound pool if target_range is empty
    targets <- constrained_sample(range=target_range,N=N_items,margin=target_margins)
  }
  #print(targets)

  if (is.numeric(targets)) {
    demo_item <- which.min(abs((targets - stats::median(targets)))) # use slider sounds of closest-to-median target value for demo
    if (!is.null(min)) targets <- targets - min # adjust targets if necessary
    targets <- mapply(function(target,sounds) sounds[target+1],target=targets,sounds=sliderSounds)
  } else {
    demo_item <- sample(1:length(targets),1) # randomly choose one item as demo
  }

  if (randomize_range) sliderSounds <- mapply(sample_range,target=targets,vector_in=sliderSounds,range_length=sliderLength,target_margin=target_margins)

  if (is.null(sliderLength)) sliderLength <- sapply(sliderSounds,length)

  RMT_item_battery <- mapply(audio_slider_page,
                             label=paste0(label,1:length(targets)),
                             ref_src=targets,
                             sliderSounds=sliderSounds,
                             value=round(sliderLength/2),
                             MoreArgs=list(dict=dict,...))

  psychTestR::module("RMT",
                     psychTestR::join(RMT_instructions(sliderSounds=sliderSounds[[demo_item]],
                                                       value=round(length(sliderSounds[[demo_item]])/2),
                                                       dict=dict,...),
                                      psychTestR::randomise_at_run_time(
                                        label=paste(label,"item_order",sep="_"),
                                        RMT_item_battery),
                                      RMT_get_score,
                                      RMT_feedback(dict=dict),
                                      psychTestR::elt_save_results_to_disk(complete=TRUE))
                     )
}
