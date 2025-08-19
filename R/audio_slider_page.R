#' Slider page to match to audio files
#'
#' @description Creates a page where the participant uses a slider to change one audio file to match another.
#' Both souunds can be played back with toggle buttons. The selected slider value is the stored response.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param item_idx Item Index to display in header.
#'
#' @param ref_src audio files for (static) reference sounds (character vector)
#'
#' @param stm_base "body" for audio files that are mapped onto the slider. If used, files must be named "body<Nr>.<format>"
#' and \code{min} and \code{max} have to be defined. Mutually exclusive with \code{sliderSounds}.
#'
#' @param sliderSounds character vector containing the audio files that should be mapped onto the slider. The slider is scaled from 0 to \code{length(sliderSounds)+1}. Mutually exclusive with \code{stm_base}.
#'
#' @param audioFormat_stimulus file-format of the sounds mapped to the slider. Only required if \code{stm_base} is used rather than \code{sliderSounds} and file format differs from (first) specified reference sound.
#'
#' @param ref_buttonSize character vector for the widths of each reference play button. Needs to contain a unit such as "px" or "cm".
#'
#' @param stm_buttonSize character vector for the width of the button to play back the slider-manipulated sound. Needs to contain a unit such as "px" or "cm".
#'
#' @param min,max minimum and maximum value for the slider. Only required if \code{stm_base} is used rather than \code{sliderSounds}
#'
#' @param step slider resolution
#'
#' @param value starting value for the slider.
#'
#' @param save_answer Should the answer the be saved?
#'
#' @param dict \code{i18n} dictionary for internationalisation.
#'
#' @param admin_ui Optional UI for the admin interface. If \code{NULL}, no admin interface is created.
#'
#' @note The RStudio preview function seems not to work
#' for slider pages on some machines.
#'
#' @export
#'
audio_slider_page <- function(label,
                              item_idx="",
                              ref_src=NULL,
                              stm_base=NULL,
                              sliderSounds=NULL,
                              audioFormat_stimulus = NULL,
                              ref_buttonSize="20%",
                              stm_buttonSize="20%",
                              min=NULL,
                              max=NULL,
                              step = 1,
                              value,
                              save_answer = TRUE,
                              dict = raleR::RALE_dict,
                              admin_ui = NULL) {

  stopifnot(rlang::is_scalar_character(label))

  if (is.null(audioFormat_stimulus)) audioFormat_stimulus <- tools::file_ext(ref_src)[1] # get file format from target sound file

  # get slider sounds if only base and min/max are specified
  if (is.null(sliderSounds)) {
    sliderSounds <- paste0(stm_base,min:max,'.',audioFormat_stimulus)
  } else {
    min <- 1
    max <- length(sliderSounds)
  }

  correct_answer <- match(ref_src,sliderSounds) # get index of correct answer

  JS_initiateSliderSounds <- JS_create_array(values=sliderSounds,array_name="sliderSounds") # initiate sliderSounds array in javascript
  JS_initiateSliderOffset <- shiny::tags$script(paste0("var sliderOffset = ",min,";")) # define slider offset in javascript

  psychTestR::new_timeline(
    psychTestR::page(ui =  shiny::div(shiny::h3(paste(psychTestR::i18n("RMT_ITEM_HEADER"),item_idx,sep=" ")),
                                      shiny::br(),
                                      shiny::p(psychTestR::i18n("RMT_ITEM_PROMPT")),
                                      shiny::br(),
                                      audio_button(ref_src,id='target',psychTestR::i18n("RMT_TARGET_LABEL"),ref_buttonSize),
                                      shiny::br(),
                                      audio_button(sliderSounds[value+1],id='sliderAudio',psychTestR::i18n("RMT_STIMULUS_LABEL"),stm_buttonSize),
                                      shiny::br(),
                                      shiny::div(class="slider-container",
                                                 shiny::tags$input(type="range",
                                                                   min=as.character(min),
                                                                   max=as.character(max),
                                                                   value=as.character(value),
                                                                   step=as.character(step),
                                                                   class="slider",
                                                                   id="slider")
                                      ),
                                      shiny::br(),
                                      psychTestR::trigger_button("next", psychTestR::i18n("NEXT_BUTTON")),
                                      JS_initiateSliderSounds,
                                      JS_initiateSliderOffset,
                                      JS_toggleSounds(),
                                      JS_processAudioSliderInput()),
                     label = label,
                     get_answer = function(input, ...)
                       as.numeric(input$slider),
                     save_answer = save_answer,
                     on_complete = function(answer,state,...) {

                       psychTestR::save_result(state,paste(label,'correct',sep="_"),correct_answer)
                       psychTestR::save_result(state,paste(label,'diff',sep="_"),as.numeric(answer)-correct_answer)
                       psychTestR::save_result(state,paste(label,'sliderSounds',sep="_"),sliderSounds)
                       this_response <- as.numeric(answer)-correct_answer # get response
                       slider_responses <- psychTestR::get_local("slider_dist_resp", state) # load previous responses
                       slider_responses <- c(slider_responses,this_response) # append new response
                       psychTestR::set_local("slider_dist_resp", slider_responses, state) # overwrite variable

                       this_max_dist <- max(c(correct_answer),length(sliderSounds)-correct_answer) # get maximum distance
                       max_dist <- psychTestR::get_local("slider_max_dist", state) # load maximum possible distance
                       max_dist <- c(max_dist,this_max_dist) # append new maximum distance
                       psychTestR::set_local("slider_max_dist", max_dist, state) # overwrite variable

                     },
                     final = FALSE,
                     admin_ui = admin_ui),
    dict=dict
  )

}
