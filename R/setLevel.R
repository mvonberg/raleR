#' Page to set the playback level
#'
#' @description Page for playback level adjustment. A sound of long noise bursts at -51 dBFS and short noise bursts at -57dBFS is presented.
#' Participants are instructed to adjust the playback level so that the long noise bursts are barely audible and the short ones are not.
#' The test sound can be triggered with a toggle button.
#'
#' @param url_dir The URL directory where the test audio file is stored.
#'
#' @param filename The name of the test audio file within the directory (including file format extension such as 'wav').
#'
#' @param dict \code{i18n} dictionary for internationalisation.
#'
#' @export

setLevel <- function(url_dir,
                     filename,
                     dict=raleR::RALE_dict) {

  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(shiny::h3(psychTestR::i18n("SET_LEVEL_HEADER")),
                        shiny::br(),
                        shiny::p(shiny::strong(psychTestR::i18n("PUT_ON_HEADPHONES"))),
                        shiny::br(),
                        shiny::div(shiny::p(psychTestR::i18n("SET_LEVEL_PROMPT1")),
                                   style = "text-align: left; width: 60%; margin-left: 5%"),
                        shiny::br(),
                        audio_button(paste0(url_dir,filename),label=psychTestR::i18n("SET_LEVEL_BUTTON_PLAY"),btn.size="40%"),
                        shiny::br(),
                        shiny::p(shiny::strong(psychTestR::i18n("SET_LEVEL_PROMPT2"))),
                        JS_toggleSounds()),
      button_text = psychTestR::i18n("NEXT_BUTTON")
    ),
    dict=dict
  )
}
