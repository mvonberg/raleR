#'Intro pages for test blocks
#'
#' @description creates a one button page to display introduction text for a new test block.
#'
#' @param test_key Identifiert for the test block in the \code{dict} object.
#'
#' @param dict \code{i18n} dictionary used for internationalisation.
#'
#' @export

test_block_intro <- function(test_key,dict=raleR::RALE_dict) {
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::p(psychTestR::i18n(paste("INTRO",test_key,sep="_")))
      ),
      button_text = psychTestR::i18n("NEXT_BUTTON")
    ),
    dict = dict,
  )
}
