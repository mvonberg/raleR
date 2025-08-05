#' NAFC page with multiple audio stimuli
#'
#' @description NAFC page to compare different sound. Reference alternatives (upper row)
#' and comparison alternatives(lower row) can be independently created and require audio sources and button labels.
#' Radio buttons are used to select which reference or comparing alternative respectively meets a given
#' criterion
#'
#' @param label page label
#'
#' @param refAlt_src,cmpAlt_src URLs (character vector) of the files for reference sounds (upper row)
#'  and comparison sounds (lower row)
#'
#' @param prompt additional text above the buttons toggling the sounds
#'
#' @param refAlt_labels,cmpAlt_labels Labels (character vector) for the buttons triggering the reference
#'  sounds (upper row) and comparison sounds (lower row)
#'
#' @param selectionQuestion Question above the answers (radio buttons).
#'
#' @param selectionLabels answer options
#'
#' @param selectionValues values returned by the radio buttons
#'
#' @param on_complete Optional callback function on page completion. Useful for processing the response (e.g., annotating false or correct responses).
#'
#' @param defaultSelection Default selection for the radio buttons. Default is an empty string, i.e. no selection.
#'
#' @param refAlt_buttonSize,cmpAlt_buttonSize character: width of the button toggling the reference and
#'  comparison sounds. Must contain the width itself and the unit, i.e. '%' or 'px' and will be written
#'  to the\code{style} attribute.
#'
#' @param msg_validation message that is displayed if no answer is selected when the user clicks the "Next" button.
#'
#' @param inline should radios button be aligned horizontically. Default is \code{FALSE}.
#'
#' @param save_answer save answer to disk. Default is \code{TRUE}.
#'
#' @param is_final_page specify if this is the last page of the test. Default is \code{FALSE}.
#'
#' @param button_text text for button that navigates to next page
#'
#' @export

multiAudio_NAFC_page <- function(label,
                                 refAlt_src,
                                 cmpAlt_src=NULL,
                                 prompt="Please listen to all sounds and select the correct answer.",
                                 refAlt_labels=LETTERS[1:length(refAlt_src)],
                                 cmpAlt_labels=paste0("X",1:length(cmpAlt_src)),
                                 selectionQuestion="Which sound is correct?",
                                 selectionLabels=refAlt_labels,
                                 selectionValues=1:length(refAlt_src),
                                 on_complete=NULL,
                                 defaultSelection="",
                                 refAlt_buttonSize="20%",
                                 cmpAlt_buttonSize=refAlt_buttonSize,
                                 msg_validation = "Please select an answer!",
                                 inline = FALSE,
                                 save_answer = TRUE,
                                 is_final_page=FALSE,
                                 button_text="Next")
{

  refAlt_audioIDs <- sapply(refAlt_src,get_filename_from_path,USE.NAMES=FALSE)
  cmpAlt_audioIDs <- sapply(cmpAlt_src,get_filename_from_path,USE.NAMES=FALSE)

  get_answer <- function(input,...) {
    out <- list(refAlt=refAlt_audioIDs,cmpAlt=cmpAlt_audioIDs,choice=input$answer)
    return(out)
  }

  validate <- function(input,...) {
    if (!is.null(input$answer)) {
      TRUE
  } else {
      msg_validation
    }
  }

  ui <- shiny::div(prompt,
                   audio_button(refAlt_src,refAlt_labels,refAlt_buttonSize),
                   shiny::p(),
                   audio_button(cmpAlt_src,cmpAlt_labels,cmpAlt_buttonSize),
                   shiny::radioButtons(inputId="answer",
                                       label=selectionQuestion,
                                       choiceNames=selectionLabels,
                                       choiceValues=selectionValues,
                                       selected=defaultSelection,
                                       inline = inline),
                   psychTestR::trigger_button("next",button_text),
                   JS_toggleSounds(),
  )
  psychTestR::page(ui=ui,admin = NULL, label=label,final=is_final_page,get_answer=get_answer,save_answer=save_answer,validate=validate,
                   on_complete=on_complete,next_elt=TRUE)
}
