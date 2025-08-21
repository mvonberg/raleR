#' Introduction and Data Policy pages
#'
#' @description Creates a welcome page and a data policy page. The texts have been approved by the DGM ethics committee and must not be changed. Only useful for this particular study.
#'
#' @param dict \code{i18n} dictionary to be used for internationalisation.
#'
#' @export

intro_datapolicy <- function(dict=raleR::RALE_dict) {
  psychTestR::new_timeline(
    # Introduction
    psychTestR::join(
      psychTestR::one_button_page(
        body = shiny::div(shiny::h3(psychTestR::i18n("INTRO_HEADER")),
                          shiny::div(shiny::p(psychTestR::i18n("INTRO_TEXT_PROJECT")),
                                     shiny::p(shiny::strong(psychTestR::i18n("INTRO_USE_HEADPHONES"))),
                                     shiny::p(psychTestR::i18n("INTRO_TEXT_KNOWLEDGE")),
                                     shiny::p(psychTestR::i18n("INTRO_TEXT_MSI")),
                                     shiny::p(psychTestR::i18n("INTRO_TEXT_RALE")),
                                     shiny::br(),
                                     shiny::p(psychTestR::i18n("INTRO_AUTHOR")),
                                     shiny::p(psychTestR::i18n("INTRO_CONTACT"),shiny::a(href="mailto:markusmartin.vonberg@hs-duesseldorf.de", "markusmartin.vonberg@hs-duesseldorf.de")),
                                     style="text-align: left; width: 60%; margin-left: 5%")
                          ),
        button_text = psychTestR::i18n("INTRO_START_BUTTON")
      ),
      # Data policy & informed consent
      psychTestR::one_button_page(
        body = shiny::div(shiny::h3(psychTestR::i18n("DATAPOL_HEADER")),
                          shiny::div(shiny::p(psychTestR::i18n("DATAPOL_INTRO_TEXT")),
                                     shiny::h4(psychTestR::i18n("DATAPOL_PURPOSE_HEADER")),
                                     shiny::p(psychTestR::i18n("DATAPOL_PURPOSE_TEXT1")),
                                     shiny::p(psychTestR::i18n("DATAPOL_PURPOSE_TEXT2")),
                                     shiny::h4(psychTestR::i18n("DATAPOL_LEGAL_HEADER")),
                                     shiny::p(psychTestR::i18n("DATAPOL_LEGAL_TEXT")),
                                     shiny::h4(psychTestR::i18n("DATAPOL_RECEIVE_HEADER")),
                                     shiny::p(psychTestR::i18n("DATAPOL_RECEIVE_TEXT")),
                                     shiny::p(psychTestR::i18n("DATAPOL_RECEIVE_PUB")),
                                     shiny::h4(psychTestR::i18n("DATAPOL_STORAGE_HEADER")),
                                     shiny::p(psychTestR::i18n("DATAPOL_STORAGE_TEXT")),
                                     shiny::p(psychTestR::i18n("DATAPOL_STORAGE_PUB")),
                                     shiny::h4(psychTestR::i18n("DATAPOL_RIGHTS_HEADER")),
                                     shiny::p(psychTestR::i18n("DATAPOL_RIGHTS_TEXT")),
                                     shiny::h4(psychTestR::i18n("DATAPOL_COMPLAIN_HEADER")),
                                     shiny::p(psychTestR::i18n("DATAPOL_COMPLAIN_TEXT")),
                                     shiny::h4(psychTestR::i18n("DATAPOL_CONTACT_HEADER")),
                                     shiny::p(psychTestR::i18n("DATAPOL_CONTACT_TEXT")),
                                     shiny::br(),
                                     shiny::p(psychTestR::i18n("DATAPOL_CONTACT_PROC")),
                                     shiny::p(shiny::a(href="mailto:studien.isave@hs-duesseldorf.de","studien.isave@hs-duesseldorf.de")),
                                     shiny::br(),
                                     shiny::p(psychTestR::i18n("DATAPOL_CONTACT_DPO")),
                                     shiny::p(shiny::a(href="mailto:datenschutzbeauftrager@hs-duesseldorf.de","datenschutzbeauftragter@hs-duesseldorf.de")),
                                     style = "text-align: left; width: 60%; margin-left: 5%")
                          ),
        button_text = psychTestR::i18n("DATAPOL_AGREE_BUTTON")
      )
    ),
    dict = dict
  )
}
