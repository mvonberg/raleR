#' Inquire educational level
#'
#' @description Multiple choice question for educational degree with seven response options
#'
#' @param label label for storing results (character scalar).
#'
#' @param dict \code{i18n} dictionary for internationalisation.
#'
#' @export

RALE_education <- function(label="education",
                           dict=raleR::RALE_dict) {
  psychTestR::module(
    psychTestR::new_timeline(
      psychTestR::NAFC_page(label=label,
                            promt=psychTestR::i18n("EDU_QUESTION"),
                            choices=purrr::map(paste0("EDU_CHOICE",1:7),psychTestR::i18n),
                            save_answer=TRUE
                            ),
      dict=dict
    ),
    psychTestR::save_results_to_disk(complete=TRUE)
  )
}
