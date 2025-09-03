#' Room Acoustic Listening Expertise Questionnaires
#'
#' @description \code{RALE}, \code{NoiSeQR} and \code{EXP} implement questionnaires to measure room acoustic listening expertise (RALE), noise sensitivity
#' using the reduced noise sensitivity questionnaire by Schuette al al. (NoiSeQR) and previous experience with room acoustics (EXP).
#'
#' @param label Label (\code{string}) for the saved results. Question identifiers will be added automatically.
#'
#' @param subscales Subscales to choose from each inventory. For \code{RALE} the subscales \code{TP} (timbre perception), \code{SL} (sound localization),
#' \code{RR} (room reverberation) and \code{L} (loudness perception) are available. For \code{NoiSeQR} the \code{GENERAL} noise sensitivity and the subscales
#' \code{WORK}, \code{SLEEP}  and \code{HABITATION} are available. For \code{EXP}, there are no subscales.
#'
#' @param dict \code{i18n} dictionary for internationalisation.
#'
#' @references Schuette, M., Marks, A., Wenning E., & Griefahn, B. (2007). The development of the noise sensitivity questionnaire. Noise and Health 9(23), pp.15-24. DOI: 10.4103/1463-1741.34700
#'
#' @export

RALE <- function(label="RALE",
                 subscales=c("TP","SL","RR","L"),
                 dict=raleR::RALE_dict) {

  make_questionnaire(inventory="RALE", label=label, subscales=subscales, dict=dict)
}

#' @rdname RALE
#' @export
NoiSeQR <- function(label="NOISEQR",
                    subscales=c("WORK","SLEEP","HABITATION"),
                    dict=raleR::RALE_dict) {

  make_questionnaire(inventory="NOISEQR", label=label, subscales=subscales, dict=dict)
}

#' @rdname RALE
#' @export
EXP <- function(label="EXP",
                dict=raleR::RALE_dict
) {

  make_questionnaire(inventory="EXP", label=label, subscales=NULL, dict=dict)

}


make_questionnaire <- function(inventory, label, subscales, dict) {

  # extract dictionary keys for this inventory
  dict_keys <- as.data.frame(dict)$key[grep(inventory,as.data.frame(dict)$key)]

  # keys for choice options
  choice_labels <- sort(dict_keys[grep("SCALE",dict_keys)])

  # keys for questions
  item_keys <- dict_keys[grep("QUESTION",dict_keys)]
  if (!is.null(subscales)) {
    item_keys <- dict_keys[grepl(paste(paste0("_",subscales,"_"),collapse="|"),dict_keys)]
  }

  # customize labels
  item_labels <- gsub(inventory,label,item_keys)

  elts <- c()
  for (i in 1:length(item_keys)) {
    this_item_key <- item_keys[i]
    this_item_label <- item_labels[i]
    this_item_num <- (1:length(item_keys))[i]
    item_page <- psychTestR::new_timeline(
      psychTestR::NAFC_page(
        label = this_item_label,
        prompt = shiny::div(#shiny::h4(paste(psychTestR::i18n("QUESTION_HEADER1"),this_item_num,psychTestR::i18n("QUESTION_HEADER2"),length(item_keys),sep=" ")),
                            shiny::p(psychTestR::i18n(this_item_key)),
                            style="width: 60%"),
        choices = choice_labels,
        button_style = "min-width: 200px",
        labels = purrr::map(choice_labels,psychTestR::i18n),
        save_answer = TRUE
      ),
      dict=dict
    )
    elts <- c(elts,item_page)
  }
  psychTestR::module(label=label,
                     psychTestR::join(
                       psychTestR::randomise_at_run_time(label=paste(label,"items",sep="_"),elts),
                       psychTestR::elt_save_results_to_disk(complete=TRUE))
                     )
}
