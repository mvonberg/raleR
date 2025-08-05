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
#' @param random_order Logical value that defines whether the items should be presented in a random order. Default is \code{TRUE}.
#'
#' @param dict \code{i18n} dictionary for internationalisation.
#'
#' @export
#'
#' @references Schuette, M., Marks, A., Wenning E., & Griefahn, B. (2007). The development of the noise sensitivity questionnaire. Noise and Health 9(23), pp.15-24. DOI: 10.4103/1463-1741.34700

RALE <- function(label="RALE",
                 subscales=c("TP","SL","RR","L"),
                 random_order=TRUE,
                 dict=raleR::RALE_dict) {

  psychTestR::module(label,
                     psychTestR::join(
                       make_questionnaire(inventory="RALE", label=label, subscales=subscales, random_order=random_order, dict=dict),
                       psychTestR::elt_save_results_to_disk(complete=TRUE)
                     ),
                     dict=dict)
}

NoiSeQR <- function(label="NOISEQR",
                    subscales=c("GENERAL","WORK","SLEEP","HABITATION"),
                    random_order=TRUE,
                    dict=raleR::RALE_dict) {

  psychTestR::module(label,
                     psychTestR::join(
                       make_questionnaire(inventory="NOISEQR", label=label, subscales=subscales, random_order=random_order, dict=dict),
                       psychTestR::elt_save_results_to_disk(complete=TRUE)
                     ),
                     dict=dict)
}

EXP <- function(label="EXP",
                random_order=TRUE,
                dict=raleR::RALE_dict
                 ) {

  psychTestR::module(label,
                     psychTestR::join(
                       make_questionnaire(inventory="EXP", label=label, subscales=NULL, random_order=random_order, dict=dict),
                       psychTestR::elt_save_results_to_disk(complete=TRUE)
                     ),
                     dict=dict)
}


make_questionnaire <- function(inventory, label, subscales, random_order, dict) {

  # extract dictionary keys for this inventory
  dict_keys <- as.data.frame(dict)$key[grep(inventory,as.data.frame(dict)$key)]

  # keys for choice options
  choice_labels <- dict_keys[grep("CHOICE",dict_keys)]

  # keys for questions
  if (is.null(subscales)) {
    item_keys <- dict_keys[grep("QUESTION",dict_keys)]
  } else {
    item_keys <- dict_keys[grepl("QUESTION",dict_keys) & grepl(paste(subscales,collapse="|"),dict_keys)]
  }

  # shuffle presentation order
  if (random_order) {
    item_order <- sample(1:length(item_keys))
    item_keys <- item_keys[item_order]
  }

  # customize labels
  item_labels <- gsub(inventory,label,item_keys)

  elts <- c()
  for (i in 1:length(item_keys)) {
    item_page <- psychTestR::new_timeline(
      psychTestR::NAFC_page(
        label = item_labels[i],
        prompt = psychTestR::i18n(item_keys[i]),
        choices = 1:length(choice_labels),
        labels = purrr::map(choice_labels,psychTestR::i18n),
        save_answer = TRUE
      ),
      dict=dict
    )
    elst <- psychTestR::join(elts,item_page)
  }

}

