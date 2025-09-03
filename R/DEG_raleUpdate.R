#' Update of the psyquest-DEG function
#'
#' @description Wrapper for the \code{DEG} function of the \code{psyquest} package. Prior to calling the function,
#' some adjustments on the dictionary (\code{psyquest::psyquest_dict}) are made to make it more formal.
#'
#' @param subscales A character vector of subscales to be included in the questionnaire.
#'
#' @param language Language for testing.
#'
#' @export

DEG_raleUpdate <- function(subscales=c(),language) {

  # load psyquest dict as data frame
  psyquest_dict_df <- psyquest::psyquest_dict$as.data.frame()

  # perform some manual tweaks to make it more formal
  replacements <- data.frame(key <- c("CHOOSE_ANSWER",
                                      "CLOSE_BROWSER",
                                      "ENTER_ID",
                                      "RESULTS_SAVED",
                                      "TDEG_0002_PROMPT",
                                      "TDEG_0010_PROMPT",
                                      "TDEG_0006_PROMPT"),
                             text = c("Bitte w\u00e4hlen Sie zuerst eine Antwort aus!",
                                      "Sie k\u00f6nnen dieses Fenster schlie\u00dfen.",
                                      "Bitte geben Sie Ihre ID ein:",
                                      "Ihre Ergebnisse wurden gespeichert.",
                                      "Haben Sie Probleme oder Schwierigkeiten mit dem H\u00f6ren? (krankheitsbedingt)",
                                      "In welchem Monat & Jahr wurden Sie geboren?",
                                      "Was ist Ihre Staatsangeh\u00f6rigkeit/Nationalt\u00e4t?"))

  # insert replacements in dictionary
  for (i in 1:nrow(replacements)) {
    psyquest::psyquest_dict$edit(key = replacements$key[i],
                                 language="de",
                                 new = replacements$text[i])
  }
  psyquest::DEG(subscales=subscales,dict=psyquest::psyquest_dict,language=language)
}
