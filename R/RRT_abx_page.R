#' RRT item page with ABX interface
#'
#' @description ABX page to with two reference sound and an comparison sound that matches either A or B.
#' Radio buttons are used to select whether A or B matches X. The function is a wrapper for the \code{\link{multiAudio_NAFC_page}}.
#'
#' @param url_A character or vector (for interchangeable sources) of URLs for A
#'
#' @param url_B character or vector (for interchangeable sources) of URLs for B
#'
#' @param label page label
#'
#' @param item_num Item number that is needed for randomised assignment of stimulus to A, B and X and whether A or B is correct.
#' Should be unique for each item if full randomisation is desired.
#'
#' @param question Question above the answers (radio buttons).
#'
#' @param labels_AB Labels (character vector) for the buttons triggering the sound A and B
#'  sounds (upper row)
#'
#' @param label_X Label (character vector for the button) for triggering the sound X
#'
#' @param selectionLabels answer options
#'
#' @param selectionValues values returned by the radio buttons
#'
#' @param inline should answer option be aligned horizontally? Default is \code{TRUE}.
#'
#' @param fixed_X If desired give a source that will always be set for X. Default is \code{NULL}, i.e. all sources will be randomly assigned to A, B, and X.
#'
#' @param dict \code{i18n} dictionary for internationalisation
#'
#' @param ... other parameters to be passed on to \code{\link{multiAudio_NAFC_page}()}.
#'
#' @export

RRT_abx_page <- function(url_A,
                         url_B,
                         label,
                         item_num=1,
                         question,
                         labels_AB=c("Play A","Play B"),
                         label_X="Play X",
                         selectionLabels=c("A = X","B = X"),
                         selectionValues=c("A","B"),
                         inline = TRUE,
                         fixed_X = NULL,
                         dict = raleR::RALE_dict,
                         ...) {

  ### RANDOMIZED IMPLEMENTATION OF ABX LOGIC ###
  stimulus_combinations <- RRT_item_config(url_A,url_B)
  src_AB <- stimulus_combinations[,1:2]
  src_X <- stimulus_combinations[,3]


  # STEP 3: randomize PRESENTATION ORDER of A and B
  randOrder <- sample(1:2)
  selectionValues <- selectionValues[randOrder]

  ### create multi audio NAFC page with 2 references (A,B) and one comparison (x) ###
  ### for real time randomization conditional pages for each of the 12 stimulus combinations are necessary
  psychTestR::join(
    psychTestR::conditional(RRT_get_current_config("a",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[1,randOrder],
                                                 cmpAlt_src=src_X[1],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="A",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("b",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                   refAlt_src=src_AB[2,randOrder],
                                                   cmpAlt_src=src_X[2],
                                                   prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                     shiny::br()),
                                                   refAlt_labels=labels_AB,
                                                   cmpAlt_labels=label_X,
                                                   selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                   selectionLabels=selectionLabels,
                                                   selectionValues=selectionValues,
                                                   correct_answer="A",
                                                   msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                   inline = inline,
                                                   is_final_page=FALSE,
                                                   button_text = psychTestR::i18n("NEXT_BUTTON")
                              ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("c",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[3,randOrder],
                                                 cmpAlt_src=src_X[3],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="A",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("d",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[4,randOrder],
                                                 cmpAlt_src=src_X[4],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="A",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("e",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[5,randOrder],
                                                 cmpAlt_src=src_X[5],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="A",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("f",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[6,randOrder],
                                                 cmpAlt_src=src_X[6],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="A",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("g",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[7,randOrder],
                                                 cmpAlt_src=src_X[7],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="B",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("h",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[8,randOrder],
                                                 cmpAlt_src=src_X[8],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="B",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("i",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[9,randOrder],
                                                 cmpAlt_src=src_X[9],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="B",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("j",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[10,randOrder],
                                                 cmpAlt_src=src_X[10],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="B",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("k",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[11,randOrder],
                                                 cmpAlt_src=src_X[11],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="B",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict)),
    psychTestR::conditional(RRT_get_current_config("l",item_num),
                            psychTestR::new_timeline(
                              multiAudio_NAFC_page(label=paste0(label,item_num),
                                                 refAlt_src=src_AB[12,randOrder],
                                                 cmpAlt_src=src_X[12],
                                                 prompt=shiny::div(shiny::h4(psychTestR::i18n("RRT_ITEM_HEADER")),
                                                                   shiny::br()),
                                                 refAlt_labels=labels_AB,
                                                 cmpAlt_labels=label_X,
                                                 selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                                                 selectionLabels=selectionLabels,
                                                 selectionValues=selectionValues,
                                                 correct_answer="B",
                                                 msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                                                 inline = inline,
                                                 is_final_page=FALSE,
                                                 button_text = psychTestR::i18n("NEXT_BUTTON")
                            ),dict=dict))
    )
}
