#' RRT item page with ABX interface
#'
#' @description ABX page to with two reference sound and an comparison sound that matches either A or B.
#' Radio buttons are used to select whether A or B matches X. The function is a wrapper for the multiAudio_NAFC_page.
#'
#' @param url_A character or vector (for interchangeable sources) of URLs for A
#'
#' @param url_B character or vector (for interchangeable sources) of URLs for B
#'
#' @param label page label
#'
#' @param prompt additional text above the buttons toggling the sounds
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
                         prompt,
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

  # STEP 1: randomize whether A=X or B=X
  url_ABX <- data.frame(A=unlist(url_A,use.names = FALSE),
                        B=unlist(url_B,use.names = FALSE)) # data frame of A and B source urls
  X_assign <- sample(1:2,1)                                # randomly assign X to A or B (save index for later)
  url_ABX <- cbind(url_ABX,X=url_ABX[,X_assign])           # double assigned column as X

  #print(paste("true:",selectionLabels[X_assign])) # for debugging
  #print(url_ABX)

  # STEP 2: assing sources (if there is more than 1). If there are just 2, A and B are the same source
  if (nrow(url_ABX) > 1) {

    # shuffle source order
    if (is.null(fixed_X)) {
      src_assignment <- sample(1:3)
    } else {
      src_assignment <- c(sample(c(1:3)[-match(fixed_X,1:3)]),fixed_X)
    }
    idx_srcX <- src_assignment[length(src_assignment)]   # X always must differ from A and B: 3rd if there are 3, 2nd if there are 2
    idx_srcA <- src_assignment[1]                        # A is always the first
    idx_srcB <- src_assignment[length(src_assignment)-1] # B is same as A if there are 2, and 2nd if there are 3

    src_AB <- c(url_ABX$A[idx_srcA],url_ABX$B[idx_srcB]) # selected audio files for A and B
    src_X <- url_ABX$X[idx_srcX]                         # selected audio file for X

  } else {

    src_AB <- c(url_ABX$A,url_ABX$B)
    src_X <- url_ABX$X

  }

  #print(src_AB) # for debugging
  #print(src_X)  # for debugging

  # STEP 3: randomize PRESENTATION ORDER of A and B
  randOrder <- sample(1:2)
  src_AB <- src_AB[randOrder]
  selectionValues <- selectionValues[randOrder]
  correctAnswer <- selectionValues[match(X_assign,randOrder)] # correct answer is the one that was assigned to X

  #print(paste("A=",src_AB[1],"B =",src_AB[2])) # for debugging

  # function to save item labels and result
  on_complete <- function(input,state,...){
    rrt_labels <- psychTestR::get_local("rrt_item_labels", state) # get list of completed items
    rrt_labels <- c(rrt_labels,label) # append current item
    psychTestR::set_local("rrt_item_labels", rrt_labels, state) # overwrite item list
    if (input$answer==correctAnswer) {
      psychTestR::set_local(label,1,state)
      psychTestR::save_result(state,paste(label,"correct",sep="_"),1)
    } else {
      psychTestR::set_local(label,0,state)
      psychTestR::save_result(state,paste(label,"correct",sep="_"),0)
    }
  }

  ### create multi audio NAFC page with 2 references (A,B) and one comparison (x) ###
  psychTestR::new_timeline(
    multiAudio_NAFC_page(label=label,
                         refAlt_src=src_AB,
                         cmpAlt_src=src_X,
                         prompt=shiny::h3(paste(psychTestR::i18n("RRT_ITEM_HEADER"),prompt)),
                         refAlt_labels=labels_AB,
                         cmpAlt_labels=label_X,
                         selectionQuestion=psychTestR::i18n("RRT_ITEM_PROMPT"),
                         selectionLabels=selectionLabels,
                         selectionValues=selectionValues,
                         on_complete=on_complete,
                         msg_validation=psychTestR::i18n("RRT_VALIDATION_ERROR"),
                         inline = inline,
                         is_final_page=FALSE,
                         button_text = psychTestR::i18n("NEXT_BUTTON")
                         ),
    dict=dict
    )
}
