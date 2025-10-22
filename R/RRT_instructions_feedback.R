RRT_demo_page <- function(url_A,
                          url_B,
                          url_X,
                          labels_AB=c("Play A","Play B"),
                          label_X="Play X",
                          inline = TRUE,
                          dict=raleR::RALE_dict) {

  psychTestR::join(
    psychTestR::conditional(include_for_participant("a"),
                            psychTestR::new_timeline(
                              psychTestR::page(ui = shiny::div(shiny::h3(psychTestR::i18n("RRT_DEMO_HEADER")),
                                                               shiny::br(),
                                                               shiny::div(shiny::p(psychTestR::i18n("RRT_DEMO_PROMPT1")),
                                                                          shiny::p(psychTestR::i18n("RRT_DEMO_PROMPT2")),
                                                                          style="text-align: left; width: 60%; margin-left: 5%;"),
                                                               shiny::br(),
                                                               audio_button(c(url_A,url_B),labels_AB,"20%"),
                                                               shiny::br(),
                                                               audio_button(c(url_X[1]),label_X,"20%"),
                                                               shiny::br(),
                                                               shiny::div(shiny::p(psychTestR::i18n("RRT_DEMO_PROMPT3")),
                                                                          shiny::p(psychTestR::i18n("START_TEST_PROMPT")),
                                                                          style="text-align: left; width: 60%; margin-left: 5%;"),
                                                               psychTestR::trigger_button("next",psychTestR::i18n("START_TEST_BUTTON")),
                                                               JS_toggleSounds()),
                                               admin = NULL, label="RRTdemo",final=FALSE,get_answer=NULL,save_answer=FALSE,validate=NULL,on_complete=NULL,
                                               next_elt=TRUE),
                              dict=dict
                            )),
    psychTestR::conditional(include_for_participant("b"),
                            psychTestR::new_timeline(
                              psychTestR::page(ui = shiny::div(shiny::h3(psychTestR::i18n("RRT_DEMO_HEADER")),
                                                               shiny::br(),
                                                               shiny::div(shiny::p(psychTestR::i18n("RRT_DEMO_PROMPT1")),
                                                                          shiny::p(psychTestR::i18n("RRT_DEMO_PROMPT2")),
                                                                          style="text-align: left; width: 60%; margin-left: 5%;"),
                                                               shiny::br(),
                                                               audio_button(c(url_A,url_B),labels_AB,"20%"),
                                                               shiny::br(),
                                                               audio_button(c(url_X[2]),label_X,"20%"),
                                                               shiny::br(),
                                                               shiny::div(shiny::p(psychTestR::i18n("RRT_DEMO_PROMPT3")),
                                                                          shiny::p(psychTestR::i18n("START_TEST_PROMPT")),
                                                                          style="text-align: left; width: 60%; margin-left: 5%;"),
                                                               psychTestR::trigger_button("next",psychTestR::i18n("START_TEST_BUTTON")),
                                                               JS_toggleSounds()),
                                               admin = NULL, label="RRTdemo",final=FALSE,get_answer=NULL,save_answer=FALSE,validate=NULL,on_complete=NULL,
                                               next_elt=TRUE),
                              dict=dict
                            ))
  )

}

RRT_init_item_config <- psychTestR::code_block(function(state,...) {
  set_seed_from_id(state,...)
  item_config <- sample(letters[1:12],size=20,replace = TRUE)
  psychTestR::set_local("rrt_item_config",item_config,state)
})

RRT_get_raw_score <- psychTestR::code_block(function(state,...) {
  item_labels <- psychTestR::get_local("rrt_item_labels", state)
  responses_rrt <- purrr::map_dbl(item_labels,psychTestR::get_local,state)
  #print(responses_rrt) # for degugging
  score <- sum(responses_rrt)
  psychTestR::set_local("rrt_score_raw",score,state)
  psychTestR::save_result(state,"rrt_score_raw",score)
})

RRT_feedback <- function(dict=raleR::RALE_dict) {
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state,...) {
      score <- psychTestR::get_local("rrt_score_raw", state)
      rrt_items <- psychTestR::get_local("rrt_item_labels", state)
      feedback_body <- shiny::div(
        shiny::h3(psychTestR::i18n("FEEDBACK_HEADER")),
        shiny::br(),
        shiny::p(psychTestR::i18n("RRT_FEEDBACK_TEXT1")),
        shiny::p(shiny::strong(paste0(score, "/",length(rrt_items)))),
        shiny::p(psychTestR::i18n("RRT_FEEDBACK_TEXT2")),
        shiny::br(),
        shiny::p(feedback_plot(score/length(rrt_items)*100))
      )
      psychTestR::one_button_page(body=feedback_body,
        button_text = psychTestR::i18n("NEXT_BUTTON")
      )
    }),
    dict=dict
  )
}
