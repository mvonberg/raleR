RRT_demo_page <- function(url_A,
                          url_B,
                          url_X,
                          labels_AB=c("Play A","Play B"),
                          label_X="Play X",
                          inline = TRUE,
                          dict=raleR::RALE_dict) {

  psychTestR::new_timeline(
    psychTestR::page(ui = shiny::div(shiny::h3(psychTestR::i18n("RRT_DEMO_HEADER")),
                                     shiny::p(psychTestR::i18n("RRT_DEMO_PROMPT")),
                                     shiny::br(),
                                     audio_button(c(url_A,url_B),labels_AB,"20%"),
                                     shiny::br(),
                                     audio_button(c(url_X),label_X,"20%"),
                                     shiny::br(),
                                     psychTestR::trigger_button("next",psychTestR::i18n("RRT_DEMO_BUTTON")),
                                     JS_toggleSounds()),
                     admin = NULL, label="RRTdemo",final=FALSE,get_answer=NULL,save_answer=FALSE,validate=NULL,on_complete=NULL,
                     next_elt=TRUE),
    dict=dict
  )

}


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
        shiny::h3(psychTestR::i18n("RRT_FEEDBACK_HEADER")),
        shiny::br(),
        shiny::p(psychTestR::i18n("RRT_FEEDBACK_TEXT1")),
        shiny::p(shiny::strong(paste0(score, "/",length(rrt_items)))),
        shiny::p(psychTestR::i18n("RRT_FEEDBACK_TEXT2")),
        shiny::br(),
        shiny::p(RRT_feed_plot(score/length(rrt_items)*100))
      )
      psychTestR::one_button_page(body=feedback_body,
        button_text = psychTestR::i18n("RRT_BUTTON_NEXT")
      )
    }),
    dict=dict
  )
}

RRT_feed_plot <- function(score){
  p <- plotly::plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = score,
    title = list(text = psychTestR::i18n("RRT_FEEDBACK_PLOT")),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      bar = list(color = "#8cc77f"),
      axis =list(range = list(0, 100)),
      steps = list(
        list(range = c(0, 20), color = "#ffe4b3"),
        list(range = c(20, 40), color = "#ffd58a"),
        list(range = c(40, 60), color = "#ffc65e"),
        list(range = c(60, 80), color = "#ffb836"),
        list(range = c(80, 100), color = "#ffa500"))
    ),
    height = 400,
    width = 500)

  plotly::layout(p, margin = list(l=20,r=30))
}
