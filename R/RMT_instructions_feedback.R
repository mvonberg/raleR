RMT_instructions <- function(dict=raleR::RALE_dict) {
  psychTestR::new_timeline(
    psychTestR::one_button_page(body = shiny::div(shiny::h3(psychTestR::i18n("RMT_DEMO_HEADER")),
                                                  shiny::br(),
                                                  shiny::p(psychTestR::i18n("RMT_DEMO_PROMPT1")),
                                                  shiny::p(psychTestR::i18n("RMT_DEMO_PROMPT2")),
                                                  shiny::p(psychTestR::i18n("RMT_DEMO_PROMPT3"))),
                                button_text = psychTestR::i18n("RMT_DEMO_BUTTON")),
    dict=dict
  )
}

RMT_get_score <- psychTestR::code_block(function(state,...) {
  rmt_responses <- psychTestR::get_local("slider_dist_resp", state) # get responses as distances from target per item
  rmt_max_dist <- psychTestR::get_local("slider_max_dist", state) # get maximum possible distance per item
  #print(rmt_responses)
  avg_score <- mean(abs(rmt_responses))
  rel_score <- mean(1-abs(rmt_responses)/rmt_max_dist,na.rm=TRUE)

  psychTestR::set_local("rmt_score_avg",avg_score,state)
  psychTestR::set_local("rmt_score_rel",rel_score,state)

  psychTestR::save_result(state,"rmt_score_avg",avg_score)
  psychTestR::save_result(state,"rmt_score_rel",rel_score)
})

RMT_feedback <- function(dict=raleR::RALE_dict) {
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state,...) {
      avg_score <- psychTestR::get_local("rmt_score_avg", state)
      rel_score <- psychTestR::get_local("rmt_score_rel", state)
      feedback_body <- shiny::div(
        shiny::h3(psychTestR::i18n("RMT_FEEDBACK_HEADER")),
        shiny::br(),
        shiny::p(psychTestR::i18n("RMT_FEEDBACK_TEXT1")),
        shiny::p(shiny::strong(paste(avg_score, psychTestR::i18n("RMT_FEEDBACK_TEXT2")))),
        shiny::br(),
        shiny::p(RMT_feed_plot(rel_score*100))
      )
      psychTestR::one_button_page(body=feedback_body,
                                  button_text = psychTestR::i18n("RMT_BUTTON_NEXT")
      )
    }),
    dict=dict
  )
}


RMT_feed_plot <- function(score){
  p <- plotly::plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = score,
    title = list(text = psychTestR::i18n("RMT_FEEDBACK_PLOT")),
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
