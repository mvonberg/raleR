RMT_instructions <- function(sliderSounds,value,step=1,ref_buttonSize="20%",
                             stm_buttonSize="20%",dict=raleR::RALE_dict) {

  psychTestR::new_timeline(
    psychTestR::page(ui =  shiny::div(shiny::h3(psychTestR::i18n("RMT_DEMO_HEADER")),
                                      shiny::br(),
                                      shiny::p(shiny::strong(psychTestR::i18n("PUT_ON_HEADPHONES"))),
                                      shiny::br(),
                                      shiny::div(shiny::p(psychTestR::i18n("RMT_DEMO_PROMPT1")),
                                                 shiny::p(psychTestR::i18n("RMT_DEMO_PROMPT2")),
                                                 style = "text-align: left; width: 60%; margin-left: 5%;"),
                                      shiny::br(),
                                      audio_button(sliderSounds[round(length(sliderSounds)/5)],id='target',psychTestR::i18n("RMT_TARGET_LABEL"),ref_buttonSize),
                                      shiny::br(),
                                      audio_button(sliderSounds[value+1],id='sliderAudio',psychTestR::i18n("RMT_STIMULUS_LABEL"),stm_buttonSize),
                                      shiny::br(),
                                      shiny::div(class="slider-container",
                                                 shiny::tags$input(type="range",
                                                                   min=0,
                                                                   max=length(sliderSounds)-1,
                                                                   value=value,
                                                                   step=step,
                                                                   class="slider",
                                                                   id="slider")
                                      ),
                                      shiny::br(),
                                      shiny::div(shiny::p(psychTestR::i18n("RMT_DEMO_PROMPT3")),
                                                 shiny::p(psychTestR::i18n("RMT_DEMO_PROMPT4")),
                                                 shiny::p(psychTestR::i18n("START_TEST_PROMPT")),
                                                 style="text-align: left; width: 60%; margin-left: 5%;"),
                                      shiny::br(),
                                      psychTestR::trigger_button("next", psychTestR::i18n("NEXT_BUTTON")),
                                      # javascript
                                      JS_create_array(values=sliderSounds,array_name="sliderSounds"),
                                      shiny::tags$script(paste0("var sliderOffset = ",0,";")),
                                      JS_toggleSounds(),
                                      JS_processAudioSliderInput()),
                     label = "RMTdemo", get_answer = NULL, save_answer = FALSE,
                     on_complete = NULL, final = FALSE,
                     admin_ui = NULL),
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
        shiny::h3(psychTestR::i18n("FEEDBACK_HEADER")),
        shiny::br(),
        shiny::p(psychTestR::i18n("RMT_FEEDBACK_TEXT1")),
        shiny::p(shiny::strong(paste(avg_score, psychTestR::i18n("RMT_FEEDBACK_TEXT2")))),
        shiny::br(),
        shiny::p(feedback_plot(rel_score*100))
      )
      psychTestR::one_button_page(body=feedback_body,
                                  button_text = psychTestR::i18n("NEXT_BUTTON")
      )
    }),
    dict=dict
  )
}
