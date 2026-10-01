DMT_intro <- function(tempo,
                      with_id = TRUE,
                      with_feedback = TRUE,
                      num_examples = 0L,
                      trial_timeout = 90) {

  psychTestR::join(

    # Welcome and task
    intro_text_page(
      shiny::tags$p(shiny::tags$strong(psychTestR::i18n("INSTR_WELCOME_TITLE"))),
      shiny::tags$p(psychTestR::i18n("INSTR_WELCOME_1")),
      shiny::tags$p(psychTestR::i18n("INSTR_WELCOME_2")),
      shiny::tags$p(shiny::tags$strong(psychTestR::i18n("INSTR_TASK_TITLE"))),
      shiny::tags$p(psychTestR::i18n("INSTR_TASK_1")),
      if (with_feedback) shiny::tags$p(psychTestR::i18n("INSTR_TASK_FEEDBACK")),
      shiny::tags$p(psychTestR::i18n("INSTR_NAVIGATE"))
    ),

    # Participant ID
    if (with_id) psychTestR::get_p_id(button_text = psychTestR::i18n("CONTINUE")),

    # Learning test
    intro_text_page(
      shiny::tags$p(psychTestR::i18n("INSTR_LEARNING_1")),
      shiny::tags$p(psychTestR::i18n("INSTR_LEARNING_2")),
      shiny::tags$p(psychTestR::i18n("INSTR_LEARNING_3"))
    ),

    # Drum machine overview (locked)
    intro_grid_page(
      tempo,
      shiny::tags$p(psychTestR::i18n("INSTR_LOOKS_LIKE")),
      intro_config = list(lockGrid = TRUE)
    ),

    # One page per instrument layer
    intro_layer_page(tempo, layer_no = 1L, row = 0L, instrument = "HiHat"),
    intro_layer_page(tempo, layer_no = 2L, row = 1L, instrument = "Snare"),
    intro_layer_page(tempo, layer_no = 3L, row = 2L, instrument = "Kick"),

    # Free exploration
    intro_grid_page(
      tempo,
      shiny::tags$p(psychTestR::i18n("INSTR_EXPLORE_1")),
      shiny::tags$p(psychTestR::i18n("INSTR_EXPLORE_2")),
      shiny::tags$p(psychTestR::i18n("INSTR_EXPLORE_3")),
      show_play_buttons = TRUE
    ),

    # Feedback
    if (with_feedback) intro_text_page(
      shiny::tags$p(shiny::tags$strong(psychTestR::i18n("INSTR_FEEDBACK_1"))),
      shiny::tags$p(psychTestR::i18n("INSTR_FEEDBACK_2"))
    ),

    # Practice trials
    if (num_examples > 0L) intro_text_page(
      shiny::tags$p(shiny::tags$strong(psychTestR::i18n("PRACTICE_TITLE"))),
      shiny::tags$p(psychTestR::i18n("INSTR_PRACTICE_1")),
      if (!is.null(trial_timeout)) shiny::tags$p(
        psychTestR::i18n("INSTR_PRACTICE_TIMER", sub = c(trial_timeout = as.character(trial_timeout)))
      ),
      shiny::tags$p(psychTestR::i18n("MISTAKES_NORMAL"))
    )

  ) %>% unlist(recursive = FALSE)

}


intro_text_page <- function(...) {
  psychTestR::one_button_page(
    shiny::tags$div(
      dmt_ui_header(),
      ...
    ),
    button_text = psychTestR::i18n("CONTINUE")
  )
}


# Instruction page with a drum machine grid. `intro_config` is passed to
# dmt.js (window.dmtIntroConfig): lockGrid, highlightRow, demo.
intro_grid_page <- function(tempo,
                            ...,
                            intro_config = NULL,
                            show_play_buttons = FALSE,
                            sound_button_key = NULL) {

  psychTestR::page(
    ui = shiny::tags$div(
      ...,
      if (!is.null(sound_button_key)) intro_sound_button(sound_button_key),
      dmt_ui(trial_no = NULL,
             stimulus_id = NULL,
             num_trials = NULL,
             stimulus_json = NULL,
             tempo = tempo,
             show_play_buttons = show_play_buttons,
             trial_timeout = NULL,
             intro_config = intro_config),
      psychTestR::trigger_button(
        "next",
        psychTestR::i18n("CONTINUE"),
        onclick = "if(window.stopDMT){ window.stopDMT(); }"
      )
    )
  )
}


intro_layer_page <- function(tempo, layer_no, row, instrument) {

  intro_grid_page(
    tempo,
    shiny::tags$p(psychTestR::i18n("INSTR_LAYERS_INTRO")),
    shiny::tags$p(
      shiny::tags$strong(psychTestR::i18n(paste0("INSTR_LAYER_", layer_no, "_TITLE"))),
      shiny::tags$br(),
      psychTestR::i18n("INSTR_LAYER_SOUNDS_LIKE")
    ),
    intro_config = list(lockGrid = TRUE, highlightRow = row, demo = instrument),
    sound_button_key = paste0("INSTR_SOUND_BUTTON_", layer_no)
  )
}


# Plays one bar of the demo instrument (see dmt.js). The click also provides
# the user gesture that browsers (notably Safari/iOS) require for audio.
intro_sound_button <- function(label_key) {
  shiny::tags$p(
    shiny::tags$button(
      id = "intro-sound-button",
      type = "button",
      class = "btn btn-default",
      psychTestR::i18n(label_key)
    )
  )
}
