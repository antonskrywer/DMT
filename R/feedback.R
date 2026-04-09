

DMT_feedback <- function(trial_no, num_trials, tempo, stimulus_drum_matrix, demo = FALSE) {

  psychTestR::reactive_page(function(state, answer, ...) {

    attempt <- psychTestR::get_local("attempt", state) %||% 1L
    feedback_layer <- min(c(attempt, 4L))

    feedback <- parse_feedback(answer, feedback_layer,
                               trial_no, num_trials, tempo)

    show_solution <- feedback_layer == 4 && !answer$global_correct

    DMT_trial_page(
      trial_no,
      num_trials,
      feedback = feedback,
      tempo = tempo,
      show_solution = show_solution,
      show_input_grid = attempt == 4L,
      demo = demo,
      show_play_buttons = FALSE,
      stimulus_drum_matrix = stimulus_drum_matrix
    )
  })
}

parse_feedback <- function(answer, feedback_layer, trial_no, num_trials, tempo) {

  global_correct <- answer$global_correct

  breakdown <- answer$res_summary

  correct_answer <- answer$correct_answer

  switch (feedback_layer,

          `1` = feedback_layer_1(global_correct),

          `2` = feedback_layer_2(global_correct, breakdown),

          `3` = feedback_layer_3(global_correct, breakdown),

          `4` = feedback_layer_4(global_correct),
  )
}

feedback_layer_1 <- function(global_correct) {
  shiny::tags$div(shiny::tags$p(if(global_correct) "Correct!" else "Not Correct!"))
}

feedback_layer_2 <- function(global_correct, breakdown) {

  feedback_parsed <- breakdown %>%
    dplyr::mutate(
      Feedback = dplyr::case_when(
        NoMistakes > 1L ~ paste0("Mistake on ", Instrument),
        TRUE ~ paste0(Instrument, " — Correct!")
      )
    ) %>%
    dplyr::select(Feedback)


  shiny::tags$div(
    if (global_correct) {
      shiny::tags$p("Correct!")
    } else {
      feedback_container(
        shiny::tags$div(
          style = "text-align: center;",
          shiny::tags$style(HTML("
            table { margin-left: auto; margin-right: auto; }
          ")),
          shiny_table(feedback_parsed, colnames = FALSE, width = "60%")
        )
      )
    }
  )
}

feedback_layer_3 <- function(global_correct, breakdown) {

  feedback_parsed <- breakdown %>%
    dplyr::mutate(`No. Mistakes` = NoMistakes) %>%
    dplyr::select(Instrument, `No. Mistakes`)

  shiny::tags$div(if(global_correct)shiny::tags$p("Correct!") else feedback_container( shiny_table(feedback_parsed, colnames = TRUE, width = "50%") ) )
}

feedback_layer_4 <- function(global_correct) {
  shiny::tags$div(
    shiny::tags$p(if (global_correct) "Correct!" else "Here's the correct pattern:")
  )
}
