# feedback.R
#
# Vektorisierte Version: alle nutzer-sichtbaren Strings laufen über
# psychTestR::i18n() und beziehen sich auf Keys aus DMT_dict.xlsx.
#
# Neu: instrument_label() übersetzt die internen Instrument-Codes
# (HiHat / Snare / Kick, wie sie in trial_logic.R::complete_instruments()
# als Faktor-Level erzeugt werden) in die im Dict hinterlegten,
# sprachabhängigen Anzeige-Labels (INSTRUMENT_HIHAT / INSTRUMENT_SNARE /
# INSTRUMENT_BASSDRUM). Die internen Codes selbst bleiben unverändert,
# damit die restliche Trial-Logik (dmt_get_answer(), complete_instruments())
# nicht angefasst werden muss.

DMT_feedback <- function(trial_no, num_trials, tempo, stimulus_drum_matrix, demo = FALSE, stratified_sampling = TRUE, trial_timeout = NULL) {

  psychTestR::reactive_page(function(state, answer, ...) {

    attempt <- psychTestR::get_local("attempt", state) %||% 1L
    feedback_layer <- min(c(attempt, 4L))

    feedback <- parse_feedback(answer, feedback_layer,
                               trial_no, num_trials, tempo)

    show_solution <- feedback_layer == 4 && !answer$global_correct

    if(stratified_sampling && !demo) {
      stimulus_drum_matrix <- psychTestR::get_global("sampled_trials", state)
    }

    DMT_trial_page(
      trial_no,
      num_trials,
      feedback = feedback,
      tempo = tempo,
      attempt = attempt,
      show_solution = show_solution,
      show_input_grid = show_solution,
      demo = demo,
      show_play_buttons = FALSE,
      stimulus_drum_matrix = stimulus_drum_matrix,
      trial_timeout = trial_timeout,
      stratified_sampling = stratified_sampling,
      collect_answer = FALSE
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

# ------------------------------------------------------------------
# Instrument-Label-Lookup (Dict-Keys: INSTRUMENT_HIHAT / INSTRUMENT_SNARE /
# INSTRUMENT_BASSDRUM). Vorschlag: diese Funktion nach utils.R verschieben,
# sobald trial_logic.R (Grid-Labels "Hi-hat"/"Snare"/"Kick") ebenfalls
# vektorisiert wird, da sie dort erneut gebraucht wird.
# ------------------------------------------------------------------

instrument_label <- function(instrument) {

  key <- switch(
    as.character(instrument),
    HiHat = "INSTRUMENT_HIHAT",
    Snare = "INSTRUMENT_SNARE",
    Kick  = "INSTRUMENT_BASSDRUM",
    stop("instrument_label(): unbekanntes Instrument '", instrument, "'")
  )

  psychTestR::i18n(key, html = FALSE)
}

feedback_layer_1 <- function(global_correct) {
  shiny::tags$div(
    shiny::tags$p(
      if (global_correct) psychTestR::i18n("FEEDBACK_CORRECT") else psychTestR::i18n("FEEDBACK_NOT_CORRECT")
    )
  )
}

feedback_layer_2 <- function(global_correct, breakdown) {

  feedback_parsed <- breakdown %>%
    dplyr::mutate(
      InstrumentLabel = vapply(Instrument, instrument_label, character(1)),
      Feedback = mapply(
        function(no_mistakes, inst_label) {
          if (no_mistakes > 0L) {
            psychTestR::i18n("FEEDBACK_MISTAKE_ON", html = FALSE, sub = c(instrument = inst_label))
          } else {
            psychTestR::i18n("FEEDBACK_INSTRUMENT_CORRECT", html = FALSE, sub = c(instrument = inst_label))
          }
        },
        NoMistakes, InstrumentLabel
      )
    ) %>%
    dplyr::select(Feedback)


  shiny::tags$div(
    if (global_correct) {
      shiny::tags$p(psychTestR::i18n("FEEDBACK_CORRECT"))
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
    dplyr::mutate(
      Instrument = vapply(Instrument, instrument_label, character(1)),
      `No. Mistakes` = NoMistakes
    ) %>%
    dplyr::select(Instrument, `No. Mistakes`)

  names(feedback_parsed) <- c(
    psychTestR::i18n("TABLE_COL_INSTRUMENT", html = FALSE),
    psychTestR::i18n("TABLE_COL_MISTAKES", html = FALSE)
  )

  shiny::tags$div(
    if (global_correct) {
      shiny::tags$p(psychTestR::i18n("FEEDBACK_CORRECT"))
    } else {
      feedback_container(shiny_table(feedback_parsed, colnames = TRUE, width = "50%"))
    }
  )
}

feedback_layer_4 <- function(global_correct) {
  shiny::tags$div(
    shiny::tags$p(
      if (global_correct) psychTestR::i18n("FEEDBACK_CORRECT") else psychTestR::i18n("FEEDBACK_SHOW_SOLUTION")
    )
  )
}
