


#' Standalone Drum Machine Test
#'
#' @param tempo
#' @param num_trials
#' @param num_examples
#' @param with_feedback
#'
#' @returns
#' @export
#'
#' @examples
DMT_standalone <- function(tempo = 100,
                           num_trials = 5L,
                           num_examples = 3,
                           with_feedback = TRUE) {
  DMT(tempo = tempo,
      num_trials = num_trials,
      num_examples = num_examples,
      with_feedback = with_feedback) %>%
    psychTestR::make_test(
      opt = psychTestR::test_options(
        title = "Drum Machine Test",
        admin_password = "test",
        researcher_email = "sebastian.silas@uni_hamburg.de",
        display = psychTestR::display_options(full_screen = TRUE),
        additional_scripts = "https://cdnjs.cloudflare.com/ajax/libs/tone/14.8.49/Tone.js"
      )
    )
}

#' Embed Drum Machine Test in battery
#'
#' @param num_trials
#' @param tempo
#' @param num_examples
#' @param with_feedback
#'
#' @returns
#' @export
#'
#' @examples
DMT <- function(num_trials = 5L, tempo = 100, num_examples = 3, with_feedback = TRUE) {

  # Setup resource paths
  dmt_resources()

  psychTestR::join(

    # Intro
    DMT_intro(tempo),

    if(num_examples > 0L) DMT_training(num_examples, tempo, with_feedback),

    psychTestR::one_button_page("Now you're ready for the real thing. Good luck!"),

    # Main Trials
    DMT_main_trials(num_trials, tempo, with_feedback),

    psychTestR::final_page("You have finished the Drum Machine Test!")
  )
}


DMT_main_trials <- function(num_trials, tempo, with_feedback) {
  purrr::map(1:num_trials, ~ DMT_page_loop(.x, num_trials, tempo, with_feedback = with_feedback)) %>% unlist()
}

DMT_training <- function(num_examples, tempo, with_feedback) {
  purrr::map(1:num_examples, ~ DMT_demo_loop(.x, num_examples, tempo, with_feedback = with_feedback)) %>% unlist()
}


DMT_demo_loop <- function(trial_no, num_examples, tempo, with_feedback = TRUE) {

  easy_stimuli_drum_matrix <- easy_stimuli_drum_matrix %>%
    dplyr::filter(Stimulus %in% 1:num_examples)

  psychTestR::join(

    one_button_page_trial_no(trial_no, num_examples, demo = TRUE,
                             text = 'The next page will show you the correct answer. Click "Play stimulus" to hear it.'),

    # Show stimulus as example
    DMT_trial_page(
      trial_no = trial_no,
      num_trials = num_examples,
      tempo = tempo,
      attempt = 0L,
      demo = TRUE,
      stimulus_drum_matrix = easy_stimuli_drum_matrix,
      show_solution = TRUE
    ),

    one_button_page_trial_no(trial_no, num_examples, demo = TRUE, text = "Now enter the pattern you just saw."),

    # Get user to enter it
    DMT_page_loop(trial_no, num_examples, tempo, demo = TRUE, stimulus_drum_matrix = easy_stimuli_drum_matrix, with_feedback = with_feedback)

  ) %>% unlist()
}


one_button_page_trial_no <- function(trial_no, num_trials, text, demo = FALSE) {
  psychTestR::one_button_page(
    shiny::tags$div(
      display_trial_no(trial_no, num_trials, demo = demo),
      shiny::tags$p(text),
    )
  )
}



