


#' Standalone Drum Machine Test
#'
#' @param tempo
#' @param num_trials
#' @param num_examples
#' @param with_feedback
#' @param custom_stratified_sampling_allocation
#' @param trial_timeout
#'
#' @returns
#' @export
#'
#' @examples
DMT_standalone <- function(tempo = 100,
                           num_trials = 5L,
                           num_examples = 3,
                           with_feedback = TRUE,
                           stratified_sampling = TRUE,
                           custom_stratified_sampling_allocation = NULL,
                           trial_timeout = 90) {
  DMT(tempo = tempo,
      num_trials = num_trials,
      num_examples = num_examples,
      with_feedback = with_feedback,
      stratified_sampling = stratified_sampling,
      custom_stratified_sampling_allocation = custom_stratified_sampling_allocation,
      trial_timeout = trial_timeout
      ) %>%
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
#' @param custom_stratified_sampling_allocation
#' @param trial_timeout Trial timeout in seconds.
#'
#' @returns
#' @export
#'
#' @examples
DMT <- function(num_trials = 5L,
                tempo = 100,
                num_examples = 3,
                with_feedback = TRUE,
                stratified_sampling = TRUE,
                custom_stratified_sampling_allocation = NULL,
                trial_timeout = 90) {


  if(!is.null(custom_stratified_sampling_allocation) && sum(unlist(custom_stratified_sampling_allocation)) != num_trials) {
    stop("Number of trials specified in custom_stratified_sampling_allocation must add up to num_trials.")
  }

  # If static test:
  easy_stimuli_drum_matrix <- easy_stimuli_drum_matrix %>%
    dplyr::mutate(Source = "easy")

  drum_matrix <- drum_matrix %>%
    dplyr::mutate(Source = "normal")

  full_drum_matrix <- dplyr::bind_rows(easy_stimuli_drum_matrix, drum_matrix) %>%
    dplyr::mutate(TrialNo = dplyr::row_number())


  # Setup resource paths
  dmt_resources()

  psychTestR::join(

    # Intro
    DMT_intro(tempo),

    if(num_examples > 0L) DMT_training(num_examples, tempo, with_feedback),

    psychTestR::one_button_page("Now you're ready for the real thing. Good luck!"),

    # Sample main trials
    if(stratified_sampling) sample_trials(num_trials, custom_stratified_sampling_allocation),

    # Main Trials
    DMT_main_trials(num_trials, tempo, with_feedback, trial_timeout, stratified_sampling, full_drum_matrix),

    psychTestR::final_page("You have finished the Drum Machine Test!")
  )
}


DMT_main_trials <- function(num_trials, tempo, with_feedback, trial_timeout = 90, stratified_sampling, drum_matrix) {
  purrr::map(1:num_trials, ~ DMT_page_loop(trial_no = .x,
                                           num_trials = num_trials,
                                           tempo = tempo,
                                           with_feedback = with_feedback,
                                           trial_timeout = trial_timeout,
                                           stratified_sampling = stratified_sampling,
                                           stimulus_drum_matrix = drum_matrix)) %>% unlist()
}

DMT_training <- function(num_examples, tempo, with_feedback) {
  purrr::map(1:num_examples, ~ DMT_demo_loop(.x, num_examples, tempo, with_feedback = with_feedback)) %>% unlist()
}


DMT_demo_loop <- function(trial_no, num_examples, tempo, with_feedback = TRUE) {

  psychTestR::join(

    one_button_page_trial_no(trial_no, num_examples, demo = TRUE,
                             text = 'The next page will show you the correct answer. Click "Play stimulus" to hear it.'),

    psychTestR::code_block(function(state, ...) {

      psychTestR::set_local("attempt", 1L, state)

      stimulus <- demo_drum_matrix %>%
        dplyr::filter(TrialNo == trial_no)

      stimulus_id <- stimulus %>%
        dplyr::pull(Stimulus) %>%
        unique()

      logging::loginfo(
        "trial=%i rows=%i ids=%s",
        trial_no,
        nrow(stimulus),
        paste(unique(stimulus$Stimulus), collapse = ",")
      )

      psychTestR::set_local("trial_no", trial_no, state)
      psychTestR::set_local("stimulus_id", stimulus_id, state)
      psychTestR::set_local("demo", TRUE, state)

    }),

    # Show stimulus as example
    DMT_trial_page(
      trial_no = trial_no,
      num_trials = num_examples,
      tempo = tempo,
      attempt = 0L,
      demo = TRUE,
      stimulus_drum_matrix = demo_drum_matrix,
      show_solution = TRUE
    ),

    one_button_page_trial_no(trial_no, num_examples, demo = TRUE, text = "Now enter the pattern you just saw."),

    # Get user to enter it
    DMT_page_loop(trial_no, num_examples, tempo, demo = TRUE, stimulus_drum_matrix = demo_drum_matrix, with_feedback = with_feedback)

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


sample_trials <- function(num_trials, custom_stratified_sampling_allocation) {
  psychTestR::code_block(function(state, ...) {

    # Label
    easy_stimuli_drum_matrix <- easy_stimuli_drum_matrix %>%
      dplyr::mutate(Source = "easy")

    drum_matrix <- drum_matrix %>%
      dplyr::mutate(Source = "normal")

    sample_stratum <- function(dat, half_label, n) {

      stimuli <- dat %>%
        dplyr::filter(ComplexityHalves == half_label) %>%
        dplyr::distinct(Stimulus)

      selected <- stimuli %>%
        dplyr::slice_sample(n = min(n, nrow(stimuli)))

      dat %>%
        dplyr::semi_join(selected, by = "Stimulus")
    }

    # labels for each dataset
    get_halves <- function(dat) {
      dat %>%
        dplyr::group_by(ComplexityHalves) %>%
        dplyr::summarise(
          mean_complexity = mean(Complexity),
          .groups = "drop"
        ) %>%
        dplyr::arrange(mean_complexity) %>%
        dplyr::pull(ComplexityHalves)
    }

    easy_levels <- get_halves(easy_stimuli_drum_matrix)
    normal_levels <- get_halves(drum_matrix)

    easy_easy   <- easy_levels[1]
    easy_hard   <- easy_levels[2]

    normal_easy <- normal_levels[1]
    normal_hard <- normal_levels[2]

    # Base allocation
    n_per_group <- floor(num_trials / 4)
    remainder   <- num_trials %% 4



    if(check_sampling_allocation(custom_stratified_sampling_allocation)) {
      allocation <- custom_stratified_sampling_allocation
    } else {

      allocation <- list(
        easy_easy   = n_per_group,
        easy_hard   = n_per_group,
        normal_easy = n_per_group,
        normal_hard = n_per_group
      )

      # Give leftovers to the easy dataset
      if (remainder >= 1) allocation[["easy_easy"]] <- allocation[["easy_easy"]] + 1
      if (remainder >= 2) allocation[["easy_hard"]] <- allocation[["easy_hard"]] + 1
      if (remainder >= 3) allocation[["easy_easy"]] <- allocation[["easy_easy"]] + 1


    }


    sampled_drum_matrix <- dplyr::bind_rows(

      sample_stratum(
        easy_stimuli_drum_matrix,
        easy_easy,
        allocation[["easy_easy"]]
      ),

      sample_stratum(
        easy_stimuli_drum_matrix,
        easy_hard,
        allocation[["easy_hard"]]
      ),

      sample_stratum(
        drum_matrix,
        normal_easy,
        allocation[["normal_easy"]]
      ),

      sample_stratum(
        drum_matrix,
        normal_hard,
        allocation[["normal_hard"]]
      )

    )

    logging::loginfo(
      "Sampled %s items via stratified sampling!",
      dplyr::n_distinct(sampled_drum_matrix$Stimulus)
    )

    sampled_drum_matrix %>%
      dplyr::distinct(Stimulus, Source, ComplexityHalves) %>%
      dplyr::count(ComplexityHalves) %>%
      print()

    # Randomise and trial no
    trial_order <- sampled_drum_matrix %>%
      dplyr::distinct(Stimulus) %>%
      dplyr::mutate(TrialNo = sample(dplyr::n()))

    sampled_drum_matrix <- sampled_drum_matrix %>%
      dplyr::left_join(trial_order, by = "Stimulus") %>%
      dplyr::arrange(TrialNo)

    psychTestR::set_global("sampled_trials", sampled_drum_matrix, state)

  })
}


