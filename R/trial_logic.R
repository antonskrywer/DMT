

DMT_page_loop <- function(trial_no, num_trials, tempo, demo = FALSE, stimulus_drum_matrix = drum_matrix, show_solution = FALSE, with_feedback = TRUE) {

  logging::loginfo("show_solution: %s", show_solution)

  logging::loginfo("with_feedback: %s", with_feedback)

  logging::loginfo("!show_solution && with_feedback: %s", !show_solution && with_feedback )

  psychTestR::join(

    psychTestR::code_block(function(state, ...) {
      psychTestR::set_local("attempt", 1L, state)
    }),

    psychTestR::while_loop(

      test = if(with_feedback) while_logic(trial_no) else no_feedback_logic(),

      logic = list(

        psychTestR::reactive_page(function(state, ...) {

          attempt <- psychTestR::get_local("attempt", state)

          DMT_trial_page(
            trial_no = trial_no,
            num_trials = num_trials,
            tempo = tempo,
            attempt = attempt,
            demo = demo,
            stimulus_drum_matrix = stimulus_drum_matrix,
            show_solution = show_solution
          )

        }),

        # Feedback

        if (with_feedback && !show_solution) DMT_feedback(trial_no, num_trials, tempo, stimulus_drum_matrix, demo = demo),

        # Update count
        psychTestR::code_block(function(state, ...) {
          attempt <- psychTestR::get_local("attempt", state)
          psychTestR::set_local("attempt", attempt + 1L, state)
        })

      )
    )
  )
}


while_logic <- function(trial_no) {

  function(state, ...) {

  logging::loginfo("Run while_logic")

  attempt <- psychTestR::get_local("attempt", state)

  # Always run first attempt
  if (attempt == 1L) {
    logging::loginfo("attempt == 1L so run the loop!")
    return(TRUE)
  }

  last_attempt <- attempt - 1L

  trial_name <- paste0("DMT_trial_", trial_no, "_attempt_", last_attempt)

  results <- psychTestR::results(state)$result

  if (!trial_name %in% names(results)) {
    logging::loginfo("%s not in results so run the loop!", trial_name)
    return(TRUE)
  }

  answer <- results[[trial_name]]

  is_correct <- isTRUE(answer$global_correct)

  # Stop if correct
  if (is_correct) {
    logging::loginfo("is_correct TRUE, so stop and exit loop!")
    return(FALSE)
  }

  # Otherwise continue up to 4 attempts
  return(last_attempt < 4L)
  }
}

no_feedback_logic <- function() {
  function(state, ...) {

    logging::loginfo("Run no_feedback_logic")

    psychTestR::get_local("attempt", state) == 1L
  }
}

DMT_trial_page <- function(trial_no,
                           num_trials,
                           tempo,
                           feedback = NULL,
                           show_solution = FALSE,
                           show_input_grid = TRUE,
                           attempt = 1L,
                           show_play_buttons = TRUE,
                           stimulus_drum_matrix = drum_matrix,
                           demo = FALSE) {

  logging::loginfo("show_solution?? %s", show_solution)

  stimulus <- stimulus_drum_matrix %>%
    dplyr::filter(Stimulus == trial_no)

  stimulus_json <- jsonlite::toJSON(stimulus, dataframe = "rows")

  ui <- shiny::tags$div(
    dmt_ui(trial_no, num_trials, stimulus_json, tempo, feedback, show_solution, show_input_grid, show_play_buttons, demo),
    psychTestR::trigger_button(
      "next",
      "Next",
      onclick = if(show_solution)  "if(window.stopDMT){ window.stopDMT();resetSequencer();}" else "if(window.stopDMT){ window.stopDMT(); }"
    )
  )

  psychTestR::page(

    ui,

    label = paste0("DMT_trial_", trial_no, "_attempt_", attempt),

    get_answer = if(show_solution) NULL else dmt_get_answer,

    save_answer = !show_solution
  )
}

dmt_get_answer <- function(input, ...) {

  stim_no <- as.integer(input$stimulus_no)
  is_demo <- isTRUE(input$demo)

  logging::loginfo("stim_no: %i | demo: %s", stim_no, is_demo)

  if (is_demo) {
    correct_answer <- easy_stimuli_drum_matrix %>%
      dplyr::filter(Stimulus == !!stim_no) %>%
      dplyr::select(Instrument, BeatPositionSixteenth)
  } else {
    correct_answer <- drum_matrix %>%
      dplyr::filter(Stimulus == !!stim_no) %>%
      dplyr::select(Instrument, BeatPositionSixteenth)
  }


  if (length(input$sequencer_state) == 0) {
    user_answer_df <- tibble::tibble()
  } else {
    user_answer_df <- matrix(unlist(input$sequencer_state), ncol = 3) %>%
      tibble::as_tibble() %>%
      dplyr::rename(HiHat = V1,
                    Snare = V2,
                    Kick = V3) %>%
      dplyr::mutate(BeatPositionSixteenth = dplyr::row_number()) %>%
      tidyr::pivot_longer(HiHat:Kick, names_to = "Instrument", values_to = "UserSelected")
  }

  if(length(user_answer_df) == 0L) {
    # Case of no user entry
    compare <- correct_answer %>%
      dplyr::mutate(Correct = 0L,
                    Mistake = 1L)

  } else {
    compare <- correct_answer %>%
      dplyr::mutate(ShouldHaveSelected = TRUE) %>%
      dplyr::full_join(user_answer_df,
                       by = c("Instrument", "BeatPositionSixteenth")) %>%
      dplyr::mutate(ShouldHaveSelected = dplyr::case_when(is.na(ShouldHaveSelected) ~ FALSE, TRUE ~ ShouldHaveSelected),
                    Correct = ShouldHaveSelected & UserSelected == 1,
                    Mistake = ShouldHaveSelected & UserSelected == 0 | !ShouldHaveSelected & UserSelected)

  }

  inst_levels <- c("HiHat", "Snare", "Kick")

  res_summary <- compare %>%
    dplyr::group_by(Instrument, .drop = FALSE) %>%
    dplyr::summarise(
      ProportionCorrect = mean(Correct, na.rm = TRUE),
      NoMistakes = sum(Mistake, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete_instruments(inst_levels = inst_levels)


  global_correct <- all(res_summary$NoMistakes == 0)

  list(
    res_summary = res_summary,
    global_correct = global_correct,
    correct_answer = correct_answer
  )
}

dmt_ui <- function(trial_no,
                   num_trials,
                   stimulus_json,
                   tempo,
                   feedback = NULL,
                   show_solution = FALSE,
                   show_input_grid = TRUE,
                   show_play_buttons = TRUE,
                   demo = FALSE) {


  stopifnot(is.null(feedback) || all(dim(feedback) == c(2, 3)))

  input_grid <- shiny::tags$div(

    shiny::tags$script("
      if (window.resetDMT) {
        window.resetDMT();
      }
    "),
    # ------------------------------------------------------------
    # CONTROLS
    # ------------------------------------------------------------

    if(show_play_buttons) shiny::fluidRow(
      if(!is.null(stimulus_json)) shiny::actionButton("play_stimulus", "Play stimulus"),
      if(!demo) shiny::actionButton("play_sequencer", "Play your pattern")
    ),

    shiny::tags$br(),

    shiny::tags$div(

      id = "sequencer-wrapper",

      # ------------------------------------------------------------
      # BAR NUMBERS
      # ------------------------------------------------------------

      shiny::tags$div(
        class = "barnumbers",

        # empty cell to align with instrument labels column
        shiny::tags$div(class = "inst-spacer", ""),

        lapply(1:16, function(i) {
          if (i %% 4 == 1) {
            shiny::tags$div(class = "barlabel", (i - 1) / 4 + 1)
          } else {
            shiny::tags$div(class = "barlabel-empty", "")
          }
        })
      ),

      # ------------------------------------------------------------
      # GRID
      # ------------------------------------------------------------

      shiny::tags$div(
        class = "sequencer",

        shiny::tags$div(class = "inst", "Hi-hat"),
        shiny::tags$div(class = "grid", id = "row0"),

        shiny::tags$div(class = "inst", "Snare"),
        shiny::tags$div(class = "grid", id = "row1"),

        shiny::tags$div(class = "inst", "Kick"),
        shiny::tags$div(class = "grid", id = "row2")
      ),
    )
  )

  shiny::tags$div(
    # ------------------------------------------------------------
    # HEADER
    # ------------------------------------------------------------

    dmt_ui_header(),

    # ------------------------------------------------------------
    # STIMULUS + TRIAL
    # ------------------------------------------------------------

    shiny::tags$script(
      sprintf(
        '
    window.drumStimulus = %s;
    window.showSolution = %s;

    Shiny.setInputValue("stimulus_no", %s, {priority: "event"});
    Shiny.setInputValue("demo", %s, {priority: "event"});
    ',
        stimulus_json,
        tolower(show_solution),
        if(is.null(trial_no)) "" else trial_no,
        tolower(demo)
      )
    ),

    # TEMPO FROM R
    shiny::tags$script(
      sprintf(
        'Shiny.setInputValue("tempo_init", %s, {priority: "event"});',
        tempo
      )
    ),

    # Trial No. UI
    display_trial_no(trial_no, num_trials, demo),

    # ------------------------------------------------------------
    # FEEDBACK
    # ------------------------------------------------------------

    if (!is.null(feedback)) {
      shiny::tags$div(class = "feedback-container",
                      shiny::tags$h4("Feedback"),
                      feedback)
    },

    if(show_input_grid) input_grid,

    # ------------------------------------------------------------
    # JS
    # ------------------------------------------------------------

    shiny::tags$script(src = "js/dmt.js"),

    shiny::tags$script(
      "
      setTimeout(function(){
        if(window.initDMT){
          window.initDMT();
        }
      }, 50);
    "
    )
  )
}


display_trial_no <- function(trial_no, num_trials, demo = FALSE) {
  if (!is.null(trial_no))
    shiny::tags$p(shiny::strong(
      if(demo) sprintf("Example Trial %s / %s", trial_no, num_trials) else sprintf("Trial %s / %s", trial_no, num_trials)
    ))
}


complete_instruments <- function(res_summary,
                                 inst_levels = c("Kick", "HiHat", "Snare")) {

  # Add missing instruments
  missing_insts <- setdiff(inst_levels, res_summary$Instrument)

  if (length(missing_insts) > 0) {
    res_summary <- dplyr::bind_rows(
      res_summary,
      tibble::tibble(
        Instrument = missing_insts,
        ProportionCorrect = 1L,
        NoMistakes = 0L
      )
    )
  }

  # Enforce factor order + sort
  res_summary %>%
    dplyr::mutate(
      Instrument = factor(Instrument, levels = inst_levels)
    ) %>%
    dplyr::arrange(Instrument)
}
