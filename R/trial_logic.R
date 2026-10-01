DMT_page_loop <- function(trial_no,
                          num_trials,
                          tempo,
                          demo = FALSE,
                          stimulus_drum_matrix = drum_matrix,
                          show_solution = FALSE,
                          with_feedback = TRUE,
                          trial_timeout = 90,
                          stratified_sampling = TRUE) {

  logging::loginfo("trial_no: %s", trial_no)

  logging::loginfo("show_solution: %s", show_solution)

  logging::loginfo("with_feedback: %s", with_feedback)

  logging::loginfo("!show_solution && with_feedback: %s", !show_solution && with_feedback )

  logging::loginfo("stratified_sampling: %s", stratified_sampling)

  psychTestR::join(

    psychTestR::code_block(function(state, ...) {

      psychTestR::set_local("attempt", 1L, state)

      psychTestR::set_local("sequencer_state", NULL, state)

      psychTestR::set_local("last_global_correct", NULL, state)

      if(stratified_sampling && !demo) {

        dynamic_drum_matrix <- psychTestR::get_global("sampled_trials", state)

        stimulus <- dynamic_drum_matrix %>%
          dplyr::filter(TrialNo == trial_no)

      } else {

        stimulus <- stimulus_drum_matrix %>%
          dplyr::filter(TrialNo == trial_no)

      }

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
      psychTestR::set_local("demo", demo, state)

    }),

    psychTestR::while_loop(

      test = if(with_feedback) while_logic(trial_no, demo) else no_feedback_logic(),

      logic = list(

        psychTestR::reactive_page(function(state, ...) {

          attempt <- psychTestR::get_local("attempt", state)

          saved_state <- psychTestR::get_local("sequencer_state", state)

          if(stratified_sampling && !demo) {

            stimulus_drum_matrix <- psychTestR::get_global("sampled_trials", state)

          }

          DMT_trial_page(
            trial_no = trial_no,
            num_trials = num_trials,
            tempo = tempo,
            attempt = attempt,
            demo = demo,
            stimulus_drum_matrix = stimulus_drum_matrix,
            show_solution = show_solution,
            trial_timeout = trial_timeout,
            initial_state = saved_state,
            stratified_sampling = stratified_sampling,
            collect_answer = TRUE
          )

        }),

        # Feedback

        if (with_feedback && !show_solution) DMT_feedback(trial_no, num_trials, tempo, stimulus_drum_matrix, demo = demo, stratified_sampling = stratified_sampling, trial_timeout = trial_timeout),

        # Update count
        psychTestR::code_block(function(state, ...) {
          attempt <- psychTestR::get_local("attempt", state)
          psychTestR::set_local("attempt", attempt + 1L, state)
        })

      )
    )
  )
}


while_logic <- function(trial_no, demo = FALSE) {

  function(state, ...) {

    logging::loginfo("Run while_logic")

    attempt <- psychTestR::get_local("attempt", state)

    # Always run first attempt
    if (attempt == 1L) {
      logging::loginfo("attempt == 1L so run the loop!")
      return(TRUE)
    }

    last_attempt <- attempt - 1L

    is_correct <- isTRUE(psychTestR::get_local("last_global_correct", state))

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
                           demo = FALSE,
                           trial_timeout = 90,
                           initial_state = NULL,
                           stratified_sampling = TRUE,
                           collect_answer = TRUE) {

  logging::loginfo("show_solution?? %s", show_solution)
  logging::loginfo("initial_state?? %s", initial_state)

  stimulus <- stimulus_drum_matrix %>%
    dplyr::filter(TrialNo == trial_no)

  stimulus_id <- stimulus %>%
    dplyr::pull(Stimulus) %>%
    unique()

  stimulus_json <- jsonlite::toJSON(stimulus, dataframe = "rows")

  should_collect <- collect_answer && !show_solution

  # Page created by DMT_feedback(), including the solution page
  is_feedback_page <- !collect_answer && !is.null(feedback)

  metrics <- page_metrics_js(
    if (should_collect) "attempt" else if (is_feedback_page) "feedback" else NULL
  )

  ui <- shiny::tags$div(
    metrics$start,
    dmt_ui(
      trial_no,
      stimulus_id,
      num_trials,
      stimulus_json,
      tempo,
      feedback,
      show_solution,
      show_input_grid,
      show_play_buttons,
      demo,
      trial_timeout,
      initial_state
    ),
    psychTestR::trigger_button(
      "next",
      psychTestR::i18n(if (collect_answer) "BUTTON_CHECK" else "BUTTON_NEXT"),
      onclick = paste0(
        metrics$send,
        if(show_solution)
          "if(window.stopDMT){ window.stopDMT();resetSequencer();}"
        else
          "if(window.stopDMT){ window.stopDMT(); }"
      )
    )
  )

  label_prefix <- if (demo) "DMT_demo_trial_" else "DMT_trial_"

  label <- paste0(label_prefix, trial_no, "_attempt_", attempt)

  if (is_feedback_page) {

    # Saves the time spent on the feedback page as result
    # "<attempt label>_feedback"
    return(psychTestR::page(
      ui,
      label = paste0(label, "_feedback"),
      get_answer = function(input, ...) {
        list(
          trial_no       = trial_no,
          attempt        = attempt,
          demo           = demo,
          feedback_rt_ms = input$feedback_rt_ms %||% NA_real_
        )
      },
      save_answer = TRUE
    ))
  }

  psychTestR::page(
    ui,
    label = label,
    get_answer = if(should_collect) dmt_get_answer(stimulus_drum_matrix, stratified_sampling) else NULL,
    save_answer = should_collect
  )

}

# JS for recording per-page measures:
#   type = "attempt":  attempt_rt_ms, attempt_stim_plays, attempt_pattern_plays
#   type = "feedback": feedback_rt_ms
# `start` resets the timer, the play counters (incremented in dmt.js) and the
# Shiny inputs when the page is rendered; `send` is added to the onclick
# handler of the next button.
page_metrics_js <- function(type = NULL) {

  if (is.null(type)) return(list(start = NULL, send = ""))

  inputs <- switch(
    type,
    attempt  = c(rt = "attempt_rt_ms",
                 stim = "attempt_stim_plays",
                 pattern = "attempt_pattern_plays"),
    feedback = c(rt = "feedback_rt_ms"),
    stop("page_metrics_js(): unknown type '", type, "'")
  )

  reset_js <- paste0(
    "Shiny.setInputValue('", inputs, "', null, {priority: 'event'});",
    collapse = " "
  )

  send_js <- paste0(
    "Shiny.setInputValue('", inputs["rt"], "', ",
    "Math.round(performance.now() - window.dmtPageStart), {priority: 'event'});",
    if (type == "attempt") paste0(
      " Shiny.setInputValue('", inputs["stim"], "', window.dmtStimPlays || 0, {priority: 'event'});",
      " Shiny.setInputValue('", inputs["pattern"], "', window.dmtPatternPlays || 0, {priority: 'event'});"
    )
  )

  list(
    start = shiny::tags$script(shiny::HTML(sprintf("
      window.dmtPageStart = performance.now();
      window.dmtStimPlays = 0;
      window.dmtPatternPlays = 0;
      if (window.Shiny) { %s }
    ", reset_js))),
    send = sprintf("
      if (window.dmtPageStart !== undefined && window.Shiny) {
        %s
        window.dmtPageStart = undefined;
      }
    ", send_js)
  )
}

# Counts the mistakes for one instrument. A missed onset and a wrongly set
# onset at most max_pair_distance sixteenths apart (i.e. a note entered
# slightly off) count as one mistake rather than two. Closest pairs are
# matched first.
count_paired_mistakes <- function(missed_positions, extra_positions, max_pair_distance = 1) {

  if (length(missed_positions) == 0 || length(extra_positions) == 0) {
    return(length(missed_positions) + length(extra_positions))
  }

  candidates <- expand.grid(missed = missed_positions, extra = extra_positions)
  candidates$dist <- abs(candidates$missed - candidates$extra)
  candidates <- candidates[candidates$dist <= max_pair_distance, , drop = FALSE]
  candidates <- candidates[order(candidates$dist), , drop = FALSE]

  used_missed <- numeric(0)
  used_extra  <- numeric(0)
  n_pairs     <- 0L

  if (nrow(candidates) > 0) {
    for (i in seq_len(nrow(candidates))) {
      m <- candidates$missed[i]
      e <- candidates$extra[i]
      if (!(m %in% used_missed) && !(e %in% used_extra)) {
        used_missed <- c(used_missed, m)
        used_extra  <- c(used_extra, e)
        n_pairs     <- n_pairs + 1L
      }
    }
  }

  (length(missed_positions) + length(extra_positions)) - n_pairs
}

dmt_get_answer <- function(drum_matrix, stratified_sampling) {

  function(input, state, ...) {

    psychTestR::set_local(
      "sequencer_state",
      input$sequencer_state,
      state
    )

    logging::loginfo("dmt_get_answer stratified_sampling: %s", stratified_sampling)

    # If dynamic, use dynamically sampled drum matrix
    if(stratified_sampling) {
      drum_matrix <- psychTestR::get_global("sampled_trials", state)
    }

    trial_no    <- psychTestR::get_local("trial_no", state)
    stimulus_id <- psychTestR::get_local("stimulus_id", state)
    is_demo     <- psychTestR::get_local("demo", state)
    attempt     <- psychTestR::get_local("attempt", state) %||% 1L
    timed_out   <- isTRUE(input$dmtTimedOut)

    logging::loginfo("trial_no: %i | stimulus_id: %s | demo: %s", trial_no, stimulus_id, is_demo)

    stimulus <- (if (is_demo) demo_drum_matrix else drum_matrix) %>%
      dplyr::filter(Stimulus == !!stimulus_id)

    correct_answer <- stimulus %>%
      dplyr::select(Instrument, BeatPositionSixteenth)

    stimulus_meta <- dplyr::slice(stimulus, 1)

    complexity <- stimulus_meta$Complexity %||% NA_real_

    if (is_demo) {
      source_label    <- NA_character_
      complexity_half <- NA_character_
    } else {
      source_label    <- stimulus_meta$Source %||% NA_character_
      complexity_half <- stimulus_meta$ComplexityHalves %||% NA_character_
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
      compare <- correct_answer %>%
        dplyr::mutate(ShouldHaveSelected = TRUE,
                      UserSelected = 0L,
                      Correct = 0L,
                      Mistake = 1L)

    } else {
      compare <- correct_answer %>%
        dplyr::mutate(ShouldHaveSelected = TRUE) %>%
        dplyr::full_join(user_answer_df,
                         by = c("Instrument", "BeatPositionSixteenth")) %>%
        dplyr::mutate(ShouldHaveSelected = dplyr::case_when(is.na(ShouldHaveSelected) ~ FALSE, TRUE ~ ShouldHaveSelected),
                      Correct = ShouldHaveSelected & UserSelected == 1,
                      Mistake = (ShouldHaveSelected & UserSelected == 0) | (!ShouldHaveSelected & UserSelected))

    }

    # Instrument must be a factor for .drop = FALSE to keep instruments
    # without rows; their ProportionCorrect (NaN) is set to 1.
    inst_levels <- c("HiHat", "Snare", "Kick")

    res_summary <- compare %>%
      dplyr::mutate(Instrument = factor(Instrument, levels = inst_levels)) %>%
      dplyr::group_by(Instrument, .drop = FALSE) %>%
      dplyr::summarise(
        ProportionCorrect = mean(Correct, na.rm = TRUE),
        NoMistakes = count_paired_mistakes(
          BeatPositionSixteenth[Mistake & ShouldHaveSelected],
          BeatPositionSixteenth[Mistake & !ShouldHaveSelected]
        ),
        NoHits = sum(Correct, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        NoPositions = 16L,
        ProportionCorrect = dplyr::if_else(is.nan(ProportionCorrect), 1, ProportionCorrect)
      ) %>%
      complete_instruments(inst_levels = inst_levels)

    global_correct <- all(res_summary$NoMistakes == 0)

    psychTestR::set_local("last_global_correct", global_correct, state)

    feedback_layer_shown <- min(attempt, 4L)

    cumulative_attempt <- (psychTestR::get_global("cumulative_attempt", state) %||% 0L) + 1L
    psychTestR::set_global("cumulative_attempt", cumulative_attempt, state)

    # Sent by page_metrics_js("attempt")
    rt_ms         <- input$attempt_rt_ms %||% NA_real_
    stim_plays    <- input$attempt_stim_plays %||% NA_integer_
    pattern_plays <- input$attempt_pattern_plays %||% NA_integer_

    list(
      res_summary          = res_summary,
      global_correct       = global_correct,
      correct_answer       = correct_answer,
      timed_out            = timed_out,
      trial_no             = trial_no,
      stimulus_id          = stimulus_id,
      demo                 = is_demo,
      attempt              = attempt,
      feedback_layer_shown = feedback_layer_shown,
      cumulative_attempt   = cumulative_attempt,
      complexity           = complexity,
      source               = source_label,
      complexity_half      = complexity_half,
      rt_ms                = rt_ms,
      stim_plays           = as.integer(stim_plays),
      pattern_plays        = as.integer(pattern_plays),
      timestamp            = Sys.time()
    )
  }
}

dmt_ui <- function(trial_no,
                   stimulus_id,
                   num_trials,
                   stimulus_json,
                   tempo,
                   feedback = NULL,
                   show_solution = FALSE,
                   show_input_grid = TRUE,
                   show_play_buttons = TRUE,
                   demo = FALSE,
                   trial_timeout = 90,
                   initial_state = NULL,
                   intro_config = NULL) {


  initial_state_json <-
    if (is.null(initial_state)) {
      "null"
    } else {

      initial_state <- list(
        initial_state[1:16],
        initial_state[17:32],
        initial_state[33:48]
      )

      jsonlite::toJSON(initial_state, auto_unbox = TRUE)
    }

  stopifnot(is.null(feedback) || all(dim(feedback) == c(2, 3)))

  input_grid <- shiny::tags$div(

    shiny::tags$script("
      if (window.resetDMT) {
        window.resetDMT();
      }
    "),

    if(show_play_buttons) shiny::fluidRow(
      if(!is.null(stimulus_json)) shiny::actionButton("play_stimulus", psychTestR::i18n("BUTTON_PLAY_STIMULUS")),
      shiny::actionButton("play_sequencer", psychTestR::i18n("BUTTON_PLAY_PATTERN"))
    ),

    if(show_play_buttons) shiny::tags$div(
      style = "display: none;",
      shiny::tags$span(id = "lbl_stop_stimulus", psychTestR::i18n("BUTTON_STOP_STIMULUS")),
      shiny::tags$span(id = "lbl_stop_pattern", psychTestR::i18n("BUTTON_STOP_PATTERN"))
    ),

    shiny::tags$br(),

    shiny::tags$div(

      id = "sequencer-wrapper",

      shiny::tags$div(
        class = "barnumbers",

        shiny::tags$div(class = "inst-spacer", ""),

        lapply(1:16, function(i) {
          if (i %% 4 == 1) {
            shiny::tags$div(class = "barlabel", (i - 1) / 4 + 1)
          } else {
            shiny::tags$div(class = "barlabel-empty", "")
          }
        })
      ),

      shiny::tags$div(
        class = "sequencer",

        shiny::tags$div(class = "inst", psychTestR::i18n("INSTRUMENT_HIHAT")),
        shiny::tags$div(class = "grid", id = "row0"),

        shiny::tags$div(class = "inst", psychTestR::i18n("INSTRUMENT_SNARE")),
        shiny::tags$div(class = "grid", id = "row1"),

        shiny::tags$div(class = "inst", psychTestR::i18n("INSTRUMENT_BASSDRUM")),
        shiny::tags$div(class = "grid", id = "row2")
      ),
    )
  )

  shiny::tags$div(

    timeout_js(show_solution, trial_timeout),

    dmt_ui_header(),

    shiny::tags$script(sprintf(
      "
      window.initialSequencerState = %s;
      ",
      initial_state_json
    )),

    shiny::tags$script(
      sprintf(
        '
    window.drumStimulus = %s;
    window.showSolution = %s;
    window.dmtIntroConfig = %s;
    ',
        if (is.null(stimulus_json)) "null" else stimulus_json,
        tolower(show_solution),
        if (is.null(intro_config)) "null" else jsonlite::toJSON(intro_config, auto_unbox = TRUE)
      )
    ),

    shiny::tags$script(
      sprintf(
        'Shiny.setInputValue("tempo_init", %s, {priority: "event"});',
        tempo
      )
    ),

    display_trial_no(trial_no, num_trials, demo),

    if (!is.null(feedback)) {
      shiny::tags$div(class = "feedback-container",
                      shiny::tags$h4(psychTestR::i18n("FEEDBACK_HEADER")),
                      feedback)
    },

    if(show_input_grid) input_grid,

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
  if (!is.null(trial_no)) {

    key <- if (demo) "TRIAL_COUNTER_EXAMPLE" else "TRIAL_COUNTER"

    shiny::tags$p(shiny::strong(
      psychTestR::i18n(
        key,
        sub = c(trial_no = as.character(trial_no), num_trials = as.character(num_trials))
      )
    ))
  }
}


complete_instruments <- function(res_summary,
                                 inst_levels = c("Kick", "HiHat", "Snare")) {

  missing_insts <- setdiff(inst_levels, res_summary$Instrument)

  if (length(missing_insts) > 0) {
    res_summary <- dplyr::bind_rows(
      res_summary,
      tibble::tibble(
        Instrument = missing_insts,
        ProportionCorrect = 1L,
        NoMistakes = 0L,
        NoHits = 0L,
        NoPositions = 16L
      )
    )
  }

  res_summary %>%
    dplyr::mutate(
      Instrument = factor(Instrument, levels = inst_levels)
    ) %>%
    dplyr::arrange(Instrument)
}

timeout_js <- function(show_solution, trial_timeout) {
  if (!show_solution && !is.null(trial_timeout))
    shiny::tags$script(sprintf("
      clearTimeout(window.dmtTrialTimeout);

      // Shiny inputs persist across pages, so reset the flag on every page
      window.dmtTimedOut = false;
      if (window.Shiny) Shiny.setInputValue('dmtTimedOut', false, {priority: 'event'});

      window.dmtTrialTimeout = setTimeout(function(){

        if(window.stopDMT){
          window.stopDMT();
        }

        window.dmtTimedOut = true;
        if (window.Shiny) Shiny.setInputValue('dmtTimedOut', true, {priority: 'event'});

        document.getElementById('next').click();

      }, %i);

    ", trial_timeout * 1000))
}
