# trial_logic.R
#
# Vektorisiert: alle Teilnehmer-sichtbaren Strings laufen über
# psychTestR::i18n() und Keys aus DMT_dict.
#
# WICHTIG: `label` in DMT_trial_page() (Format "DMT_trial_<n>_attempt_<a>")
# und `trial_name` in while_logic() sind interne Ergebnis-Keys, KEINE
# UI-Texte — diese dürfen nie über i18n() laufen, sonst bricht die
# Attempt-Loop-Logik (results[[trial_name]] Lookup).

DMT_page_loop <- function(trial_no,
                          num_trials,
                          tempo,
                          demo = FALSE,
                          stimulus_drum_matrix = drum_matrix,
                          show_solution = FALSE,
                          with_feedback = TRUE,
                          trial_timeout = NULL,
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

  ui <- shiny::tags$div(
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
      psychTestR::i18n("BUTTON_NEXT"),
      onclick = if(show_solution)
        "if(window.stopDMT){ window.stopDMT();resetSequencer();}"
      else
        "if(window.stopDMT){ window.stopDMT(); }"
    )
  )

  label_prefix <- if (demo) "DMT_demo_trial_" else "DMT_trial_"

  should_collect <- collect_answer && !show_solution

  psychTestR::page(
    ui,
    label = paste0(label_prefix, trial_no, "_attempt_", attempt),
    get_answer = if(should_collect) dmt_get_answer(stimulus_drum_matrix, stratified_sampling) else NULL,
    save_answer = should_collect
  )

}

# ----------------------------------------------------------------------
# BUGFIX (Kollegen-Feedback Bug 1): Eine Note, die der Teilnehmer nur um
# eine Position verschoben eintraegt (z.B. Position 12 statt der
# korrekten Position 13), wurde bisher als 2 GETRENNTE Fehler gezaehlt
# (1x die korrekte Position 13 fehlt, 1x Position 12 wurde faelschlich
# gesetzt) - fuer den Teilnehmer sieht das nach nur 1 Fehler aus.
#
# Fix (Nutzer-Entscheidung): Pro Instrument werden fehlende Pflicht-
# Positionen mit falsch gesetzten Positionen gepaart, wenn sie hoechstens
# 1 Sechzehntel auseinanderliegen (naechstliegendes Paar zuerst, danach
# das naechste usw.). Jedes so gefundene Paar zaehlt als 1 Fehler statt 2.
# Positionen, die keinen Partner in dieser Naehe finden, zaehlen weiterhin
# einzeln. Aendert NICHT die einzelnen Mistake-Zellen in `compare`
# (weiterhin pro Zelle korrekt, relevant fuer andere Auswertungen) -
# betrifft nur die aggregierte NoMistakes-Zahl pro Instrument.
# ----------------------------------------------------------------------
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

    if (is_demo) {
      correct_answer <- demo_drum_matrix %>%
        dplyr::filter(Stimulus == !!stimulus_id) %>%
        dplyr::select(Instrument, BeatPositionSixteenth)

    } else {
      correct_answer <- drum_matrix %>%
        dplyr::filter(Stimulus == !!stimulus_id) %>%
        dplyr::select(Instrument, BeatPositionSixteenth)

    }

    if (is_demo) {

      stimulus_meta <- demo_drum_matrix %>%
        dplyr::filter(Stimulus == !!stimulus_id) %>%
        dplyr::slice(1)

      complexity      <- stimulus_meta$Complexity %||% NA_real_
      source_label    <- NA_character_
      complexity_half <- NA_character_

    } else {

      stimulus_meta <- drum_matrix %>%
        dplyr::filter(Stimulus == !!stimulus_id) %>%
        dplyr::slice(1)

      complexity      <- stimulus_meta$Complexity %||% NA_real_
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

    # ----------------------------------------------------------------
    # BUGFIX Punkt 3b: dplyr::group_by(Instrument, .drop = FALSE) war
    # bisher wirkungslos, weil Instrument zu diesem Zeitpunkt ein
    # Character-Vektor war (.drop = FALSE wirkt nur bei Faktoren). Dass
    # trotzdem immer alle 3 Instrumente im Ergebnis auftauchten, lag
    # allein an complete_instruments() danach.
    #
    # Fix: Instrument wird VOR group_by() explizit zu
    # factor(levels = inst_levels) gemacht. Damit garantiert
    # .drop = FALSE jetzt wirklich, dass alle 3 Instrument-Gruppen im
    # summarise()-Output erscheinen - auch wenn eine davon 0 Zeilen in
    # compare hat (z.B. bei komplett leerer Nutzereingabe, siehe
    # Punkt 3d, oder falls ein Instrument keine erforderlichen Onsets
    # hat).
    #
    # Nebenwirkung: mean() einer leeren Gruppe ergibt NaN statt eines
    # Werts -> wird explizit auf 1 gesetzt (= derselbe Wert, den vorher
    # complete_instruments() fuer fehlende Instrumente eingesetzt hat).
    # Nach aussen also KEINE Verhaltensaenderung.
    #
    # complete_instruments() bleibt als zusaetzliches Sicherheitsnetz
    # bestehen (tut jetzt i.d.R. nichts mehr, da res_summary durch
    # .drop = FALSE bereits vollstaendig ist).
    # ----------------------------------------------------------------
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

    rt_ms <- input$attempt_rt_ms %||% NA_real_

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
                   initial_state = NULL) {


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
      if(!demo) shiny::actionButton("play_sequencer", psychTestR::i18n("BUTTON_PLAY_PATTERN"))
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
    ',
        stimulus_json,
        tolower(show_solution)
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

      window.dmtTrialTimeout = setTimeout(function(){

        if(window.stopDMT){
          window.stopDMT();
        }

        window.dmtTimedOut = true;

        document.getElementById('next').click();

      }, %i);

    ", trial_timeout * 1000))
}
