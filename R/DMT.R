#' Standalone Drum Machine Test
#'
#' Wraps \code{\link{DMT}()} in a standalone psychTestR app.
#'
#' @inheritParams DMT
#' @param admin_password Password for the psychTestR admin panel (reachable
#'   by appending \code{?admin=1} to the test URL). Change the default before
#'   deploying to a public-facing server.
#' @param researcher_email Contact email shown at the bottom of every page and
#'   in the admin panel.
#'
#' @returns A Shiny app object.
#' @export
#'
#' @examples
#' \dontrun{
#' DMT_standalone(num_trials = 8L, language = "de")
#' }
DMT_standalone <- function(tempo = 100,
                           num_trials = 5L,
                           num_examples = 2,
                           with_feedback = TRUE,
                           stratified_sampling = TRUE,
                           custom_stratified_sampling_allocation = NULL,
                           trial_timeout = 90,
                           language = "en",
                           with_id = TRUE,
                           admin_password = "conifer",
                           researcher_email = "anton.schreiber@uni-hamburg.de") {

  if (!is.scalar.character(admin_password)) {
    stop("admin_password must be a single character string.")
  }

  DMT(tempo = tempo,
      num_trials = num_trials,
      num_examples = num_examples,
      with_feedback = with_feedback,
      stratified_sampling = stratified_sampling,
      custom_stratified_sampling_allocation = custom_stratified_sampling_allocation,
      trial_timeout = trial_timeout,
      language = language,
      with_id = with_id
  ) %>%
    psychTestR::make_test(
      opt = psychTestR::test_options(
        title = "Drum Machine Test",
        admin_password = admin_password,
        enable_admin_panel = TRUE,
        researcher_email = researcher_email,
        problems_info = DMT_problems_info(researcher_email, language),
        languages = language,
        # Equivalent to display_options(full_screen = TRUE), except that the
        # footer stays visible: it holds the admin panel link.
        display = psychTestR::display_options(
          show_header  = FALSE,
          show_footer  = TRUE,
          left_margin  = 0L,
          right_margin = 0L,
          content_border = "0px"
        ),
        additional_scripts = "https://cdnjs.cloudflare.com/ajax/libs/tone/14.8.49/Tone.js"
      )
    )
}

# Contact line shown at the bottom of each page, in the test language.
DMT_problems_info <- function(researcher_email, language) {

  text <- switch(
    tolower(language),
    en   = paste0("Problems? Contact ", researcher_email, " with a link to this page."),
    de   = paste0("Probleme? Schreib an ", researcher_email, " und schick den Link zu dieser Seite mit."),
    de_f = paste0("Probleme? Schreiben Sie an ", researcher_email, " und senden Sie den Link zu dieser Seite mit."),
    stop("No contact line defined for language '", language, "'.")
  )

  stats::setNames(text, language)
}

#' Embed Drum Machine Test in battery
#'
#' @param num_trials Number of main trials.
#' @param tempo Playback tempo in BPM.
#' @param num_examples Number of practice trials (0 to 5). 0 skips the
#'   practice phase and its instruction page.
#' @param with_feedback If \code{TRUE}, participants get up to four attempts
#'   per trial with increasingly detailed feedback; the solution is shown
#'   after the fourth attempt. If \code{FALSE}, each trial has one attempt.
#' @param stratified_sampling If \code{TRUE}, main trials are sampled per
#'   participant from four complexity strata and presented from easiest to
#'   hardest stratum. If \code{FALSE}, the full item bank is presented in
#'   fixed order.
#' @param custom_stratified_sampling_allocation Optional named list giving the
#'   number of trials per stratum. Must contain exactly the names
#'   \code{easy_easy}, \code{easy_hard}, \code{normal_easy} and
#'   \code{normal_hard} and sum to \code{num_trials}. By default, trials are
#'   distributed evenly, with any remainder going to the easy strata.
#' @param trial_timeout Time limit per attempt in seconds (practice and main
#'   trials).
#' @param language Test language, one of \code{\link{DMT_languages}()}.
#' @param with_id If \code{TRUE}, participants are asked for an ID at the
#'   start. Otherwise psychTestR assigns one automatically.
#'
#' @returns A psychTestR timeline.
#' @export
#'
#' @examples
#' \dontrun{
#' psychTestR::make_test(DMT(num_trials = 8L))
#' }
DMT <- function(num_trials = 5L,
                tempo = 100,
                num_examples = 2,
                with_feedback = TRUE,
                stratified_sampling = TRUE,
                custom_stratified_sampling_allocation = NULL,
                trial_timeout = 90,
                language = "en",
                with_id = TRUE) {

  if(!is.null(custom_stratified_sampling_allocation) && sum(unlist(custom_stratified_sampling_allocation)) != num_trials) {
    stop("Number of trials specified in custom_stratified_sampling_allocation must add up to num_trials.")
  }

  n_demo_stimuli <- dplyr::n_distinct(demo_drum_matrix$TrialNo)

  if(num_examples > n_demo_stimuli) {
    stop(sprintf("num_examples (%i) exceeds the number of available demo stimuli (%i).", as.integer(num_examples), n_demo_stimuli))
  }

  easy_stimuli_drum_matrix <- prepare_item_bank(easy_stimuli_drum_matrix, source = "easy")
  drum_matrix <- prepare_item_bank(drum_matrix, source = "normal")

  # Used if stratified_sampling = FALSE
  full_drum_matrix <- dplyr::bind_rows(easy_stimuli_drum_matrix, drum_matrix) %>%
    dplyr::mutate(TrialNo = dplyr::row_number())

  # The number of main trials can be smaller than num_trials if a stratum (or
  # the item bank) has too few stimuli. This only depends on the item bank,
  # so it can be determined when the timeline is built.
  main_trial_count <- num_trials

  if (stratified_sampling) {

    resolved_allocation <- resolve_stratum_allocation(
      easy_stimuli_drum_matrix, drum_matrix, num_trials, custom_stratified_sampling_allocation
    )

    main_trial_count <- resolved_n_sampled(resolved_allocation)

    if (main_trial_count < num_trials) {
      logging::logwarn(
        "DMT(): Only %i of the %i requested trials can be sampled from the available stimuli.",
        main_trial_count, num_trials
      )
    }

  } else {

    n_available <- dplyr::n_distinct(full_drum_matrix$TrialNo)

    if (num_trials > n_available) {
      logging::logwarn(
        "DMT(): %i trials requested, but only %i are available in the item bank.",
        num_trials, n_available
      )
      main_trial_count <- n_available
    }

  }

  # Setup resource paths
  dmt_resources()

  psychTestR::new_timeline(

    psychTestR::join(

      # Intro
      DMT_intro(tempo, with_id, with_feedback, num_examples, trial_timeout),

      phase_timestamp("intro_end"),

      if(num_examples > 0L) DMT_training(num_examples, tempo, with_feedback, trial_timeout),

      if(num_examples > 0L) phase_timestamp("practice_end"),

      psychTestR::one_button_page(psychTestR::i18n("READY_MESSAGE"), button_text = psychTestR::i18n("CONTINUE")),

      phase_timestamp("main_start"),

      # Save after the practice phase so that dropouts appear in the results
      psychTestR::elt_save_results_to_disk(complete = FALSE),

      # Sample main trials
      if(stratified_sampling) sample_trials(num_trials, custom_stratified_sampling_allocation),

      # Main Trials
      DMT_main_trials(main_trial_count, tempo, with_feedback, trial_timeout, stratified_sampling, full_drum_matrix),

      phase_timestamp("main_end"),

      psychTestR::elt_save_results_to_disk(complete = TRUE),

      psychTestR::final_page(psychTestR::i18n("FINAL_MESSAGE"))
    ),

    dict = DMT_dict_for_language(DMT_dict, language)
  )
}

DMT_main_trials <- function(num_trials, tempo, with_feedback, trial_timeout = 90, stratified_sampling, drum_matrix) {
  purrr::map(1:num_trials, ~ psychTestR::join(
    DMT_page_loop(trial_no = .x,
                  num_trials = num_trials,
                  tempo = tempo,
                  with_feedback = with_feedback,
                  trial_timeout = trial_timeout,
                  stratified_sampling = stratified_sampling,
                  stimulus_drum_matrix = drum_matrix),
    psychTestR::elt_save_results_to_disk(complete = FALSE)
  )) %>% unlist()
}

# Saves the current time as result "DMT_phase_<phase>" (see
# DMT_phase_durations()).
phase_timestamp <- function(phase) {
  psychTestR::code_block(function(state, ...) {
    psychTestR::save_result(state, paste0("DMT_phase_", phase), Sys.time())
  })
}

DMT_training <- function(num_examples, tempo, with_feedback, trial_timeout = 90) {
  purrr::map(1:num_examples, ~ DMT_demo_loop(.x, num_examples, tempo, with_feedback = with_feedback, trial_timeout = trial_timeout)) %>% unlist()
}


DMT_demo_loop <- function(trial_no, num_examples, tempo, with_feedback = TRUE, trial_timeout = 90) {

  psychTestR::join(

    # Explain the play buttons before the first practice trial
    if (trial_no == 1L) psychTestR::one_button_page(
      shiny::tags$div(
        display_trial_no(trial_no, num_examples, demo = TRUE),
        shiny::tags$p(psychTestR::i18n("INSTR_EXAMPLE1_1")),
        shiny::tags$p(psychTestR::i18n("INSTR_EXAMPLE1_2")),
        shiny::tags$p(psychTestR::i18n("INSTR_EXAMPLE1_3"))
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ),

    DMT_page_loop(trial_no, num_examples, tempo, demo = TRUE, stimulus_drum_matrix = demo_drum_matrix, with_feedback = with_feedback, trial_timeout = trial_timeout)

  ) %>% unlist()
}

# Adds the Source label and harmonises column types, which differ between
# the item banks (Audiofile/Seconds are logical NA for the easy stimuli).
prepare_item_bank <- function(dat, source) {
  dat %>%
    dplyr::mutate(
      Source = source,
      Audiofile = as.character(Audiofile),
      Seconds = as.numeric(Seconds)
    )
}

# Returns the four strata, the number of trials requested per stratum and the
# number of distinct stimuli available per stratum. Used both when building
# the timeline (DMT()) and when sampling at runtime (sample_trials()), so that
# both always agree on the number of trials.
resolve_stratum_allocation <- function(easy_stimuli_drum_matrix,
                                       drum_matrix,
                                       num_trials,
                                       custom_stratified_sampling_allocation) {

  if (!is.null(custom_stratified_sampling_allocation)) {

    missing_strata <- setdiff(dmt_strata, names(custom_stratified_sampling_allocation))
    unknown_strata <- setdiff(names(custom_stratified_sampling_allocation), dmt_strata)

    if (length(missing_strata) > 0 || length(unknown_strata) > 0) {
      stop(
        "custom_stratified_sampling_allocation must contain exactly the strata '",
        paste(dmt_strata, collapse = "', '"),
        "' (use 0 to leave a stratum out).",
        if (length(missing_strata) > 0) paste0(" Missing: ", paste(missing_strata, collapse = ", "), "."),
        if (length(unknown_strata) > 0) paste0(" Unknown: ", paste(unknown_strata, collapse = ", "), ".")
      )
    }
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

  easy_levels   <- get_halves(easy_stimuli_drum_matrix)
  normal_levels <- get_halves(drum_matrix)

  strata <- list(
    easy_easy   = list(dat = easy_stimuli_drum_matrix, half = easy_levels[1]),
    easy_hard   = list(dat = easy_stimuli_drum_matrix, half = easy_levels[2]),
    normal_easy = list(dat = drum_matrix,              half = normal_levels[1]),
    normal_hard = list(dat = drum_matrix,              half = normal_levels[2])
  )

  if (!is.null(custom_stratified_sampling_allocation)) {
    allocation <- custom_stratified_sampling_allocation
  } else {

    # Base allocation
    n_per_group <- floor(num_trials / 4)
    remainder   <- num_trials %% 4

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

  available <- lapply(strata, function(s) {
    s$dat %>%
      dplyr::filter(ComplexityHalves == s$half) %>%
      dplyr::distinct(Stimulus) %>%
      nrow()
  })

  list(
    strata     = strata,
    allocation = allocation,
    available  = available
  )
}

# Number of trials that will actually be sampled for a resolved allocation.
resolved_n_sampled <- function(resolved) {
  sum(mapply(min, resolved$allocation, resolved$available))
}

# One row per stimulus with 48 binary onset columns
# (HiHat 1-16, Snare 17-32, Kick 33-48).
stimulus_onset_matrix <- function(df, inst_levels = c("HiHat", "Snare", "Kick")) {

  stim_ids <- unique(df$Stimulus)

  m <- matrix(
    0L,
    nrow = length(stim_ids),
    ncol = 16L * length(inst_levels),
    dimnames = list(stim_ids, NULL)
  )

  inst_idx <- match(df$Instrument, inst_levels)
  col      <- (inst_idx - 1L) * 16L + df$BeatPositionSixteenth
  row      <- match(df$Stimulus, stim_ids)

  m[cbind(row, col)] <- 1L

  m
}

# Samples n stimuli whose pairwise Hamming distance is at least min_distance,
# so that a participant does not get near-identical patterns within a stratum.
# Tries random draws first; if none satisfies the constraint, falls back to a
# greedy selection that relaxes min_distance step by step.
sample_stimuli_min_distance <- function(stimulus_ids, onset_matrix, n, min_distance = 3, max_random_attempts = 200) {

  if (n <= 0) return(character(0))
  if (n >= length(stimulus_ids)) return(stimulus_ids)
  if (n == 1) return(sample(stimulus_ids, 1))

  pairwise_ok <- function(ids, threshold) {
    pairs <- utils::combn(ids, 2, simplify = FALSE)
    all(vapply(pairs, function(p) sum(onset_matrix[p[1], ] != onset_matrix[p[2], ]) >= threshold, logical(1)))
  }

  for (i in seq_len(max_random_attempts)) {
    candidate <- sample(stimulus_ids, n)
    if (pairwise_ok(candidate, min_distance)) return(candidate)
  }

  for (threshold in seq(min_distance, 0, by = -1)) {

    shuffled <- sample(stimulus_ids)
    picked   <- shuffled[1]

    for (id in shuffled[-1]) {
      if (length(picked) >= n) break
      dists <- vapply(picked, function(p) sum(onset_matrix[p, ] != onset_matrix[id, ]), numeric(1))
      if (all(dists >= threshold)) picked <- c(picked, id)
    }

    if (length(picked) >= n) {
      if (threshold < min_distance) {
        logging::logwarn(
          "sample_stimuli_min_distance(): Minimum distance %i not achievable for %i of %i stimuli, relaxed to %i.",
          min_distance, n, length(stimulus_ids), threshold
        )
      }
      return(picked[seq_len(n)])
    }
  }

  sample(stimulus_ids, n)
}

sample_trials <- function(num_trials, custom_stratified_sampling_allocation) {
  psychTestR::code_block(function(state, ...) {

    # Label
    easy_stimuli_drum_matrix <- prepare_item_bank(easy_stimuli_drum_matrix, source = "easy")
    drum_matrix <- prepare_item_bank(drum_matrix, source = "normal")

    resolved <- resolve_stratum_allocation(
      easy_stimuli_drum_matrix, drum_matrix, num_trials, custom_stratified_sampling_allocation
    )

    sample_stratum <- function(stratum_name) {

      s         <- resolved$strata[[stratum_name]]
      n         <- resolved$allocation[[stratum_name]]
      available <- resolved$available[[stratum_name]]

      if (available < n) {
        logging::logwarn(
          "sample_trials(): Stratum '%s' has only %i stimuli, but %i were requested.",
          stratum_name, available, n
        )
      }

      stratum_dat <- s$dat %>%
        dplyr::filter(ComplexityHalves == s$half)

      selected_ids <- sample_stimuli_min_distance(
        stimulus_ids = unique(stratum_dat$Stimulus),
        onset_matrix = stimulus_onset_matrix(stratum_dat),
        n = min(n, available),
        min_distance = 3
      )

      stratum_dat %>%
        dplyr::filter(Stimulus %in% selected_ids) %>%
        dplyr::mutate(Stratum = stratum_name)
    }

    sampled_drum_matrix <- purrr::map_dfr(dmt_strata, sample_stratum)

    n_sampled <- dplyr::n_distinct(sampled_drum_matrix$Stimulus)

    logging::loginfo("Sampled %s items via stratified sampling!", n_sampled)

    sampled_drum_matrix %>%
      dplyr::distinct(Stimulus, Source, ComplexityHalves) %>%
      dplyr::count(ComplexityHalves) %>%
      print()

    if (n_sampled < num_trials) {
      logging::logwarn(
        "sample_trials(): Only %i of %i requested trials could be sampled.",
        n_sampled, num_trials
      )
    }

    # Present strata in fixed order (easiest first), randomise within strata
    trial_order <- sampled_drum_matrix %>%
      dplyr::distinct(Stimulus, Stratum) %>%
      dplyr::mutate(Stratum = factor(Stratum, levels = dmt_strata)) %>%
      dplyr::group_by(Stratum) %>%
      dplyr::mutate(WithinBlockOrder = sample(dplyr::n())) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(Stratum, WithinBlockOrder) %>%
      dplyr::mutate(TrialNo = dplyr::row_number()) %>%
      dplyr::select(Stimulus, TrialNo)

    sampled_drum_matrix <- sampled_drum_matrix %>%
      dplyr::select(-Stratum) %>%
      dplyr::left_join(trial_order, by = "Stimulus") %>%
      dplyr::arrange(TrialNo)

    psychTestR::set_global("sampled_trials", sampled_drum_matrix, state)

  })
}

#' DMT languages
#'
#' Lists the languages available for the DMT. Each language corresponds to a
#' column in \code{data-raw/DMT_dict.xlsx}.
#'
#' @returns A character vector of language codes.
#' @export
DMT_languages <- function() {
  c("en", "de", "de_f")
}

#' Reduce DMT_dict to a single language
#'
#' Subsets an i18n dictionary to the \code{key} column plus one language
#' column, so that the timeline is built for exactly that language.
#'
#' @param dict A \code{psychTestR::i18n_dict} object (e.g. \code{DMT_dict}).
#' @param language A language code, one of \code{\link{DMT_languages}()}.
#' @keywords internal
DMT_dict_for_language <- function(dict, language) {

  stopifnot(is.scalar.character(language))

  language <- tolower(language)

  if (!language %in% DMT_languages()) {
    stop(
      "Unsupported language '", language, "'. ",
      "Supported languages: ", paste(DMT_languages(), collapse = ", ")
    )
  }

  dict_df <- as.data.frame(dict)

  if (!language %in% names(dict_df)) {
    stop("Language '", language, "' has no column in DMT_dict.")
  }

  psychTestR::i18n_dict$new(dict_df[, c("key", language)])
}
