#' Standalone Drum Machine Test
#'
#' @param tempo
#' @param num_trials
#' @param num_examples
#' @param with_feedback
#' @param custom_stratified_sampling_allocation
#' @param trial_timeout
#' @param language Scalar character, one of DMT_languages() (currently
#' "en", "de", "de_f"). Fixes the standalone test to exactly this language.
#' @param admin_password Password required to access the psychTestR admin
#' panel (reachable by appending \code{?admin=1} to the test URL). Must be
#' set explicitly — there is no insecure default anymore. Choose a strong,
#' non-guessable password before deploying to a public server.
#' @param researcher_email Contact email shown to participants and used in
#' the admin panel.
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
                           trial_timeout = 90,
                           language = "en",
                           admin_password,
                           researcher_email = "sebastian.silas@uni_hamburg.de") {

  if (missing(admin_password) || !is.scalar.character(admin_password) || nchar(admin_password) < 8) {
    stop(
      "admin_password must be supplied as a character string with at least ",
      "8 characters. Example: DMT_standalone(admin_password = 'a_strong_password_here')"
    )
  }

  DMT(tempo = tempo,
      num_trials = num_trials,
      num_examples = num_examples,
      with_feedback = with_feedback,
      stratified_sampling = stratified_sampling,
      custom_stratified_sampling_allocation = custom_stratified_sampling_allocation,
      trial_timeout = trial_timeout,
      language = language
  ) %>%
    psychTestR::make_test(
      opt = psychTestR::test_options(
        title = "Drum Machine Test",
        admin_password = admin_password,
        enable_admin_panel = TRUE,
        researcher_email = researcher_email,
        languages = language,
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
#' @param language Scalar character, one of DMT_languages() (currently
#' "en", "de", "de_f"). Fixes the test to exactly this language — no other
#' language is reachable in the resulting timeline, not even via URL param.
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
                trial_timeout = 90,
                language = "en") {


  if(!is.null(custom_stratified_sampling_allocation) && sum(unlist(custom_stratified_sampling_allocation)) != num_trials) {
    stop("Number of trials specified in custom_stratified_sampling_allocation must add up to num_trials.")
  }

  # If static test:
  easy_stimuli_drum_matrix <- easy_stimuli_drum_matrix %>%
    dplyr::mutate(
      Source = "easy",
      Audiofile = as.character(Audiofile),
      Seconds = as.numeric(Seconds)
    )

  drum_matrix <- drum_matrix %>%
    dplyr::mutate(
      Source = "normal",
      Audiofile = as.character(Audiofile),
      Seconds = as.numeric(Seconds)
    )

  full_drum_matrix <- dplyr::bind_rows(easy_stimuli_drum_matrix, drum_matrix) %>%
    dplyr::mutate(TrialNo = dplyr::row_number())


  # Setup resource paths
  dmt_resources()

  # Reduce DMT_dict to just the requested language (see DMT_dict_for_language()
  # above) - new_timeline() will then build the test for this one language only.
  dict <- DMT_dict_for_language(DMT_dict, language)

  psychTestR::new_timeline(

    psychTestR::join(

      # Intro
      DMT_intro(tempo),

      if(num_examples > 0L) DMT_training(num_examples, tempo, with_feedback),

      psychTestR::one_button_page(psychTestR::i18n("READY_MESSAGE"), button_text = psychTestR::i18n("CONTINUE")),

      # Sample main trials
      if(stratified_sampling) sample_trials(num_trials, custom_stratified_sampling_allocation),

      # Main Trials
      DMT_main_trials(num_trials, tempo, with_feedback, trial_timeout, stratified_sampling, full_drum_matrix),

      psychTestR::final_page(psychTestR::i18n("FINAL_MESSAGE"))
    ),

    dict = dict
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
                             text = psychTestR::i18n("DEMO_SOLUTION_PROMPT")),

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

    one_button_page_trial_no(trial_no, num_examples, demo = TRUE, text = psychTestR::i18n("DEMO_ENTER_PROMPT")),

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
      dplyr::mutate(
        Source = "easy",
        Audiofile = as.character(Audiofile),
        Seconds = as.numeric(Seconds)
      )

    drum_matrix <- drum_matrix %>%
      dplyr::mutate(
        Source = "normal",
        Audiofile = as.character(Audiofile),
        Seconds = as.numeric(Seconds)
      )

    # ----------------------------------------------------------------
    # BUGFIX Punkt 3a: sample_stratum() gab bei zu wenigen verfuegbaren
    # Stimuli in einem Stratum bisher STILLSCHWEIGEND weniger Trials
    # zurueck als angefordert (durch min(n, nrow(stimuli))), ohne dass
    # das irgendwo sichtbar wurde. Jetzt: sample_stratum() bekommt einen
    # stratum_name (nur fuer Logging) und loggt eine explizite Warnung
    # (logging::logwarn), wenn nicht genug Stimuli vorhanden sind. Das
    # Verhalten selbst aendert sich NICHT (Test laeuft mit weniger
    # Trials in diesem Stratum weiter) - es wird nur sichtbar gemacht.
    # ----------------------------------------------------------------
    sample_stratum <- function(dat, half_label, n, stratum_name) {

      stimuli <- dat %>%
        dplyr::filter(ComplexityHalves == half_label) %>%
        dplyr::distinct(Stimulus)

      available <- nrow(stimuli)

      if (available < n) {
        logging::logwarn(
          "sample_trials(): Stratum '%s' hat nur %i verfuegbare Stimuli, aber %i wurden angefordert. Es werden nur %i Trials aus diesem Stratum gesampelt - der Test laeuft mit insgesamt weniger Trials als num_trials weiter!",
          stratum_name, available, n, available
        )
      }

      selected <- stimuli %>%
        dplyr::slice_sample(n = min(n, available))

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

    # --------------------------------------------------------------
    # NEU: Stratum-Label mitführen, damit wir die Blockreihenfolge
    # (easy_easy -> easy_hard -> normal_easy -> normal_hard) am Ende
    # erzwingen können. Vorher ging dieses Label verloren, weshalb
    # die TrialNo komplett zufällig über alle vier Gruppen vergeben
    # wurde.
    #
    # stratum_name wird jetzt zusaetzlich an sample_stratum()
    # durchgereicht (Punkt 3a - siehe Definition oben).
    # --------------------------------------------------------------
    sampled_drum_matrix <- dplyr::bind_rows(

      sample_stratum(
        easy_stimuli_drum_matrix,
        easy_easy,
        allocation[["easy_easy"]],
        stratum_name = "easy_easy"
      ) %>% dplyr::mutate(Stratum = "easy_easy"),

      sample_stratum(
        easy_stimuli_drum_matrix,
        easy_hard,
        allocation[["easy_hard"]],
        stratum_name = "easy_hard"
      ) %>% dplyr::mutate(Stratum = "easy_hard"),

      sample_stratum(
        drum_matrix,
        normal_easy,
        allocation[["normal_easy"]],
        stratum_name = "normal_easy"
      ) %>% dplyr::mutate(Stratum = "normal_easy"),

      sample_stratum(
        drum_matrix,
        normal_hard,
        allocation[["normal_hard"]],
        stratum_name = "normal_hard"
      ) %>% dplyr::mutate(Stratum = "normal_hard")

    )

    logging::loginfo(
      "Sampled %s items via stratified sampling!",
      dplyr::n_distinct(sampled_drum_matrix$Stimulus)
    )

    sampled_drum_matrix %>%
      dplyr::distinct(Stimulus, Source, ComplexityHalves) %>%
      dplyr::count(ComplexityHalves) %>%
      print()

    # ----------------------------------------------------------------
    # BUGFIX Punkt 3a (Fortsetzung): Gesamt-Kontrolle NACH dem Sampling.
    # Selbst wenn kein einzelnes Stratum betroffen war, kann die Summe
    # ueber alle vier Strata < num_trials sein (z.B. wenn mehrere
    # Strata leicht knapp waren). Diese Warnung fasst das zusammen.
    # ----------------------------------------------------------------
    n_sampled <- dplyr::n_distinct(sampled_drum_matrix$Stimulus)

    if (n_sampled < num_trials) {
      logging::logwarn(
        "sample_trials(): Insgesamt wurden nur %i von %i angeforderten Trials gesampelt (zu wenige verfuegbare Stimuli in mindestens einem Stratum - siehe Warnungen oben).",
        n_sampled, num_trials
      )
    }

    # --------------------------------------------------------------
    # NEU: TrialNo wird jetzt blockweise vergeben, nicht mehr komplett
    # zufällig. Reihenfolge der Blöcke ist fest:
    #   easy_easy -> easy_hard -> normal_easy -> normal_hard
    # Innerhalb jedes Blocks bleibt die Stimulus-Reihenfolge zufällig.
    # --------------------------------------------------------------
    stratum_order <- c("easy_easy", "easy_hard", "normal_easy", "normal_hard")

    trial_order <- sampled_drum_matrix %>%
      dplyr::distinct(Stimulus, Stratum) %>%
      dplyr::mutate(Stratum = factor(Stratum, levels = stratum_order)) %>%
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
#' Lists the languages available for DMT implementations. Muss mit den
#' Spaltennamen (in Kleinschreibung) in data_raw/DMT_dict.xlsx übereinstimmen.
#' @export
DMT_languages <- function() {
  c("en", "de", "de_f")
}
#' Reduce DMT_dict to a single language
#'
#' Internal helper: subsets an i18n_dict down to just the `key` column
#' plus one language column, so that new_timeline() builds the test for
#' exactly that one language. The resulting timeline supports no other
#' language, not even via the `?language=` URL parameter that psychTestR
#' would otherwise offer.
#'
#' @param dict An i18n_dict object (e.g. DMT_dict).
#' @param language Scalar character, one of DMT_languages().
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
    stop(
      "Language '", language, "' is listed in DMT_languages() but has no ",
      "matching column in DMT_dict. Check data_raw/DMT_dict.xlsx."
    )
  }

  sub_df <- dict_df[, c("key", language)]

  psychTestR::i18n_dict$new(sub_df)
}
