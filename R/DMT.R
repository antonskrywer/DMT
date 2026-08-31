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
#' @param with_id Whether to ask participants for a participant ID at the
#' start of the test (default TRUE). If FALSE, the ID prompt page is
#' skipped; psychTestR still assigns each participant an automatically
#' generated ID internally (via \code{test_options(auto_p_id = TRUE)}), so
#' results saving is unaffected either way.
#' @param admin_password Password to access the psychTestR admin panel
#' (reachable by appending \code{?admin=1} to the test URL). Defaults to
#' "conifer" (the convention used across psychTestR test batteries) so the
#' admin panel is always available without extra setup. Pass a different,
#' non-guessable password before deploying to a public-facing server.
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
                           with_id = TRUE,
                           admin_password = "conifer",
                           researcher_email = "sebastian.silas@uni_hamburg.de") {

  if (!is.scalar.character(admin_password)) {
    stop("admin_password must be supplied as a single character string.")
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
        languages = language,
        # ------------------------------------------------------------
        # BUGFIX (Admin-Panel unsichtbar): display_options(full_screen =
        # TRUE) schaltet intern NICHT nur Header/Raender ab, sondern auch
        # show_footer <- FALSE (siehe display_options()-Quellcode in
        # psychTestR). Der Admin-Panel-Link ("Admin login") wird aber
        # genau in diesem Footer-<div> gerendert - unabhaengig vom
        # admin_panel-Flag, das fuer sich allein nichts nuetzt, wenn der
        # ganze Footer per hidden-Attribut versteckt ist. Deshalb zeigte
        # der DMT (anders als z.B. SLT, das full_screen gar nicht setzt)
        # unten keinen Admin-Login-Link mehr an.
        # Fix: dieselben full_screen-Effekte (kein Header, keine seitl.
        # Raender/Rahmen fuer die volle Grid-Breite) manuell setzen, aber
        # show_footer explizit TRUE lassen, damit der Admin-Panel-Link
        # sichtbar bleibt.
        # ------------------------------------------------------------
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
#' @param with_id Whether to ask participants for a participant ID at the
#' start of the test (default TRUE). If FALSE, the ID prompt page is
#' skipped; psychTestR/the enclosing battery still assigns each participant
#' an ID internally, so results saving is unaffected either way.
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
                language = "en",
                with_id = TRUE) {


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

  # ------------------------------------------------------------------
  # BUGFIX Punkt 3a-Folgefehler: DMT_main_trials() baute bisher IMMER
  # 1:num_trials Seiten (die urspruenglich ANGEFORDERTE Anzahl), egal wie
  # viele Trials sample_trials() zur Laufzeit tatsaechlich sampelt, wenn
  # ein Stratum zu wenige verfuegbare Stimuli hat (siehe Warnung dort).
  # Fuer TrialNo-Werte oberhalb der tatsaechlich gesampelten Trials fand
  # DMT_page_loop()/dmt_get_answer() dann keine Zeilen mehr -> leerer
  # Stimulus/leeres Grid, UI-Zaehler zeigte z.B. "1/200" statt "1/84".
  #
  # Fix: main_trial_count bestimmt jetzt vorab (zur BUILD-TIME), wie viele
  # Trials tatsaechlich zustande kommen werden, und wird sowohl fuer die
  # Trial-Loop (DMT_main_trials()) als auch fuer die Anzeige (num_trials
  # in DMT_page_loop() -> display_trial_no()) verwendet. Das ist moeglich,
  # weil die Verfuegbarkeit pro Stratum eine reine Eigenschaft der (festen)
  # Item-Bank-Daten ist - nicht vom einzelnen Teilnehmer oder vom Zufall
  # der Stichprobenziehung -, und daher bereits hier deterministisch
  # feststeht. sample_trials() (Runtime-code_block) nutzt fuer die
  # Allokation denselben Helper resolve_stratum_allocation(), damit
  # Build-Time-Zaehlung und tatsaechliches Runtime-Sampling niemals
  # auseinanderlaufen koennen (vgl. Punkt 7 in CLAUDE.md, wo genau ein
  # solches Duplizieren derselben Logik an zwei Stellen bereits zu einem
  # Bug gefuehrt hat). sample_trials() selbst bekommt weiterhin das
  # urspruengliche num_trials (siehe DMT_main_trials()-Aufruf unten) -
  # sie braucht die urspruenglich ANGEFORDERTE Zahl, um dieselbe Allokation
  # herzuleiten und ihre eigene Kurzfassungs-Warnung korrekt zu loggen.
  # ------------------------------------------------------------------
  main_trial_count <- num_trials

  if (stratified_sampling) {

    resolved_allocation <- resolve_stratum_allocation(
      easy_stimuli_drum_matrix, drum_matrix, num_trials, custom_stratified_sampling_allocation
    )

    main_trial_count <- resolved_n_sampled(resolved_allocation)

    if (main_trial_count < num_trials) {
      logging::logwarn(
        "DMT(): Mit den verfuegbaren Stimuli koennen nur %i von %i angeforderten Trials stratifiziert gesampelt werden. Die Haupttest-Phase wird mit %i Trials gebaut (siehe auch die Stratum-Warnungen von sample_trials() zur Laufzeit).",
        main_trial_count, num_trials, main_trial_count
      )
    }

  } else {

    n_available <- dplyr::n_distinct(full_drum_matrix$TrialNo)

    if (num_trials > n_available) {
      logging::logwarn(
        "DMT(): num_trials = %i angefordert, aber nur %i Trials in der (nicht-stratifizierten) Stimulus-Matrix verfuegbar. Die Haupttest-Phase wird mit %i Trials gebaut.",
        num_trials, n_available, n_available
      )
      main_trial_count <- n_available
    }

  }

  # Setup resource paths
  dmt_resources()

  # Reduce DMT_dict to just the requested language (see DMT_dict_for_language()
  # above) - new_timeline() will then build the test for this one language only.
  dict <- DMT_dict_for_language(DMT_dict, language)

  psychTestR::new_timeline(

    psychTestR::join(

      # Intro
      DMT_intro(tempo, with_id),

      if(num_examples > 0L) DMT_training(num_examples, tempo, with_feedback),

      psychTestR::one_button_page(psychTestR::i18n("READY_MESSAGE"), button_text = psychTestR::i18n("CONTINUE")),

      # Sample main trials
      if(stratified_sampling) sample_trials(num_trials, custom_stratified_sampling_allocation),

      # Main Trials
      DMT_main_trials(main_trial_count, tempo, with_feedback, trial_timeout, stratified_sampling, full_drum_matrix),

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


# ----------------------------------------------------------------------
# resolve_stratum_allocation(): berechnet fuer die vier Strata
# (easy_easy / easy_hard / normal_easy / normal_hard) sowohl die
# angeforderte Ziel-Trial-Zahl (allocation) als auch die tatsaechlich
# verfuegbare Anzahl distinkter Stimuli (available). Beides haengt nur
# von den (festen) Item-Bank-Daten, num_trials und
# custom_stratified_sampling_allocation ab - nicht vom Zufall der
# eigentlichen Stichprobenziehung. Deshalb kann dieselbe Funktion sowohl
# zur BUILD-TIME (DMT(), um main_trial_count zu bestimmen) als auch zur
# RUNTIME (sample_trials(), fuer das eigentliche Sampling) verwendet
# werden, ohne dass beide Stellen auseinanderlaufen koennen.
# ----------------------------------------------------------------------
resolve_stratum_allocation <- function(easy_stimuli_drum_matrix,
                                       drum_matrix,
                                       num_trials,
                                       custom_stratified_sampling_allocation) {

  # ----------------------------------------------------------------
  # BUGFIX (Kollegen-Feedback Bug 3, Crash): check_sampling_allocation()
  # prueft nur, ob die uebergebenen Namen eine TEILMENGE der 4 gueltigen
  # Strata sind - eine custom_stratified_sampling_allocation mit nur
  # 2 von 4 Strata (z.B. list(easy_easy = 2, easy_hard = 2)) wurde also
  # bisher stillschweigend akzeptiert, obwohl normal_easy/normal_hard
  # dann komplett fehlten. resolve_stratum_allocation()$allocation[[...]]
  # wurde fuer diese Strata dadurch NULL, was spaeter in sample_stratum()
  # bei "if (available < n)" mit NULL als n zu "Error in if: Argument hat
  # Laenge 0" fuehrte (Crash beim Uebergang von Trial 3 zu Trial 4).
  #
  # Fix (Nutzer-Entscheidung: hart validieren statt fehlende Strata
  # stillschweigend auf 0 zu setzen): Wenn eine custom Allocation
  # angegeben wird, MUESSEN alle 4 Strata-Namen explizit vorkommen (auch
  # mit 0, falls ein Stratum bewusst nicht verwendet werden soll).
  # Hier statt in check_sampling_allocation() geprueft, weil
  # resolve_stratum_allocation() die einzige Stelle ist, die sowohl von
  # DMT() (Build-Time) als auch von sample_trials() (Runtime) genutzt
  # wird - so kann die Pruefung nicht an einer der beiden Stellen
  # vergessen werden (vgl. Punkt 7 in CLAUDE.md).
  # ----------------------------------------------------------------
  required_strata <- c("easy_easy", "easy_hard", "normal_easy", "normal_hard")

  if (!is.null(custom_stratified_sampling_allocation)) {

    missing_strata <- setdiff(required_strata, names(custom_stratified_sampling_allocation))
    unknown_strata <- setdiff(names(custom_stratified_sampling_allocation), required_strata)

    if (length(missing_strata) > 0 || length(unknown_strata) > 0) {
      stop(
        "custom_stratified_sampling_allocation muss genau die 4 Strata '",
        paste(required_strata, collapse = "', '"),
        "' enthalten (auch mit 0 Trials, falls ein Stratum bewusst nicht ",
        "verwendet werden soll).",
        if (length(missing_strata) > 0) paste0(" Es fehlen: ", paste(missing_strata, collapse = ", "), "."),
        if (length(unknown_strata) > 0) paste0(" Unbekannt: ", paste(unknown_strata, collapse = ", "), ".")
      )
    }
  }

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

  # Base allocation
  n_per_group <- floor(num_trials / 4)
  remainder   <- num_trials %% 4

  if (check_sampling_allocation(custom_stratified_sampling_allocation)) {
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

  available <- stats::setNames(
    lapply(strata, function(s) {
      s$dat %>%
        dplyr::filter(ComplexityHalves == s$half) %>%
        dplyr::distinct(Stimulus) %>%
        nrow()
    }),
    names(strata)
  )

  list(
    strata     = strata,
    allocation = allocation,
    available  = available
  )
}

# Anzahl Trials, die mit dieser Allokation tatsaechlich zustande kommen
# (min(angefordert, verfuegbar) je Stratum, aufsummiert).
resolved_n_sampled <- function(resolved) {
  sum(mapply(
    function(n, avail) min(n, avail),
    resolved$allocation,
    resolved$available
  ))
}

# ----------------------------------------------------------------------
# BUGFIX Punkt 4 (CLAUDE.md): Innerhalb eines Stratums (v.a. in den
# leichteren Straten easy_easy/easy_hard/normal_easy) enthaelt der
# Item-Pool teils Stimuli, die sich nur in 1-2 Onsets unterscheiden
# (z.B. Easy_2 vs. Easy_8: nur ein einziger Kick-Onset verschoben).
# Empirisch: 6-12% aller Stimulus-Paare in diesen Straten haben eine
# Hamming-Distanz <= 2 (von max. 48 moeglichen Positionen). Bei rein
# zufaelligem Sampling landen manche Probanden dadurch mehrere fast
# identische Patterns in derselben Sitzung (vom Nutzer berichtet:
# Trials wirkten wie Wiederholungen). resolve_stratum_allocation()/
# sample_stratum() sampeln deshalb jetzt so, dass die pro Proband
# gezogenen Stimuli eines Stratums einen Mindestabstand einhalten -
# die Item-Bank und ihre Complexity-Werte (relevant fuer ISLP/IRT)
# bleiben dabei unveraendert, es aendert sich nur die zufaellige
# Auswahl pro Sitzung.
# ----------------------------------------------------------------------

# Baut je Stimulus einen 48-elementigen Binaervektor (16 Positionen x 3
# Instrumente: HiHat 1-16, Snare 17-32, Kick 33-48) fuer den
# Hamming-Distanz-Vergleich.
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

# Zieht n Stimulus-IDs aus stimulus_ids, deren paarweise Hamming-Distanz
# (in onset_matrix) mindestens min_distance betraegt. Versucht zunaechst
# reines Zufalls-Resampling (schnell, da Kollisionen selten sind); falls
# das nach max_random_attempts Versuchen nicht gelingt, folgt ein
# greedy Fallback, der min_distance in 1er-Schritten lockert, bis eine
# vollstaendige Auswahl zustande kommt (terminiert spaetestens bei
# min_distance = 0, d.h. ohne Einschraenkung - stellt sicher, dass das
# Sampling nie haengen bleibt oder crasht).
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
          "sample_stimuli_min_distance(): Mindestabstand %i mit %i von %i verfuegbaren Stimuli nicht erreichbar, auf %i gelockert.",
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

    resolved <- resolve_stratum_allocation(
      easy_stimuli_drum_matrix, drum_matrix, num_trials, custom_stratified_sampling_allocation
    )

    # ----------------------------------------------------------------
    # BUGFIX Punkt 3a: sample_stratum() gab bei zu wenigen verfuegbaren
    # Stimuli in einem Stratum bisher STILLSCHWEIGEND weniger Trials
    # zurueck als angefordert (durch min(n, nrow(stimuli))), ohne dass
    # das irgendwo sichtbar wurde. Jetzt: sample_stratum() loggt eine
    # explizite Warnung (logging::logwarn), wenn nicht genug Stimuli
    # vorhanden sind. Das Verhalten selbst aendert sich NICHT (Test
    # laeuft mit weniger Trials in diesem Stratum weiter) - es wird nur
    # sichtbar gemacht. Allokation und Verfuegbarkeit kommen jetzt aus
    # resolve_stratum_allocation() (s.o.), damit diese Zahlen garantiert
    # mit main_trial_count aus DMT() (Build-Time) uebereinstimmen.
    # ----------------------------------------------------------------
    sample_stratum <- function(stratum_name) {

      s         <- resolved$strata[[stratum_name]]
      n         <- resolved$allocation[[stratum_name]]
      available <- resolved$available[[stratum_name]]

      if (available < n) {
        logging::logwarn(
          "sample_trials(): Stratum '%s' hat nur %i verfuegbare Stimuli, aber %i wurden angefordert. Es werden nur %i Trials aus diesem Stratum gesampelt - der Test laeuft mit insgesamt weniger Trials als num_trials weiter!",
          stratum_name, available, n, available
        )
      }

      stratum_dat <- s$dat %>%
        dplyr::filter(ComplexityHalves == s$half)

      # BUGFIX Punkt 4: Mindestabstand statt reinem Zufalls-Sampling
      # (siehe sample_stimuli_min_distance() oben).
      onset_matrix <- stimulus_onset_matrix(stratum_dat)
      stimulus_ids <- unique(stratum_dat$Stimulus)

      selected_ids <- sample_stimuli_min_distance(
        stimulus_ids, onset_matrix, n = min(n, available), min_distance = 3
      )

      stratum_dat %>%
        dplyr::filter(Stimulus %in% selected_ids)
    }

    # --------------------------------------------------------------
    # NEU: Stratum-Label mitführen, damit wir die Blockreihenfolge
    # (easy_easy -> easy_hard -> normal_easy -> normal_hard) am Ende
    # erzwingen können. Vorher ging dieses Label verloren, weshalb
    # die TrialNo komplett zufällig über alle vier Gruppen vergeben
    # wurde.
    # --------------------------------------------------------------
    sampled_drum_matrix <- dplyr::bind_rows(

      sample_stratum("easy_easy")   %>% dplyr::mutate(Stratum = "easy_easy"),
      sample_stratum("easy_hard")   %>% dplyr::mutate(Stratum = "easy_hard"),
      sample_stratum("normal_easy") %>% dplyr::mutate(Stratum = "normal_easy"),
      sample_stratum("normal_hard") %>% dplyr::mutate(Stratum = "normal_hard")

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
