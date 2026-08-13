# R/results.R
#
# Post-hoc Export: wandelt psychTestR-Ergebnisse des DMT in ein
# Long-Format-Tibble um (eine Zeile pro Trial x Attempt), das direkt als
# Input fuer die ISLP-Schaetzung (Yu & Douglas, 2023) dient - kompatibel
# mit dem MCMC-Code in data_simulation.R (run_mcmc_islp_all_dims() etc.),
# wenn man dessen sim_object$data durch dieses Tibble ersetzt.
#
# Voraussetzung: dmt_get_answer() (trial_logic.R) muss die erweiterte
# Version verwenden, die trial_no, stimulus_id, demo, attempt,
# feedback_layer_shown, cumulative_attempt, complexity, source,
# complexity_half, rt_ms, timestamp mit ins Answer-Objekt schreibt, UND
# DMT_trial_page()/DMT_feedback() muessen den Label-/collect_answer-Fix
# enthalten (siehe trial_logic.R, feedback.R) - sonst gibt es doppelte
# oder kollidierende Trial-Labels in aelteren Ergebnisdateien.

# ------------------------------------------------------------------
# Interner Helper: res_summary (3 Zeilen: HiHat/Snare/Kick) -> breites
# 1-Zeilen-Tibble mit hihat_hits/hihat_n, snare_hits/snare_n,
# kick_hits/kick_n, n_total_mistakes.
# ------------------------------------------------------------------

dmt_res_summary_wide <- function(res_summary) {

  get_val <- function(inst, col) {
    row <- dplyr::filter(res_summary, as.character(Instrument) == inst)
    if (nrow(row) == 0 || !col %in% names(row)) return(NA_integer_)
    row[[col]][1]
  }

  tibble::tibble(
    hihat_hits = get_val("HiHat", "NoHits"),
    hihat_n    = get_val("HiHat", "NoPositions"),
    snare_hits = get_val("Snare", "NoHits"),
    snare_n    = get_val("Snare", "NoPositions"),
    kick_hits  = get_val("Kick",  "NoHits"),
    kick_n     = get_val("Kick",  "NoPositions"),
    n_total_mistakes = suppressWarnings(sum(res_summary$NoMistakes, na.rm = TRUE))
  )
}

# ------------------------------------------------------------------
# Ein Teilnehmer -> Long-Format
# ------------------------------------------------------------------

#' Convert one participant's DMT results into long format
#'
#' Nimmt das komplette Ergebnis-Objekt EINES Teilnehmers entgegen, wie es
#' `readRDS("output/results/<datei>.rds")` liefert (Liste mit `$results`
#' und `$session`), und baut daraus ein Long-Format-Tibble mit einer Zeile
#' pro (Trial x Attempt).
#'
#' Nur Seiten, deren Label dem Muster "DMT_trial_<n>_attempt_<a>" oder
#' "DMT_demo_trial_<n>_attempt_<a>" folgt, werden beruecksichtigt (siehe
#' trial_logic.R::DMT_trial_page(), Kommentar zu internen Ergebnis-Keys).
#' Alle anderen Ergebnis-Eintraege (z.B. aus get_p_id()) werden ignoriert.
#'
#' @param res Das komplette Ergebnis-Objekt eines Teilnehmers (Liste mit
#'   `$results` und optional `$session`), ODER direkt die Ergebnisliste
#'   selbst (z.B. `psychTestR::results(state)$result`) - beides wird
#'   erkannt.
#' @param p_id Optionale Teilnehmer-ID als Spalte. Falls NULL, wird
#'   versucht, sie aus `res$session$p_id` zu lesen; sonst NA.
#' @param include_demo Sollen Demo-/Instruktions-Trials mit ausgegeben
#'   werden (Spalte `demo`)? Default TRUE, damit man sie bei Bedarf selbst
#'   herausfiltern kann.
#'
#' @returns Ein Tibble, eine Zeile pro Attempt.
#' @export
DMT_results_to_long <- function(res, p_id = NULL, include_demo = TRUE) {

  stopifnot(is.list(res))

  # Akzeptiere sowohl das volle RDS-Objekt (mit $results/$session) als
  # auch eine bereits extrahierte Ergebnisliste direkt.
  results_list <- if (!is.null(res[["results"]])) res[["results"]] else res

  if (is.null(p_id)) {
    p_id <- res[["session"]][["p_id"]] %||% NA_character_
  }

  label_names <- names(results_list)

  keep_idx <- grep("^DMT_(demo_)?trial_[0-9]+_attempt_[0-9]+$", label_names)

  if (length(keep_idx) == 0) {
    logging::logwarn("DMT_results_to_long(): keine DMT-Trial-Ergebnisse gefunden.")
    return(tibble::tibble())
  }

  # Sicherheitsnetz: doppelte Labels deuten auf Ergebnisdateien hin, die
  # VOR dem Label-/collect_answer-Fix in trial_logic.R/feedback.R erzeugt
  # wurden. Wird trotzdem verarbeitet (positionsbasiert, nicht per Name),
  # aber mit Warnung.
  dup_labels <- label_names[keep_idx][duplicated(label_names[keep_idx])]
  if (length(dup_labels) > 0) {
    logging::logwarn(
      "DMT_results_to_long(): doppelte Labels gefunden (%s) - vermutlich Daten von VOR dem Label-Fix. Alle Vorkommen werden trotzdem uebernommen, bitte Datenqualitaet pruefen.",
      paste(unique(dup_labels), collapse = ", ")
    )
  }

  rows <- purrr::map_dfr(keep_idx, function(i) {

    label  <- label_names[i]
    answer <- results_list[[i]]

    if (is.null(answer) || is.null(answer$res_summary)) {
      logging::logwarn("DMT_results_to_long(): '%s' hat kein res_summary, wird uebersprungen.", label)
      return(NULL)
    }

    # Fallback fuer aeltere Daten (vor der dmt_get_answer()-Erweiterung):
    # trial_no/attempt notfalls aus dem Label parsen.
    core_label <- sub("^DMT_(demo_)?trial_", "", label)
    label_nums <- as.integer(unlist(strsplit(core_label, "_attempt_")))

    trial_no <- answer$trial_no %||% label_nums[1]
    attempt  <- answer$attempt  %||% label_nums[2]

    inst_wide <- dmt_res_summary_wide(answer$res_summary)

    tibble::tibble(
      p_id                 = p_id,
      trial_no             = trial_no,
      stimulus_id          = as.character(answer$stimulus_id %||% NA_character_),
      demo                 = answer$demo %||% grepl("^DMT_demo_trial_", label),
      source               = answer$source %||% NA_character_,
      complexity_half      = answer$complexity_half %||% NA_character_,
      complexity           = answer$complexity %||% NA_real_,
      attempt              = attempt,
      cumulative_attempt   = answer$cumulative_attempt %||% NA_integer_,
      feedback_layer_shown = answer$feedback_layer_shown %||% NA_integer_,
      global_correct       = answer$global_correct %||% NA
    ) %>%
      dplyr::bind_cols(inst_wide) %>%
      dplyr::mutate(
        timed_out = answer$timed_out %||% NA,
        rt_ms     = answer$rt_ms %||% NA_real_,
        timestamp = answer$timestamp %||% as.POSIXct(NA)
      )
  })

  if (nrow(rows) == 0) return(rows)

  if (!include_demo) {
    rows <- dplyr::filter(rows, !isTRUE(demo))
  }

  rows %>%
    dplyr::select(
      p_id, trial_no, stimulus_id, demo, source, complexity_half, complexity,
      attempt, cumulative_attempt, feedback_layer_shown,
      global_correct,
      hihat_hits, hihat_n, snare_hits, snare_n, kick_hits, kick_n,
      n_total_mistakes,
      timed_out, rt_ms, timestamp
    ) %>%
    dplyr::arrange(cumulative_attempt, trial_no, attempt)
}

# ------------------------------------------------------------------
# Alle Teilnehmer eines Ergebnis-Ordners -> Long-Format
# ------------------------------------------------------------------

#' Convert all DMT result RDS files in a directory into one long tibble
#'
#' Liest alle `.rds`-Ergebnisdateien in `dir` ein und wendet
#' \code{\link{DMT_results_to_long}} auf jede an, um einen Datensatz ueber
#' alle Teilnehmer hinweg zu bauen.
#'
#' @param dir Pfad zum Ergebnis-Ordner (z.B. "output/results").
#' @param pattern Dateimuster fuer `list.files()`. Default: alle .rds.
#' @param include_demo s. \code{\link{DMT_results_to_long}}.
#'
#' @returns Ein Tibble ueber alle Teilnehmer.
#' @export
DMT_results_dir_to_long <- function(dir, pattern = "\\.rds$", include_demo = TRUE) {

  files <- list.files(dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    logging::logwarn("DMT_results_dir_to_long(): keine Dateien in '%s' gefunden.", dir)
    return(tibble::tibble())
  }

  purrr::map_dfr(files, function(f) {

    res <- readRDS(f)

    fallback_id <- tools::file_path_sans_ext(basename(f))
    p_id <- res[["session"]][["p_id"]] %||% fallback_id

    DMT_results_to_long(res, p_id = p_id, include_demo = include_demo)
  })
}
