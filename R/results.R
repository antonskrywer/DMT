# Converts res_summary (one row per instrument) into a single wide row.
# Note that <inst>_n is the grid length (16), not the number of required
# onsets; use <inst>_mistakes == 0 to score an instrument as correct.
dmt_res_summary_wide <- function(res_summary) {

  get_val <- function(inst, col) {
    row <- dplyr::filter(res_summary, as.character(Instrument) == inst)
    if (nrow(row) == 0 || !col %in% names(row)) return(NA_integer_)
    row[[col]][1]
  }

  tibble::tibble(
    hihat_hits     = get_val("HiHat", "NoHits"),
    hihat_n        = get_val("HiHat", "NoPositions"),
    hihat_mistakes = get_val("HiHat", "NoMistakes"),
    snare_hits     = get_val("Snare", "NoHits"),
    snare_n        = get_val("Snare", "NoPositions"),
    snare_mistakes = get_val("Snare", "NoMistakes"),
    kick_hits      = get_val("Kick",  "NoHits"),
    kick_n         = get_val("Kick",  "NoPositions"),
    kick_mistakes  = get_val("Kick",  "NoMistakes"),
    n_total_mistakes = suppressWarnings(sum(res_summary$NoMistakes, na.rm = TRUE))
  )
}

#' Convert one participant's DMT results into long format
#'
#' Builds a tibble with one row per trial attempt from the results of one
#' participant. Only results labelled \code{DMT_trial_<n>_attempt_<a>} or
#' \code{DMT_demo_trial_<n>_attempt_<a>} are used.
#'
#' @param res The results of one participant, either as read from
#'   \code{output/results/*.rds} (a list with \code{$results} and
#'   \code{$session}) or the results list itself.
#' @param p_id Participant ID. If \code{NULL}, it is taken from
#'   \code{res$session$p_id} if available.
#' @param include_demo Whether to include practice trials (column
#'   \code{demo}).
#'
#' @returns A tibble with one row per attempt.
#' @export
DMT_results_to_long <- function(res, p_id = NULL, include_demo = TRUE) {

  stopifnot(is.list(res))

  results_list <- if (!is.null(res[["results"]])) res[["results"]] else res

  if (is.null(p_id)) {
    p_id <- res[["session"]][["p_id"]] %||% NA_character_
  }

  label_names <- names(results_list)

  keep_idx <- grep("^DMT_(demo_)?trial_[0-9]+_attempt_[0-9]+$", label_names)

  if (length(keep_idx) == 0) {
    logging::logwarn("DMT_results_to_long(): No DMT trial results found.")
    return(tibble::tibble())
  }

  dup_labels <- label_names[keep_idx][duplicated(label_names[keep_idx])]
  if (length(dup_labels) > 0) {
    logging::logwarn(
      "DMT_results_to_long(): Duplicated labels found (%s). All occurrences are kept.",
      paste(unique(dup_labels), collapse = ", ")
    )
  }

  rows <- purrr::map_dfr(keep_idx, function(i) {

    label  <- label_names[i]
    answer <- results_list[[i]]

    if (is.null(answer) || is.null(answer$res_summary)) {
      logging::logwarn("DMT_results_to_long(): '%s' has no res_summary, skipping.", label)
      return(NULL)
    }

    # Fall back to the label if trial_no/attempt are not stored in the answer
    core_label <- sub("^DMT_(demo_)?trial_", "", label)
    label_nums <- as.integer(unlist(strsplit(core_label, "_attempt_")))

    trial_no <- answer$trial_no %||% label_nums[1]
    attempt  <- answer$attempt  %||% label_nums[2]

    feedback_res <- results_list[[paste0(label, "_feedback")]]
    feedback_rt_ms <- if (is.list(feedback_res)) feedback_res$feedback_rt_ms %||% NA_real_ else NA_real_

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
      dplyr::bind_cols(dmt_res_summary_wide(answer$res_summary)) %>%
      dplyr::mutate(
        timed_out      = answer$timed_out %||% NA,
        rt_ms          = as.numeric(answer$rt_ms %||% NA_real_),
        stim_plays     = as.integer(answer$stim_plays %||% NA_integer_),
        pattern_plays  = as.integer(answer$pattern_plays %||% NA_integer_),
        feedback_rt_ms = as.numeric(feedback_rt_ms),
        timestamp      = answer$timestamp %||% as.POSIXct(NA)
      )
  })

  if (nrow(rows) == 0) return(rows)

  if (!include_demo) {
    rows <- dplyr::filter(rows, !(demo %in% TRUE))
  }

  rows %>%
    dplyr::select(
      p_id, trial_no, stimulus_id, demo, source, complexity_half, complexity,
      attempt, cumulative_attempt, feedback_layer_shown,
      global_correct,
      hihat_hits, hihat_n, hihat_mistakes,
      snare_hits, snare_n, snare_mistakes,
      kick_hits, kick_n, kick_mistakes,
      n_total_mistakes,
      timed_out, rt_ms, stim_plays, pattern_plays, feedback_rt_ms, timestamp
    ) %>%
    dplyr::arrange(cumulative_attempt, trial_no, attempt)
}

#' Phase durations of one or more DMT sessions
#'
#' Computes the duration of the test phases in minutes from the session start
#' time and the phase timestamps saved during the test. Missing timestamps
#' (e.g. after a dropout or with \code{num_examples = 0}) give \code{NA}.
#'
#' @param x Either a path to a results directory (all \code{.rds} files in it
#'   are read) or the results of one participant as returned by
#'   \code{readRDS()}.
#'
#' @returns A tibble with one row per session and the columns \code{p_id},
#'   \code{complete}, \code{intro_min}, \code{practice_min}, \code{ready_min}
#'   (the page between practice and main test), \code{main_min} and
#'   \code{total_min}.
#' @export
DMT_phase_durations <- function(x) {

  if (is.character(x)) {
    files <- list.files(x, pattern = "[.]rds$", full.names = TRUE)
    return(purrr::map_dfr(files, function(f) {
      out <- DMT_phase_durations(readRDS(f))
      if (is.na(out$p_id)) out$p_id <- tools::file_path_sans_ext(basename(f))
      out
    }))
  }

  results_list <- if (!is.null(x[["results"]])) x[["results"]] else x
  session      <- x[["session"]]

  get_time <- function(label) {
    v <- results_list[[label]]
    if (is.null(v)) as.POSIXct(NA) else as.POSIXct(v)
  }

  t_start        <- session[["time_started"]] %||% as.POSIXct(NA)
  t_intro_end    <- get_time("DMT_phase_intro_end")
  t_practice_end <- get_time("DMT_phase_practice_end")
  t_main_start   <- get_time("DMT_phase_main_start")
  t_main_end     <- get_time("DMT_phase_main_end")

  mins <- function(a, b) as.numeric(difftime(b, a, units = "mins"))

  tibble::tibble(
    p_id         = session[["p_id"]] %||% NA_character_,
    complete     = session[["complete"]] %||% NA,
    intro_min    = mins(t_start, t_intro_end),
    practice_min = mins(t_intro_end, t_practice_end),
    ready_min    = mins(t_practice_end, t_main_start),
    main_min     = mins(t_main_start, t_main_end),
    total_min    = mins(t_start, t_main_end)
  )
}

#' Convert all DMT result files in a directory into long format
#'
#' Applies \code{\link{DMT_results_to_long}()} to every results file in
#' \code{dir} and combines the output.
#'
#' @param dir Path to the results directory (e.g. \code{"output/results"}).
#' @param pattern File name pattern passed to \code{list.files()}.
#' @inheritParams DMT_results_to_long
#'
#' @returns A tibble with one row per attempt across all participants.
#' @export
DMT_results_dir_to_long <- function(dir, pattern = "\\.rds$", include_demo = TRUE) {

  files <- list.files(dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    logging::logwarn("DMT_results_dir_to_long(): No files found in '%s'.", dir)
    return(tibble::tibble())
  }

  purrr::map_dfr(files, function(f) {

    res <- readRDS(f)

    p_id <- res[["session"]][["p_id"]] %||% tools::file_path_sans_ext(basename(f))

    DMT_results_to_long(res, p_id = p_id, include_demo = include_demo)
  })
}
