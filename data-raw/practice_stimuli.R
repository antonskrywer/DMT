# Adds the practice pattern Practice_2 (kick on 1, snare on 2 and 4, hi-hat
# on 3) to demo_drum_matrix and sets the order of the practice trials.
#
# This updates data/demo_drum_matrix.rda directly instead of re-running
# data-raw/stimuli.R, which would also rebuild the main item banks.

devtools::load_all(".")

practice_id <- "Practice_2"

if (practice_id %in% demo_drum_matrix$Stimulus) {

  message(practice_id, " is already in demo_drum_matrix.")

} else {

  new_rows <- tibble::tibble(
    OriginalStimulusId    = NA_real_,
    Audiofile             = NA,
    Instrument            = c("Kick", "Snare", "Snare", "HiHat"),
    Seconds               = NA,
    Beats                 = 1,
    BeatPositionSixteenth = c(1, 5, 13, 9),
    Stimulus              = practice_id,
    Complexity            = NA_real_,
    TrialNo               = NA_integer_
  )

  # Check that the complexity model reproduces the stored values
  check <- predict_complexity(stimuli_df_to_matrix(demo_drum_matrix, "Easy_1"))
  stopifnot(isTRUE(all.equal(check, unique(demo_drum_matrix$Complexity[demo_drum_matrix$Stimulus == "Easy_1"]))))

  new_rows$Complexity <- predict_complexity(stimuli_df_to_matrix(new_rows, practice_id))

  practice_order <- c("Easy_1", practice_id, "Easy_2", "Easy_3", "Easy_4")

  demo_drum_matrix <- dplyr::bind_rows(demo_drum_matrix, new_rows) %>%
    dplyr::mutate(TrialNo = match(Stimulus, practice_order)) %>%
    dplyr::arrange(TrialNo, Instrument, BeatPositionSixteenth)

  stopifnot(!anyNA(demo_drum_matrix$TrialNo))

  usethis::use_data(demo_drum_matrix, overwrite = TRUE)
}
