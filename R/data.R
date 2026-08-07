# R/data.R
#
# Roxygen-Dokumentation für alle exportierten Datenobjekte des DMT-Pakets.
# Jedes Objekt liegt als .rda unter data/ (erzeugt via usethis::use_data()
# aus den jeweiligen data_raw/*.R-Skripten) und wird beim Package-Build
# per Lazy-Loading verfügbar gemacht.

#' DMT dictionary
#'
#' The default internationalisation dictionary used by the DMT.
#' Contains translations for keys used throughout the DMT package
#' (intro, trial UI, feedback) in three languages: English ("en"),
#' informal German ("de"), and formal German ("de_f").
#' Built from \code{data_raw/DMT_dict.xlsx} via \code{data_raw/DMT_dict.R}.
#' @name DMT_dict
#' @docType data
NULL

#' DMT drum matrix (normal difficulty)
#'
#' The item bank of "normal"-difficulty drum patterns used in the main
#' trials of the Drum Machine Test. Each row represents one onset
#' (Instrument x BeatPositionSixteenth) belonging to a given Stimulus.
#' @name drum_matrix
#' @docType data
NULL

#' DMT easy stimuli drum matrix
#'
#' The item bank of "easy"-difficulty drum patterns, used alongside
#' \code{\link{drum_matrix}} for stratified sampling of main trials.
#' @name easy_stimuli_drum_matrix
#' @docType data
NULL

#' DMT demo/instruction drum matrix
#'
#' The item bank of drum patterns used for the practice trials
#' (\code{DMT_training()} / \code{DMT_demo_loop()}) shown before the
#' main test, including the four hardcoded pedagogical instruction
#' stimuli (kick, snare, hi-hat, kick+snare).
#' @name demo_drum_matrix
#' @docType data
NULL
