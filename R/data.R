#' DMT dictionary
#'
#' The default internationalisation dictionary used by the DMT.
#' Contains translations for keys used throughout the DMT package
#' (intro, trial UI, feedback) in three languages: English ("en"),
#' informal German ("de"), and formal German ("de_f").
#' Built from \code{data-raw/DMT_dict.xlsx} via \code{data-raw/DMT_dict.R}.
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
#' The drum patterns used for the practice trials before the main test,
#' in order of use (\code{TrialNo}): Easy_1 (kick on 1 and 3), Practice_2
#' (kick on 1, snare on 2 and 4, hi-hat on 3), Easy_2 (snare), Easy_3
#' (hi-hat) and Easy_4 (kick and snare).
#' @name demo_drum_matrix
#' @docType data
NULL
