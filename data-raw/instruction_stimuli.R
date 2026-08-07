# data-raw/instruction_stimuli.R

raw <- read.csv(
  "data-raw/DMT_instruction_stimuli.csv",
  stringsAsFactors = FALSE
)

# Erste Spalte ist der unbenannte Zeilenindex ("" -> wird von read.csv zu "X")
if (names(raw)[1] %in% c("", "X")) {
  raw <- raw[, -1]
}

# Feste Reihenfolge: Kick solo -> Snare solo -> HiHat solo -> Kick+Snare kombiniert
instruction_order <- data.frame(
  Stimulus = c(1, 4, 7, 9),
  TrialNo  = 1:4
)

instruction_drum_matrix <- raw %>%
  dplyr::inner_join(instruction_order, by = "Stimulus") %>%
  tidyr::pivot_longer(
    cols = c(hihat, snare, bass),
    names_to = "Instrument",
    values_to = "OnOff"
  ) %>%
  dplyr::filter(OnOff == 1) %>%
  dplyr::mutate(
    Instrument = dplyr::recode(
      Instrument,
      hihat = "HiHat",
      snare = "Snare",
      bass  = "Kick"
    ),
    BeatPositionSixteenth = Beat
  ) %>%
  dplyr::select(TrialNo, Stimulus, Instrument, BeatPositionSixteenth) %>%
  dplyr::arrange(TrialNo, Instrument, BeatPositionSixteenth)

usethis::use_data(instruction_drum_matrix, overwrite = TRUE)
