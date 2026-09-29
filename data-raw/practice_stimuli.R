# data-raw/practice_stimuli.R
#
# Uebungs-Trials fuer den Kinder-Pilot (Entscheidung Anton 2026-09-29):
#   1. Easy_1     - nur Bassdrum auf Zaehlzeit 1 und 3 (Positionen 1, 9)
#   2. Practice_2 - Bassdrum auf 1 (1), Snare auf 2 und 4 (5, 13),
#                   Hi-Hat auf 3 (9). Kein Instrument faellt auf dieselbe
#                   Zaehlzeit wie ein anderes (leichter als ein Rock-Beat mit
#                   Hi-Hat auf allen Zaehlzeiten).
# Practice_2 ist ein neues Pattern und kommt in keinem Haupttest-Pool vor
# (geprueft: easy_stimuli_drum_matrix, drum_matrix) - der Haupttest-Pool
# bleibt unveraendert. Eigener Name "Practice_2", weil die "Easy_<n>"-Namen
# bereits im Haupttest-Pool vergeben sind. Die uebrigen Uebungs-Stimuli
# (Easy_2/3/4) bleiben in demo_drum_matrix und kommen nur bei
# num_examples > 2 zum Einsatz.
#
# WICHTIG: Dieses Skript aendert data/demo_drum_matrix.rda gezielt, statt
# data-raw/stimuli.R neu laufen zu lassen. Die aktuellen .rda wurden noch
# mit der alten cut_number()-Einteilung gebaut; stimuli.R nutzt inzwischen
# ntile() - ein Neubau wuerde also die Strata des Haupttests veraendern.
#
# Idempotent: bei erneutem Ausfuehren passiert nichts, wenn Practice_2 schon
# vorhanden ist.

devtools::load_all(".")

# R/complexity.R nutzt dplyr/tidyr-Funktionen ohne Namespace (wie
# data-raw/stimuli.R, das library(tidyverse) laedt)
library(dplyr)
library(tidyr)

practice_id <- "Practice_2"

if (practice_id %in% demo_drum_matrix$Stimulus) {

  message(practice_id, " ist bereits Uebungs-Stimulus - nichts zu tun.")

} else {

  new_rows <- tibble::tibble(
    OriginalStimulusId    = NA_real_,  # kein Eintrag in DMT_instruction_stimuli.csv
    Audiofile             = NA,
    Instrument            = c("Kick", "Snare", "Snare", "HiHat"),
    Seconds               = NA,
    Beats                 = 1,         # in demo_drum_matrix = Onset-Flag
    BeatPositionSixteenth = c(1, 5, 13, 9),
    Stimulus              = practice_id,
    Complexity            = NA_real_,
    TrialNo               = NA_integer_
  )

  # Complexity wie in data-raw/stimuli.R (Senn-Modell). Vorher pruefen, dass
  # die Berechnung den gespeicherten Wert eines bestehenden Demo-Stimulus
  # exakt reproduziert.
  check <- predict_complexity(stimuli_df_to_matrix(demo_drum_matrix, "Easy_1"))
  stopifnot(isTRUE(all.equal(check, unique(demo_drum_matrix$Complexity[demo_drum_matrix$Stimulus == "Easy_1"]))))

  new_rows$Complexity <- predict_complexity(stimuli_df_to_matrix(new_rows, practice_id))

  # Reihenfolge der Uebungs-Trials = TrialNo (DMT_training() nimmt 1..num_examples)
  practice_order <- c("Easy_1", practice_id, "Easy_2", "Easy_3", "Easy_4")

  demo_drum_matrix <- dplyr::bind_rows(demo_drum_matrix, new_rows) %>%
    dplyr::mutate(TrialNo = match(Stimulus, practice_order)) %>%
    dplyr::arrange(TrialNo, Instrument, BeatPositionSixteenth)

  stopifnot(!anyNA(demo_drum_matrix$TrialNo))

  usethis::use_data(demo_drum_matrix, overwrite = TRUE)
}
