# Mini-Pilot mit Kindern (5-10 Kinder, JMS Hamburg), Konfiguration vom
# 2026-09-29 (Entscheidungen Anton):
#   - 6 Haupt-Trials, Schwerpunkt leicht, Reihenfolge leicht -> schwer:
#     2 easy_easy, 2 easy_hard, 1 normal_easy, 1 normal_hard
#   - 2 Uebungs-Trials (Default: Easy_1, Practice_2)
#   - Deutsch (du-Form), Participant-ID-Abfrage an
#   - 90 s pro Versuch, bis zu 4 Versuche mit gestufter Hilfe
#
# Ausgewertet wird mit analysis/pilot_kids_analysis.R.
#
# Auf dem Server laeuft diese App mit dem INSTALLIERTEN Paket (library(DMT)),
# nicht mit devtools::load_all() - das Paket muss also nach jedem git pull
# neu installiert werden. Lokal testen: im Paket-Ordner
#   devtools::install()
#   shiny::runApp("test_apps/DMT_pilot")

library(DMT)

DMT_standalone(
  tempo = 100,
  num_trials = 6L,
  custom_stratified_sampling_allocation = list(
    easy_easy   = 2,
    easy_hard   = 2,
    normal_easy = 1,
    normal_hard = 1
  ),
  with_feedback = TRUE,
  trial_timeout = 90,
  language = "de",
  with_id = TRUE
)
