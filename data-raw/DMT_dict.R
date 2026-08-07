# data_raw/DMT_dict.R
#
# Erzeugt DMT_dict (Klasse psychTestR::i18n_dict) aus DMT_dict.xlsx
# und speichert es als internes Paket-Datenobjekt (data/DMT_dict.rda),
# analog zu sebsilas/SLT::data_raw/SLT_dict.R.
#
# Workflow:
#   1. DMT_dict.xlsx unter data_raw/ ablegen (Spalten: key, DE, DE_F, EN)
#   2. Dieses Skript einmalig ausführen (z.B. via devtools::load_all()
#      oder direkt per source("data_raw/DMT_dict.R"))
#   3. data/DMT_dict.rda wird committet und ist danach über DMT_dict
#      als lazy-loaded Paketdaten verfügbar (wie drum_matrix,
#      easy_stimuli_drum_matrix)

DMT_dict_raw <- readxl::read_xlsx("data-raw/DMT_dict.xlsx")

# i18n_dict erwartet: erste Spalte "key", danach eine Spalte je Sprachcode.
# Reihenfolge/Namen müssen zu DMT_languages() passen (c("en","de","de_f")).
DMT_dict_raw <- DMT_dict_raw[, c("key", "EN", "DE", "DE_F")]

DMT_dict <- psychTestR::i18n_dict$new(DMT_dict_raw)

usethis::use_data(DMT_dict, overwrite = TRUE)
