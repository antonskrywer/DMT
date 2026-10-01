# Builds DMT_dict from DMT_dict.xlsx (columns: key, EN, DE, DE_F).
# Language columns must match DMT_languages().

DMT_dict_raw <- readxl::read_xlsx("data-raw/DMT_dict.xlsx")

DMT_dict_raw <- DMT_dict_raw[, c("key", "EN", "DE", "DE_F")]

DMT_dict <- psychTestR::i18n_dict$new(DMT_dict_raw)

usethis::use_data(DMT_dict, overwrite = TRUE)
