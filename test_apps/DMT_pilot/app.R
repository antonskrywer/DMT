# Pilot study with children: 6 main trials (mostly easy), German, with ID.

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
