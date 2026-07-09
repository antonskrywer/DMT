

# devtools::install()

# devtools::install_github("sebsilas/DMT")

devtools::load_all(".")

# DMT_standalone(tempo = 100, num_trials = 5L)


# DMT_standalone(tempo = 100, num_trials = 36L, num_examples = 0L, with_feedback = FALSE)

DMT_standalone(tempo = 100,
               num_trials = 7L,
               num_examples = 3L,
               with_feedback = TRUE,
               custom_stratified_sampling_allocation = list(
                 easy_easy = 2,
                 easy_hard = 2,
                 normal_easy = 2,
                 normal_hard = 1
               ),
               trial_timeout = 90
               )

