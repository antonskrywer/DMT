

# devtools::install()

# devtools::install_github("sebsilas/DMT")

devtools::load_all(".")

# DMT_standalone(tempo = 100, num_trials = 5L)


# DMT_standalone(tempo = 100, num_trials = 36L, num_examples = 0L, with_feedback = FALSE)

DMT_standalone(tempo = 100, num_trials = 10, num_examples = 3L, with_feedback = TRUE)
