# Short local test run (2 main trials, German, no ID page)

devtools::load_all(".")

app <- DMT_standalone(tempo = 100,
                      num_trials = 2L,
                      language = "de",
                      with_feedback = TRUE,
                      with_id = FALSE,
                      trial_timeout = 90)

shiny::runApp(app, port = 8399, launch.browser = FALSE)
