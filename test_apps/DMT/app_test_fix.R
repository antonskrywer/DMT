# Lokaler Kurztest (Browser-Pane / .claude/launch.json, Port 8399).
# Kurz gehalten: 1 Uebungs-Trial, 2 Haupt-Trials, keine ID-Abfrage.

devtools::load_all(".")

app <- DMT_standalone(tempo = 100,
                      num_trials = 2L,
                      num_examples = 1L,
                      with_feedback = TRUE,
                      with_id = FALSE,
                      trial_timeout = 90)

shiny::runApp(app, port = 8399, launch.browser = FALSE)
