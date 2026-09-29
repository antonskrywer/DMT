# Lokaler Kurztest (Browser-Pane / .claude/launch.json, Port 8399).
# Kurz gehalten: Uebungs-Trials per Default (2), 2 Haupt-Trials, keine ID-Abfrage, Deutsch.

devtools::load_all(".")

app <- DMT_standalone(tempo = 100,
                      num_trials = 2L,
                      language = "de",
                      with_feedback = TRUE,
                      with_id = FALSE,
                      trial_timeout = 90)

shiny::runApp(app, port = 8399, launch.browser = FALSE)
