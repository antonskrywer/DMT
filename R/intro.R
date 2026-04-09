
DMT_intro <- function(tempo) {

  psychTestR::join(

    # --------------------------------------------------
    # 1. Welcome
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong("Welcome to the Drum Machine Test")),
        shiny::tags$p("This test measures your musical learning ability."),
        shiny::tags$p("You will learn to recreate drum patterns using a virtual drum machine.")
      )
    ),

    # --------------------------------------------------
    # 2. General reassurance
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p("The difficulty will adapt to your performance."),
        shiny::tags$p("Don't worry if it feels difficult at first.")
      )
    ),

    # --------------------------------------------------
    # 3. How it works (structure + layers)
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong("How it works")),
        shiny::tags$p("Each pattern consists of three layers:"),
        shiny::tags$ul(
          shiny::tags$li("Hi-hat (top row)"),
          shiny::tags$li("Snare (middle row)"),
          shiny::tags$li("Bass drum (bottom row)")
        )
      )
    ),

    # --------------------------------------------------
    # 4. Grid + interaction
    # --------------------------------------------------
    psychTestR::page(
      ui = shiny::tags$div(
        dmt_ui_header(),
        dmt_ui(trial_no = NULL,
               num_trials = NULL,
               stimulus_json = NULL,
               tempo = tempo),
        shiny::tags$p("The grid represents one bar divided into 16 steps."),
        shiny::tags$p("Click to activate or deactivate sounds."),
        shiny::tags$p("Feel free to explore how this works below and click play to hear your input."),
        shiny::tags$button("Next", class = "btn", onclick = "window.stopDMT();next_page();")
      )
    ),

    # --------------------------------------------------
    # 5. Timing + goal
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p("Your goal is to recreate a given pattern as accurately as possible.")
      )
    ),

    # --------------------------------------------------
    # 6. Feedback intro
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong("Feedback system")),
        shiny::tags$p("After each attempt, you will be told:"),
        shiny::tags$ol(
          shiny::tags$li("Correct / Incorrect"),
          shiny::tags$li("Which layer contains mistakes"),
          shiny::tags$li("How many mistakes per layer"),
          shiny::tags$li("Full correct pattern")
        )
      )
    ),

    # --------------------------------------------------
    # 7. Practice trials
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong("Practice trials")),
        shiny::tags$p("Before the main task, you will complete a few practice trials."),
        shiny::tags$p("You will receive feedback after each attempt.")
      )
    ),

    # --------------------------------------------------
    # 8. Reassurance
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p("You can repeat each pattern multiple times."),
        shiny::tags$p("It is completely normal to make mistakes at first.")
      )
    ),

    # --------------------------------------------------
    # 9. Strategy tip
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong("Tip")),
        shiny::tags$p("Focus on one layer at a time (hi-hat, snare, bass)."),
        shiny::tags$p("Build the pattern step by step.")
      )
    )

  ) %>% unlist(recursive = FALSE)

}
