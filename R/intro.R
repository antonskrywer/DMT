# intro.R
#
# Vektorisiert: alle Teilnehmer-sichtbaren Strings laufen über
# psychTestR::i18n() und Keys aus DMT_dict. Funktioniert nur innerhalb
# des new_timeline(dict = DMT_dict)-Wrappers in DMT.R.

DMT_intro <- function(tempo) {

  psychTestR::join(

    # --------------------------------------------------
    # 1. Welcome
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong(psychTestR::i18n("INTRO_WELCOME"))),
        shiny::tags$p(psychTestR::i18n("INTRO_TEST_DESC")),
        shiny::tags$p(psychTestR::i18n("INTRO_TEST_DESC_2")),
        ),
        button_text = psychTestR::i18n("CONTINUE")
    ),

    # --------------------------------------------------
    # 1.2 Get participant ID
    # --------------------------------------------------
    psychTestR::get_p_id(),

    # --------------------------------------------------
    # 2. General reassurance
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(psychTestR::i18n("INTRO_ADAPTIVE")),
        shiny::tags$p(psychTestR::i18n("INTRO_REASSURANCE")),
        ),
      button_text = psychTestR::i18n("CONTINUE")
    ),

    # --------------------------------------------------
    # 3. How it works (structure + layers)
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong(psychTestR::i18n("HOW_IT_WORKS_TITLE"))),
        shiny::tags$p(psychTestR::i18n("HOW_IT_WORKS_INTRO")),
        shiny::tags$ul(
          shiny::tags$li(psychTestR::i18n("LAYER_HIHAT")),
          shiny::tags$li(psychTestR::i18n("LAYER_SNARE")),
          shiny::tags$li(psychTestR::i18n("LAYER_BASSDRUM"))
        ),
        ),
      button_text = psychTestR::i18n("CONTINUE")
      ),

    # --------------------------------------------------
    # 4. Grid + interaction
    # --------------------------------------------------
    psychTestR::page(
      ui = shiny::tags$div(
        dmt_ui_header(),
        dmt_ui(trial_no = NULL,
               stimulus_id = NULL,
               num_trials = NULL,
               stimulus_json = NULL,
               tempo = tempo),
        shiny::tags$p(psychTestR::i18n("GRID_EXPLANATION")),
        shiny::tags$p(psychTestR::i18n("GRID_INSTRUCTION")),
        shiny::tags$p(psychTestR::i18n("GRID_EXPLORE")),
        shiny::tags$button(psychTestR::i18n("BUTTON_NEXT"), class = "btn", onclick = "window.stopDMT();next_page();")
      )
    ),

    # --------------------------------------------------
    # 5. Timing + goal
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(psychTestR::i18n("GOAL_TEXT")),
        ),
      button_text = psychTestR::i18n("CONTINUE")
    ),

    # --------------------------------------------------
    # 6. Feedback intro
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong(psychTestR::i18n("FEEDBACK_SYSTEM_TITLE"))),
        shiny::tags$p(psychTestR::i18n("FEEDBACK_SYSTEM_INTRO")),
        shiny::tags$ol(
          shiny::tags$li(psychTestR::i18n("FEEDBACK_LIST_1")),
          shiny::tags$li(psychTestR::i18n("FEEDBACK_LIST_2")),
          shiny::tags$li(psychTestR::i18n("FEEDBACK_LIST_3")),
          shiny::tags$li(psychTestR::i18n("FEEDBACK_LIST_4"))
        ),
        ),
      button_text = psychTestR::i18n("CONTINUE")
    ),

    # --------------------------------------------------
    # 7. Practice trials
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong(psychTestR::i18n("PRACTICE_TITLE"))),
        shiny::tags$p(psychTestR::i18n("PRACTICE_INTRO")),
        shiny::tags$p(psychTestR::i18n("PRACTICE_FEEDBACK_NOTE")),
        ),
      button_text = psychTestR::i18n("CONTINUE")
    ),

    # --------------------------------------------------
    # 8. Reassurance
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(psychTestR::i18n("REPEAT_NOTE")),
        shiny::tags$p(psychTestR::i18n("MISTAKES_NORMAL")),
        ),
      button_text = psychTestR::i18n("CONTINUE")
    ),

    # --------------------------------------------------
    # 9. Strategy tip
    # --------------------------------------------------
    psychTestR::one_button_page(
      shiny::tags$div(
        dmt_ui_header(),
        shiny::tags$p(shiny::tags$strong(psychTestR::i18n("TIP_TITLE"))),
        shiny::tags$p(psychTestR::i18n("TIP_TEXT_1")),
        shiny::tags$p(psychTestR::i18n("TIP_TEXT_2")),
        ),
      button_text = psychTestR::i18n("CONTINUE")
    )

  ) %>% unlist(recursive = FALSE)

}
