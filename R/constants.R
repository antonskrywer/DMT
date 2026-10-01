dmt_strata <- c("easy_easy", "easy_hard", "normal_easy", "normal_hard")

dmt_resources <- function() {

  shiny::addResourcePath("audio", system.file('audio', package = "DMT"))

  shiny::addResourcePath("css", system.file("css", package = "DMT"))

  shiny::addResourcePath("js", system.file("js", package = "DMT"))

}

dmt_ui_header <- function(load_tone_js = TRUE) {
  shiny::tags$head(
    shiny::tags$link(rel = "icon", href = "data:,"),

    # Load Tone.js only once: reloading it on every page would create a new
    # AudioContext and invalidate the cached samples.
    if (load_tone_js)
      shiny::tags$script(shiny::HTML(
        "if (!window.Tone) {
           var dmtToneScript = document.createElement('script');
           dmtToneScript.src = 'https://cdnjs.cloudflare.com/ajax/libs/tone/14.8.49/Tone.js';
           document.head.appendChild(dmtToneScript);
         }"
      )),

    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/dmt.css")
  )
}
