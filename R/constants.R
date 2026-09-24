
dmt_resources <- function() {

  shiny::addResourcePath("audio", system.file('audio', package = "DMT"))

  shiny::addResourcePath("css", system.file("css", package = "DMT"))

  shiny::addResourcePath("js", system.file("js", package = "DMT"))

}

dmt_ui_header <- function(load_tone_js = TRUE) {
  shiny::tags$head(
    shiny::tags$link(rel = "icon", href = "data:,"),

    # Tone.js nur laden, wenn es nicht schon da ist (DMT_standalone() laedt es
    # global ueber additional_scripts): ein erneutes Laden pro Seite wuerde
    # eine neue Tone-Instanz/AudioContext erzeugen und die gecachten Samples
    # (window.dmtDrum) ungueltig machen.
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
