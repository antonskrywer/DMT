feedback_container <- function(...) {
  shiny::tags$div(
    style = "display: flex; justify-content: center; width: 100%;",
    shiny::tags$div(
      style = "width: 60%; min-width: 300px; max-width: 600px;",
      ...
    )
  )
}

shiny_table <- function(content, rownames = FALSE, colnames = TRUE, style = NULL, width = "80%") {
  shiny::tags$div(
    style = style,
    shiny::renderTable({
      content
    }, rownames = rownames, colnames = colnames, width = width
    )
  )
}


is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

# check_sampling_allocation() prueft, ob eine explizit uebergebene
# custom_stratified_sampling_allocation gueltige Stratum-Namen verwendet.
#
# BUGFIX: Ohne den is.null()-Guard gab check_sampling_allocation(NULL)
# faelschlich TRUE zurueck (setdiff(names(NULL), c(...)) == NULL, also
# length(...) == 0), weil names(NULL) ein leerer Vektor ist und ein leerer
# Vektor stets Teilmenge jeder Menge ist. In sample_trials() (DMT.R) fuehrte
# das dazu, dass im ganz normalen Default-Fall (kein custom allocation
# uebergeben) faelschlich der `if`-Zweig statt des Default-Gleichverteilungs-
# Zweigs genommen wurde -> allocation <- NULL -> sample_stratum() zog dann
# via slice_sample(n = min(NULL, nrow(stimuli))) ALLE Items eines Stratums
# statt der beabsichtigten n_per_group Items (min(NULL, x) wird zu min(x),
# da c(NULL, x) == x). Ergebnis: viel zu viele Items wurden gesampelt, und
# durch die feste Blockreihenfolge (easy_easy -> easy_hard -> normal_easy ->
# normal_hard) wurden bei kleinem num_trials ausschliesslich easy_easy-Items
# praesentiert, normal_easy/normal_hard nie erreicht.
#
# Mit dem Guard gibt check_sampling_allocation(NULL) jetzt korrekt FALSE
# zurueck, sodass sample_trials() bei NULL zuverlaessig in den Default-
# Gleichverteilungs-Zweig läuft.
check_sampling_allocation <- function(custom_stratified_sampling_allocation) {

  if (is.null(custom_stratified_sampling_allocation)) {
    return(FALSE)
  }

  length(setdiff(names(custom_stratified_sampling_allocation),
                 c("easy_easy", "easy_hard", "normal_easy", "normal_hard") )
  ) == 0
}

# ------------------------------------------------------------------
# %||%: Base R hat diesen Operator erst ab Version 4.4.0 eingebaut.
# Der Server läuft mit R 4.1.2, deshalb muss die Funktion hier selbst
# definiert werden, sonst crasht das Paket dort mit
# "could not find function %||%", sobald dmt_get_answer() aufgerufen
# wird (siehe Server-Crash-Bug beim ersten Demo-Feedback-Schritt).
# ------------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
