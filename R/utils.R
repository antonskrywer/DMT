

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


