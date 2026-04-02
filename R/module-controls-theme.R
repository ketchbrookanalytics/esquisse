
#' Controls for theme
#'
#' Set color, palette, theme, legend position
#'
#' @param id Module ID.
#' @param style Custom CSS styles for the container.
#'
#' @noRd
#'
#' @importFrom utils head
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets radioGroupButtons colorPickr virtualSelectInput
controls_theme_ui <- function(id, style = NULL, default_opts) {

  ns <- NS(id)

  themes <- get_themes()


  tags$div(
    class = "esquisse-controls-theme-container",
    style = style,
    shinyWidgets::virtualSelectInput(
      inputId = ns("theme"),
      label = i18n("Theme:"),
      choices = themes,
      selected = getOption("esquisse.default.theme", default = "theme_minimal"),
      dropboxWrapper = ".esquisse-controls-theme-container",
      optionsCount = 5,
      width = "100%"
    ),
    input_legend_options(ns, default_opts)
  )
}


#' @importFrom shiny observeEvent observe req reactive
controls_theme_server <- function(id) {
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      inputs_r <- reactive({

        legend_position <- input$legend_position
        if (identical(legend_position, "right"))
          legend_position <- NULL

        legend_justification <- input$legend_justification
        if (identical(legend_justification, "center"))
          legend_justification <- NULL

        list(
          theme = input$theme,
          legend_position = legend_position,
          legend_justification = legend_justification,
          legend_text = get_axis_text(
            input$legend_text_face,
            input$legend_text_size
          ),
          legend_title = get_axis_text(
            input$legend_title_face,
            input$legend_title_size
          )
        )
      })

      return(list(inputs = inputs_r))
    }
  )
}


input_legend_text <- function(type = c("text", "title"), ns = identity, default_opts) {
  type <- match.arg(type)
  tagList(
    tags$p(capitalize(type), "options:"),
    tags$div(
      style = css(
        display = "grid",
        gridTemplateColumns = "repeat(2, 1fr)",
        gridColumnGap = "2px"
      ),
      shinyWidgets::virtualSelectInput(
        inputId = ns(paste0("legend_", type, "_face")),
        label = i18n("Font face:"),
        choices = setNames(
          c("plain", "italic", "bold", "bold.italic"),
          c("Plain", "Italic", "Bold", "Bold/Italic")
        ),
        width = "100%"
      ),
      numericInput(
        inputId = ns(paste0("legend_", type, "_size")),
        label = i18n("Size:"),
        value = if(type == "text") default_opts$legend_text_size else default_opts$legend_title_size,
        min = 0,
        width = "100%"
      )
    )
  )
}

input_legend_options <- function(ns, default_opts) {
  tagList(
    tags$hr(),
    tags$b("Legend options:"),
    selectInput(
      inputId = ns("legend_position"),
      label = i18n("Position:"),
      selected = "right",
      choices = c("Left" = "left", "Top" = "top", "Bottom" = "bottom", "Right" = "right", "None" = "none"),
      width = "100%"
    ),
    selectInput(
      inputId = ns("legend_justification"),
      label = i18n("Justification:"),
      selected = "center",
      choices = c("Left" = "left", "Top" = "top", "Bottom" = "bottom", "Right" = "right", "Center" = "center"),
      width = "100%"
    ),
    input_legend_text("text", ns = ns, default_opts),
    input_legend_text("title", ns = ns, default_opts)
  )
}
