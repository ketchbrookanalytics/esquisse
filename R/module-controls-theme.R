
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
controls_theme_ui <- function(id, style = NULL, ui_defaults) {

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
      optionsCount = 5,
      dropboxWrapper = "body",
      width = "100%"
    ),
    input_legend_options(ns, ui_defaults)
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


input_legend_text <- function(type = c("text", "title"), ns = identity, ui_defaults) {
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
        dropboxWrapper = "body",
        width = "100%"
      ),
      numericInput(
        inputId = ns(paste0("legend_", type, "_size")),
        label = i18n("Size:"),
        value = if(type == "text") ui_defaults$legend_text_size else ui_defaults$legend_title_size,
        min = 0,
        width = "100%"
      )
    )
  )
}

input_legend_options <- function(ns, ui_defaults) {
  tagList(
    tags$hr(),
    tags$b("Legend options:"),
    radioGroupButtons(
      inputId = ns("legend_position"),
      label = i18n("Position:"),
      choiceNames = list(
        ph("arrow-left", title = i18n("left")),
        ph("arrow-up", title = i18n("top")),
        ph("arrow-down", title = i18n("bottom")),
        ph("arrow-right", title = i18n("right")),
        ph("x", title = i18n("none"))
      ),
      choiceValues = c("left", "top", "bottom", "right", "none"),
      selected = "right",
      justified = TRUE,
      size = "xs"
    ),
    radioGroupButtons(
      inputId = ns("legend_justification"),
      label = i18n("Justification:"),
      choiceNames = list(
        ph("arrow-left", title = i18n("left")),
        ph("arrow-up", title = i18n("top")),
        ph("arrow-down", title = i18n("bottom")),
        ph("arrow-right", title = i18n("right")),
        ph("arrows-in-cardinal", title = i18n("center"))
      ),
      choiceValues = c("left", "top", "bottom", "right", "center"),
      selected = "center",
      justified = TRUE,
      size = "xs"
    ),
    input_legend_text("text", ns = ns, ui_defaults),
    input_legend_text("title", ns = ns, ui_defaults)
  )
}
