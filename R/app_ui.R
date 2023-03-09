#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    navbarPage(
      "iCOINr",

      theme = shinythemes::shinytheme("flatly"),

      tabPanel(
        "Data input",
        sidebarLayout(
          sidebarPanel(
            fileInput("file", "Load Data", buttonLabel = "Browse..."),
            actionButton("load_click", "Load"),
            verbatimTextOutput("data_message"),
            verbatimTextOutput("coin_print")
          ),
          mainPanel(
            plotlyOutput("framework", height = "800px")
          )
        )
      ),

      tabPanel(
        "Selection",
        fluidRow(
          column(
            8,
            DTOutput("analysis_table"),
            actionButton("remove_indicator", label = "Remove indicator"),
            actionButton("add_indicator", label = "Add indicator"),
            checkboxInput("filter_table", "Filter to flagged indicators", value = FALSE)
          ),
          column(
            4,
            plotlyOutput("violin_plot", height = "700px")
          )
        )
      ),
      tabPanel(
        "Build",
        fillPage(
          leaflet::leafletOutput("map", width = "700px", height = "700px")
        )

      ),
      tabPanel("Results")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "GolemTest"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
