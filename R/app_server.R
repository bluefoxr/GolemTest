#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # LOAD DATA ---------------------------------------------------------------

  coin <- reactiveVal({NULL})

  observeEvent(input$load_click, {

    req(input$file)

    output$text <- renderText({
      input$file$name
    })

    data_message <- capture.output({
      coin(f_data_input(input$file$datapath))
    }, type = "message")

    # Outputs
    output$data_message <- renderText(data_message, sep = "\n")
    output$coin_print <- renderPrint(f_print_coin(coin()))
    output$framework <- renderPlotly({f_plot_framework(coin())})
  })


  # ANALYSE INDICATORS ------------------------------------------------------

  # if stats table doesn't exist yet, this will be null
  # they will be re-accessed every time the coin changes
  l_analysis <- reactiveVal(NULL)

  # observe coin: extract analysis tables on changes or return NULL if don't exist
  observeEvent(coin(),{
    if(is.null(coin()$Analysis$Raw$FlaggedStats)){
      l_analysis(NULL)
    } else {
      l_analysis(
        coin()$Analysis$Raw[c("FlaggedStats", "Flags")]
      )
    }
  })

  # Generate and display results table
  output$analysis_table <- renderDT({

    no_analysis_table <- is.null(l_analysis())

    if(no_analysis_table){
      # add analysis table to coin - this will update reactives
      coin(f_analyse_indicators(coin()))
    }

    # generate table
    #return(f_display_indicator_analysis(coin()))
    f_display_indicator_analysis(l_analysis(), filter_to_flagged = FALSE)

  })

  # Filter table
  observeEvent(input$filter_table, {
    if(input$filter_table){
      l_filtered <- filter_to_flagged(l_analysis()$FlaggedStats, l_analysis()$Flags)
      l_analysis(l_filtered)
    } else {
      l_analysis(
        coin()$Analysis$Raw[c("FlaggedStats", "Flags")]
      )
    }
  })

  # Remove indicators
  observeEvent(input$remove_indicator, {

    # get code and remove, regenerate coin
    icode_to_remove <- l_analysis()$FlaggedStats$iCode[input$analysis_table_rows_selected]
    coin(f_remove_indicators(coin(), icode_to_remove))

  })

  # Add indicators
  observeEvent(input$add_indicator, {

    # get code and remove, regenerate coin
    icode_to_remove <- l_analysis()$FlaggedStats$iCode[input$analysis_table_rows_selected]
    coin(f_add_indicators(coin(), icode_to_remove))

  })

}
