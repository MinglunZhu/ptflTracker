#consts
CYBER_COLORS <- c(
  "#00fff2", "#00b4ff", "#0051ff",   # Blue spectrum
  "#7700ff", "#b300ff", "#ff00f7",   # Purple spectrum
  "#1bffad", "#45ff70", "#c9ff33"    # Green spectrum
)
#end consts

# UI Function for the holdings Chart Module
hldgsUI_sldr <- function(id) {
    ns <- NS(id) # Namespace function

    tags$li(
        sliderInput(
            ns("selectedDate"), "Select Date:",
            min = start_date,
            max = runDate,
            value = runDate,
            timeFormat = "%Y-%m-%d", 
            width = "100%",
            animate = animationOptions(
                interval = 1000,   # milliseconds between frames
                loop = T      # continue playing in a loop
            )
        ) # end sliderInput
    )
}

hldgsUI_plot <- function(id) {
    ns <- NS(id) # Namespace function
   
    fluidRow(
        column(
            6,
            plotlyOutput(
                ns("fundsPie"),
                height = "100vh",
                width = "100%"
            )
        ),
        column(
            6,
            plotlyOutput(
                ns("tickersPie"),
                height = "100vh",
                width = "100%"
            )
        )
    )
}
    

# Server Function for the Returns Chart Module
hldgsServer <- function(
    id, end_date_rv, selectedChart, selectedFunds_rv, selectedTkrs_rv, inclCash_rv, showLegend_rv, disableIpts, enableIpts
) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session$ns

            # Observe changes to end_date_rv and update the slider max
            observeEvent(
                end_date_rv(), 
                {
                    message("Updating slider max to: ", end_date_rv())
                    updateSliderInput(
                        session, "selectedDate",
                        max = end_date_rv()
                    )
                }
            )

            di <- function() {
                disableIpts()

                # without asis param, the namespace will be added to id
                shinyjs::disable('selectedDate')

                sprintf("Shiny.setInputValue('%s', false);", ns("enableIpts")) %>% shinyjs::runjs()
            }

            ei <- function() {
                enableIpts()
                shinyjs::enable('selectedDate')
            }
            
            # needed for reactive context which caches the results
            selectedHldgVals_raw <- reactive({
                di()

                filter(
                    hldgs_df, 
                    date == input$selectedDate
                )
            })

            selectedHldgVals_cashAdj <- reactive({
                filter(
                    selectedHldgVals_raw(), 
                    is.na(isInclCash)
                    | isInclCash == inclCash_rv()
                )
            })

            # Observer to handle plot status changes
            observe({
                req(input$enableIpts)

                ei()
            })

            # Modify fundsPie and tickersPie outputs for a modern/futuristic appearance
            output$fundsPie <- renderPlotly({
              di()

              # Calculate fund values for the selected date
              h <- selectedHldgVals_cashAdj()

              if(nrow(h) == 0) {
                # Return empty plot if no data
                plot_ly() %>%
                  layout(
                    title = "No holdings data available",
                    paper_bgcolor = '#222',
                    plot_bgcolor = '#222',
                    font = list(color = '#eee')
                  )
              } else {
                h %>%
                    plot_ly(
                        ids = ~id,
                        labels = ~id,
                        parents = ~parent,
                        values = ~val,
                        type = 'sunburst',
                        branchvalues = 'total', # Values represent the total sum of their children
                        source = "fundsSunburst", # Changed source name
                        #customdata = ~ids, # Pass ID for click events
                        hoverinfo = 'label+percent entry+value',
                        marker = list(
                            line = list(
                                color = 'rgba(0, 255, 242, 0.5)', 
                                width = 1.5
                            ) # Thicker cyan lines
                        ),
                        insidetextorientation = 'radial',
                        opacity = 0.95
                    ) %>%
                    layout(
                        title = list(
                            text = "Daily USD Holdings Values", 
                            font = list(color = '#00fff2')
                        ),
                        paper_bgcolor = '#1a1a1a', # Darker background
                        plot_bgcolor = '#1a1a1a',
                        font = list(
                            color = '#eee', 
                            family = "Orbitron, sans-serif"
                        ), # Futuristic font
                        colorway = CYBER_COLORS # Apply cyber colors cyclically
                    ) %>%
                    onRender(sprintf(
                        "
                            function(el, x) {
                                el.on('plotly_afterplot', function() {
                                    console.log('Plot finished rendering');
                                    Shiny.setInputValue('%s', true, {priority: 'event'});
                                });
                            }
                        ", 
                        ns("enableIpts")
                    ))
              }
            })

            

            # Render tickers pie chart.
            # output$tickersPie <- renderPlotly({
            #   disableIpts()

            #   # Ensure inputs are re-enabled
            #   on.exit({ enableIpts() })

            #   h <- selectedHldgVals_tkrs()

            #   if(nrow(h) == 0) {
            #     # Return empty plot if no data
            #     plot_ly() %>%
            #       layout(
            #         title = "No holdings data available",
            #         paper_bgcolor = '#222',
            #         plot_bgcolor = '#222',
            #         font = list(color = '#eee')
            #       )
            #   } else {
            #     sfs <- selectedFunds_hldgs()
            #     fn <- if_else(
            #       length(sfs) == 0,
            #       'Overall Portfolio',
            #       paste(
            #         sfs,
            #         collapse = " + "
            #       )
            #     )

            #     h %>%
            #       plot_ly(
            #         labels = ~name,
            #         values = ~val,
            #         type = 'pie',
            #         hole = 0.6,
            #         textinfo = 'label+percent',
            #         insidetextorientation = 'radial',
            #         marker = list(
            #           line = list(
            #             color = 'rgba(255, 255, 255, 0.3)',  # White glow effect
            #             width = 1
            #           ),
            #           colors = colorRampPalette(c("#ffffff", "#e6e6ff", "#ccccff", "#b3b3ff"))(nrow(h)) # White to light blue gradient
            #         ),
            #         hoverinfo = 'label+percent+value',
            #         opacity = 0.95,
            #         direction = 'clockwise'
            #       ) %>%
            #       createPieLayout(
            #         paste0("Holdings by Ticker (", fn, ")"),
            #         list(
            #           family = "-apple-system",
            #           size = 18,
            #           color = "#ffffff"  # White text
            #         ),
            #         list(color = '#ffffff') # White text for labels
            #       )
            #   }
            # })
        }
    ) # End moduleServer
}