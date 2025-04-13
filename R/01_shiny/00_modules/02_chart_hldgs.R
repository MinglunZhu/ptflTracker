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
            max = RUN_DATE,
            value = RUN_DATE,
            timeFormat = "%Y-%m-%d", 
            width = "100%",
            animate = animationOptions(
                interval = 1000,   # milliseconds between frames
                loop = T      # continue playing in a loop
            )
        ) # end sliderInput
    )
}

hldgsUI_slct_plotType <- function(id) {
    ns <- NS(id) # Namespace function

    tags$li(
        selectInput(
            ns("selectedChartType"), 'Select Chart Type:',
            choices = c("Sunburst", "Treemap", 'Icicle'),
            selected = "Sunburst"
        )
    )
}

hldgsUI_plot <- function(id) {
    ns <- NS(id) # Namespace function
   
    fluidRow(
        column(
            6,
            plotlyOutput(
                ns("plot_hierarchical"),
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
    id, end_date, selectedChart_rv, selectedFunds_rv, inclCash_rv, disableIpts, enableIpts
) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session$ns

            message("Updating slider max to: ", end_date)
            updateSliderInput(
                session, "selectedDate",
                max = end_date,
                value = end_date
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
            
            isFirst_chart <- T

            # needed for reactive context which caches the results
            selectedHldgVals_raw <- eventReactive(
                input$selectedDate,
                {
                    if (isFirst_chart) isFirst_chart <<- F
                    else req(selectedChart_rv() == 'Holdings')

                    di()

                    filter(
                        hldgs_df, 
                        date == input$selectedDate
                    )
                }
            )

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
            output$plot_hierarchical <- renderPlotly({
                di()

                # Calculate fund values for the selected date
                h <- selectedHldgVals_cashAdj()

                if(nrow(h) == 0) {
                    # Return empty plot if no data
                    plot_ly() %>%
                    layout(
                        title = "No holdings data available",
                        paper_bgcolor = BG_COLOR,
                        plot_bgcolor = BG_COLOR,
                        font = list(color = '#eee')
                    )
                } else {
                    h %>%
                        plot_ly(
                            ids = ~id,
                            labels = ~lbl,
                            parents = ~parent,
                            values = ~val,

                            type = 'sunburst',
                            branchvalues = 'total', # Values represent the total sum of their children

                            #source = "fundsSunburst", # Changed source name
                            #customdata = ~ids, # Pass ID for click events
                            hoverinfo = 'label+percent entry+value',

                            marker = list(
                                colors = genCyberColors(h),
                                line = list(
                                    color = 'rgba(0, 255, 242, 0.3)',
                                    width = 2
                                ) # Thicker cyan lines
                            ),
                            insidetextorientation = 'radial',
                            opacity = 0.95,
                            textfont = list(
                                family = "Orbitron, monospace",
                                color = '#fff'
                            )
                        ) %>%
                        layout(
                            title = list(
                                text = "USD Holdings Values", 
                                font = list(
                                    color = '#00fff2',
                                    family = "Orbitron, monospace"
                                )
                            ),
                            paper_bgcolor = BG_COLOR, # Darker background
                            plot_bgcolor = BG_COLOR,
                            font = list(
                                color = '#00fff2', 
                                family = "Orbitron, monospace"
                            ) # Futuristic font
                            #colorway = CYBER_COLORS # Apply cyber colors cyclically
                        ) %>%
                        # the mode bar offers download plot as png, not very useful
                        # but we can still leave it in there
                        # config(
                        #     displayModeBar = FALSE  # Hide the modebar for cleaner look
                        # ) %>%
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

            plot_proxy <- plotlyProxy("plot_hierarchical", session)

            observeEvent(
                input$selectedChartType, 
                { plotlyProxyInvoke( plot_proxy, "restyle", list(type = tolower(input$selectedChartType)) ) },
                ignoreInit = T
            )

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