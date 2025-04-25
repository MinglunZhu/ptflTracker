#consts
# CYBER_COLORS <- c(
#   "#00fff2", "#00b4ff", "#0051ff",   # Blue spectrum
#   "#7700ff", "#b300ff", "#ff00f7",   # Purple spectrum
#   "#1bffad", "#45ff70", "#c9ff33"    # Green spectrum
# )
BDR_WDT <- 1
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

    tagList(
        tags$li(
            selectInput(
                ns("selectedChartType_hierarchical"), 'Select Chart Type (Hierarchical):',
                choices = c("Sunburst", "Treemap", 'Icicle'),
                selected = "Sunburst"
            )
        ),
        tags$li(
            selectInput(
                ns("selectedChartType_flat"), 'Select Chart Type (Flat):',
                choices = c("Pie", "Waffle", 'Stacked Bar'),
                selected = "Pie"
            )
        )
    )
}

hldgsUI_plot <- function(id) {
    ns <- NS(id) # Namespace function

    fluidRow(
        column(
            6,
            id = ns("col_hierarchical"), # Assign an ID
            shinycssloaders::withSpinner(
                plotlyOutput(
                    ns("plot_hierarchical"),
                    height = "100vh",
                    width = "100%"
                ),
                type = 2
            )
        ),
        column(
            6,
            id = ns("col_flat"), # Assign an ID
            plotlyOutput(
                ns("plot_flat"),
                height = "100vh",
                width = "100%"
            )
        )
    )
}

# Server Function for the Returns Chart Module
hldgsServer <- function(
    id, end_date, selectedChart_rv, selectedFunds_rv, inclCash_rv, showColorBar_rv, enableUd_plots_rv, disableIpts, enableIpts
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
                shinyjs::disable('selectedChartType_hierarchical')
                shinyjs::disable('selectedChartType_flat')

                sprintf("Shiny.setInputValue('%s', false);", ns("enableIpts")) %>% shinyjs::runjs()
            }

            ei <- function() {
                enableIpts()
                shinyjs::enable('selectedDate')
                shinyjs::enable('selectedChartType_hierarchical')
                shinyjs::enable('selectedChartType_flat')
            }

            isChanged_date <- F

            needUd_date <- reactiveVal(NULL)

            observeEvent(
                list(enableUd_plots_rv(), selectedChart_rv()),
                {
                    req(
                        enableUd_plots_rv(),
                        selectedChart_rv() == 'Holdings'
                    )

                    if (isChanged_date) Sys.time() %>% needUd_date()
                },
                ignoreInit = T
            )

            observeEvent(
                input$selectedDate,
                {
                    isChanged_date <<- T

                    # date slider is only available in returns chart
                    req(enableUd_plots_rv())

                    Sys.time() %>% needUd_date()
                }
            )

            # needed for reactive context which caches the results
            selectedHldgVals_raw <- eventReactive(
                needUd_date(),
                {
                    isChanged_date <<- F

                    di()

                    filter(
                        hldgs_df,
                        date == input$selectedDate
                    )
                }
            )

            selectedHldgVals_cashAdj <- reactive({
                di()
                
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

            plot_sort <- F
            plot_textInfo <- 'label'

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
                    max_rtn <- max(
                        h$rtn_anlzed,
                        na.rm = T
                    )
                    min_rtn <- min(
                        h$rtn_anlzed,
                        na.rm = T
                    )
                    zero <- pmax(0 - min_rtn, 0) / (max_rtn - min_rtn)

                    # calculate border colors so that it can be used in genCyberColors
                    h <- h %>%
                        mutate(
                            # bugged for the path bar
                            # wdt_bdr = if_else(
                            #     rtn_anlzed == max_rtn
                            #     | rtn_anlzed == min_rtn,
                            #     6,
                            #     2
                            # ) %>% replace_na(2),
                            color_bdr = genCyberColors_pfmc(rtn_anlzed, max_rtn, min_rtn),
                            text = sprintf(
                                '%sAnnualized return: %.2f%%',
                                case_when(
                                    rtn_anlzed == max_rtn ~ '★ Best Performer<br>',
                                    rtn_anlzed == min_rtn ~ '✖ Worst Performer<br>',
                                    .default = ''
                                ),
                                rtn_anlzed * 100
                            )
                        )

                    h %>%
                        plot_ly(
                            ids = ~id,
                            labels = ~lbl,
                            parents = ~parent,
                            values = ~val,

                            type = input$selectedChartType_hierarchical %>%
                                isolate() %>% 
                                tolower(),
                            branchvalues = 'total', # Values represent the total sum of their children
                            sort = plot_sort, # use custom sort to order it clockwise
                            rotation = 90, # start at 12 o'clock like normal humans do
                            
                            #source = "fundsSunburst", # Changed source name
                            customdata = ~rtn_anlzed, # Pass ID for click events
                            #hoverinfo = 'label+percent entry+value',
                            hovertemplate = paste(
                                "<b>%{label}</b><br>",
                                # for privacy reasons, let's hide this for now
                                #"Value: %{value:$,.0f}<br>",
                                "Pctg of Parent: %{percentParent:.1%}<br>",
                                "Pctg of Entry: %{percentEntry:.1%}<br>",
                                "Anlzed Rtn: %{customdata:.2%}", # Assumes ann_rtn is passed to customdata
                                "<extra></extra>" # Hide the trace info
                            ),
                            text = ~text,
                            textinfo = plot_textInfo,
                            textfont = list(
                                family = "Orbitron, monospace",
                                color = ~color_bdr
                            ),
                            insidetextorientation = 'radial',

                            marker = list(
                                colors = genCyberColors(h),
                                line = list(
                                    color = ~color_bdr,
                                    width = BDR_WDT
                                ),
                                colorbar = list(
                                    title = list(
                                        text = "Annualized<br>Return",  # Use <br> for line breaks
                                        font = list(
                                            family = "Orbitron, monospace",
                                            color = '#00fff2',
                                            size = 12  # Adjust size if needed
                                        )
                                    ),
                                    tickfont = list(
                                        family = "Orbitron, monospace",
                                        color = '#00fff2'
                                    ),
                                    tickformat = ".0%",
                                    #len = 0.8,  # Length of the colorbar
                                    thickness = BDR_WDT,  # Width of the colorbar, match border
                                    outlinewidth = 0,
                                    bordercolor = 'rgba(255, 255, 255, 0.3)'
                                    #bgcolor = 'rgba(0, 0, 0, 0.3)'
                                ),
                                colorscale = list(
                                    list(0, PFMC_RED),  # Red for negative
                                    list(zero, light_red),
                                    list(zero, light_green),
                                    list(1, PFMC_GREEN)   # Cyan for positive
                                ),
                                cmin = min_rtn,
                                cmax = max_rtn,
                                showscale = showColorBar_rv() %>% isolate(),
                                pad = list(
                                    #t = 10, 
                                    b = 0, 
                                    l = 0, 
                                    r = 0
                                )
                            ),
                            # remove internal padding between boxes
                            # 4 levels src ctg fund tkr
                            tiling = list(pad = 0),
                            # not applicable to hierarchical charts
                            #pull = ~pull,

                            opacity = 0.95
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
                                family = "Orbitron, monospace" # Futuristic font
                            ),

                            autosize = T,
                            margin = list(
                                l = 0,
                                r = 0,
                                b = 0,
                                #t = 30,
                                pad = 0
                            )

                            # Add color axis for the border performance scale
                            # coloraxis = list(
                            #     colorscale = list(c(0, CYBER_BASE_COLORS[6]), c(1, CYBER_BASE_COLORS[1])),
                            #     colorbar = list(
                            #         title = "Anlzed Rtn%",
                            #         tickformat = ".2%"
                            #     ),
                            #     cmin = -max_abs_rtn, # Set min/max for the color bar
                            #     cmax = max_abs_rtn,
                            #     showscale = T # Ensure color bar is shown
                            # )
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

                                        // resize
                                        if (window.chartTypeChged) {
                                            Plotly.Plots.resize(document.getElementById('%s')); 
                                            Plotly.Plots.resize(document.getElementById('%s'));
                                            
                                            window.chartTypeChged = false;
                                        }
                                        
                                        // enable inputs
                                        Shiny.setInputValue('%s', true, {priority: 'event'});
                                    });
                                }
                            ",
                            ns("plot_hierarchical"),
                            ns("plot_flat"),
                            ns("enableIpts")
                        ))
                }
            })

            plot_proxy <- plotlyProxy("plot_hierarchical", session)

            observeEvent(
                input$selectedChartType_hierarchical,
                {
                    di()

                    # the idea is that the circular nature of the surburst chart
                    # means that its width is limited by its height
                    # so it can not take the full available width
                    # so we make it only have the screen
                    # but for the other charts we want them to take the full width
                    rmv <- "col-sm-6"
                    add <- "col-sm-12"
                    
                    plot_sort <<- T
                    plot_textInfo <<- 'label+text+percent parent+percent entry'

                    t <- input$selectedChartType_hierarchical

                    if (t == 'Sunburst') {
                        rmv <- "col-sm-12"
                        add <- "col-sm-6"

                        plot_sort <<- F
                        plot_textInfo <<- 'label'
                    }

                    # change col size before change plot
                    # so that new plot uses the new size
                    # Target the column using its ID within the namespace
                    id_col_hierarchical <- paste0("#", ns("col_hierarchical"))
                    id_col_flat <- paste0("#", ns("col_flat"))

                    shinyjs::removeClass(
                        selector = id_col_hierarchical,
                        class = rmv
                    )
                    shinyjs::addClass(
                        selector = id_col_hierarchical,
                        class = add
                    )
                    shinyjs::removeClass(
                        selector = id_col_flat,
                        class = rmv
                    )
                    shinyjs::addClass(
                        selector = id_col_flat,
                        class = add
                    )

                    # Construct the JavaScript call to resize BOTH plots
                    # Run the JavaScript
                    shinyjs::runjs('window.chartTypeChged = true;')

                    plotlyProxyInvoke(
                        plot_proxy, "restyle",
                        list(
                            type = tolower(t),
                            sort = plot_sort,
                            textinfo = plot_textInfo
                        )
                    )
                },
                ignoreInit = T
            )

            observeEvent(
                showColorBar_rv(),
                { plotlyProxyInvoke( plot_proxy, "restyle", list(marker.showscale = showColorBar_rv()) ) },
                ignoreInit = T
            )

            #Render tickers pie chart.
            output$plot_flat <- renderPlotly({
              h <- selectedHldgVals_tkrs()

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
                sfs <- selectedFunds_hldgs()
                fn <- if_else(
                  length(sfs) == 0,
                  'Overall Portfolio',
                  paste(
                    sfs,
                    collapse = " + "
                  )
                )

                h %>%
                  plot_ly(
                    labels = ~name,
                    values = ~val,
                    type = 'pie',
                    hole = 0.6,
                    textinfo = 'label+percent',
                    insidetextorientation = 'radial',
                    marker = list(
                      line = list(
                        color = 'rgba(255, 255, 255, 0.3)',  # White glow effect
                        width = 1
                      ),
                      colors = colorRampPalette(c("#ffffff", "#e6e6ff", "#ccccff", "#b3b3ff"))(nrow(h)) # White to light blue gradient
                    ),
                    hoverinfo = 'label+percent+value',
                    opacity = 0.95,
                    direction = 'clockwise'
                  ) %>%
                  createPieLayout(
                    paste0("Holdings by Ticker (", fn, ")"),
                    list(
                      family = "-apple-system",
                      size = 18,
                      color = "#ffffff"  # White text
                    ),
                    list(color = '#ffffff') # White text for labels
                  )
              }
            })
        }
    ) # End moduleServer
}
