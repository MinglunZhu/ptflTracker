# UI Function for the Returns Chart Module
rtnsUI_sldr <- function(id) {
    ns <- NS(id) # Namespace function

    tags$li(
        sliderInput(
            ns("selectedDate"), "Select Rebase Date:",
            min = start_date,
            max = RUN_DATE,
            # default value is required
            value = start_date,
            timeFormat = "%Y-%m-%d", 
            width = "100%",
            animate = animationOptions(
                interval = 1500,   # milliseconds between frames
                loop = T      # continue playing in a loop
            )
        ) # end sliderInput
    )
}

rtnsUI_plot <- function(id) {
    ns <- NS(id) # Namespace function
    
    plotlyOutput(
        ns("plot"),
        height = "100vh"
    )
}

# Server Function for the Returns Chart Module
rtnsServer <- function(
    id, end_date, selectedChart_rv, selectedSrcs_rv, selectedCtgs_rv, selectedFunds_rv, selectedUas_rv, inclCash_rv, showLegend_rv,
    showRangeSldr_rv, disableIpts, enableIpts
) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session$ns

            message("Updating slider max to: ", end_date)
            updateSliderInput(
                session, "selectedDate",
                max = end_date
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

            selectedRtns_raw <- eventReactive(
                list(
                    selectedSrcs_rv(), selectedCtgs_rv(), selectedFunds_rv(), selectedUas_rv()
                ),
                {
                    if (isFirst_chart) isFirst_chart <<- F
                    else req(selectedChart_rv() == 'Returns')

                    di()

                    # we will try to always trigger slider update and use slider update to trigger rebase
                    # to avoid double update on the plot
                    rtns_df %>%
                        filter(
                            istmt %in% c(
                                'Overall Portfolio', NAMES_BMS, selectedSrcs_rv(), selectedCtgs_rv(), selectedFunds_rv(),
                                selectedUas_rv()
                            )
                    ) %>%
                    select(istmt, istmt_legend, type, date, cmltvRtn_inclCash, cmltvRtn_xcluCash)
                }
            )

            selectedRtns_cashAdjed <- reactive({
                selectedRtns_raw() %>%
                    mutate(
                        rtn = if (inclCash_rv()) cmltvRtn_inclCash
                        else cmltvRtn_xcluCash
                    ) %>%
                    select(-c(cmltvRtn_inclCash, cmltvRtn_xcluCash))
            })

            # normal variable as no reaction is needed
            # if using reactive val, it would trigger the related funciton again
            # when it's val is updated by the function
            minDate_rtns_prv <- start_date
            needUd_rtns <- reactiveVal(NULL)

            observeEvent(
                #list(selectedRtns_cashAdjed(), needUd_minDate()),
                selectedRtns_cashAdjed(),
                {
                    # --- Update the NEW returns slider ---
                    min_new <- selectedRtns_cashAdjed() %>%
                        group_by(istmt) %>%
                        filter(rtn != 0) %>%
                        summarise(
                            startDate = min(
                                date,
                                na.rm = T
                            )
                        ) %>%
                        pull(startDate) %>%
                        max() - 1

                    # can not dynamically change animation options

                    # force date update to trigger rebase
                    # because rebase no longer triggered by df update
                    if (min_new == minDate_rtns_prv) {
                        Sys.time() %>% needUd_rtns()
                        return()
                    }

                    updateSliderInput(
                        session, "selectedDate", # Target the correct slider
                        min = min_new                 # Set the new minimum
                    )

                    crt <- input$selectedDate

                    if (min_new <= crt) {
                        # if crt = old min, at default position
                        # and new min < old min, if new min is > old min, then the selection would automatically be pushed up
                        if (
                            crt == minDate_rtns_prv
                            & min_new < crt
                        ) {
                            updateSliderInput(
                                session, "selectedDate", # Target the correct slider
                                value = min_new               # Set the calculated value
                            )
                        } else Sys.time() %>% needUd_rtns()
                    }

                    minDate_rtns_prv <<- min_new
                }
                # no ignore init because when this code is run
                # selected rtns raw is already done
                # and we need to init it once as it won't get triggered by selectedRtns_raw as it's already run
            )

            # there seems to be a first selectedDate set to start_date
            # possibly from slider initialization
            # we want to ignore that from triggering rebase
            isFirst_date <- T

            # Reactive: rebase all selected return series to a common base date.
            selectedRtns_rebased <- eventReactive(
                list(needUd_rtns(), input$selectedDate),
                {
                    if (isFirst_date) {
                        isFirst_date <<- F
                        req(F)
                    }

                    di()

                    # change in data frame does not trigger re-run
                    # only changes in date can trigger re-run
                    # and they we try to force change in date every time.
                    df <- selectedRtns_cashAdjed()

                    #common base date
                    cbd <- input$selectedDate

                    if (cbd <= start_date) return(df)

                    df %>%
                        #rtn before rebased date will be negative and meaningless
                        # we still plot them, but won't be shown by default
                        #filter(date >= cbd) %>%
                        group_by(istmt_legend) %>%
                        mutate(
                            baseVal = rtn[date == cbd],
                            rtn = (rtn + 1) / (baseVal + 1) - 1
                        ) %>%
                        select(-baseVal)
                }
            )

            # Observer to handle plot status changes
            observe({
                req(input$enableIpts)

                ei()
            })

            output$plot <- renderPlotly({
                df <- selectedRtns_rebased()

                # Check if no series available
                if (nrow(df) == 0) {
                    plotly_empty() %>%
                        layout(
                            title = "No return data available for selected instruments/period",
                            plot_bgcolor = '#222',
                            paper_bgcolor = '#222',
                            font = list(color = '#eee')
                        ) %>%
                        return()
                }

                range_x <- NULL
                range_y <- NULL # Default to NULL (autoscale)
                x_sd <- isolate(input$selectedDate)

                # only need custom x & y range if there is rebase
                if (x_sd > start_date) {
                    # it does matter if we isolate because end date would only be set
                    # at initialization.
                    x_ed <- end_date

                    range_x <- c(x_sd, x_ed)

                    xRange_df <- df %>%
                        filter(
                            date >= x_sd
                            & date <= x_ed
                        )

                    if (nrow(xRange_df) > 0) {
                        min_y <- min(
                            xRange_df$rtn,
                            na.rm = T
                        )
                        max_y <- max(
                            xRange_df$rtn,
                            na.rm = T
                        )

                        # Check if min/max are valid numbers
                        if (is.finite(min_y) && is.finite(max_y)) {
                            # Calculate padding (e.g., 5% of the range)
                            # Handle case where min_y == max_y
                            padding <- if (max_y == min_y) abs(max_y * 0.05) + 0.01 # Add a small absolute padding if range is zero
                            else 0

                            # Calculate final yaxis range
                            range_y <- c(min_y - padding, max_y + padding)
                        }
                    }
                # If yaxis_range is still NULL (e.g., no valid data in view), plotly will autoscale Y
                }

                df %>%
                    plot_ly(
                        x = ~date,
                        y = ~rtn,
                        color = ~istmt_legend,
                        linetype = ~type,

                        #legendrank = ~rank,
                        text = ~paste0("<b>", istmt, "</b>"), # Bold name
                        #name = ~istmt_nbred,
                        hoverinfo = 'x+y+text', # Display x, y, and the content of 'text'

                        type = 'scatter',
                        mode = 'lines',

                        colors = hue_pal(
                            l = 75
                            # c = 100 # Chroma (saturation). Default is 100. Can adjust if needed.
                        )(
                            df$istmt_legend %>%
                                unique() %>%
                                length()
                        )
                    ) %>%
                    layout(
                        title = "Cumulative Daily USD Returns",
                        xaxis = list(
                            title = "Date",
                            gridcolor = '#444',
                            color = '#eee',
                            range = range_x,
                            rangeslider = list(visible = showRangeSldr_rv() %>% isolate() %||% T),
                            rangeselector = list(
                                visible = showRangeSldr_rv() %>% isolate() %||% T, # Set initial state
                                buttons = list(
                                    list(
                                        step = "month",
                                        count = 1, 
                                        label = "1m", 
                                        stepmode = "backward"
                                    ),
                                    list(
                                        step = "month",
                                        count = 3, 
                                        label = "3m", 
                                        stepmode = "backward"
                                    ),
                                    list(
                                        step = "month",
                                        count = 6, 
                                        label = "6m", 
                                        stepmode = "backward"
                                    ),
                                    list(
                                        step = "year",
                                        count = 1, 
                                        label = "YTD", 
                                        stepmode = "todate"
                                    ),
                                    list(
                                        step = "year",
                                        count = 1, 
                                        label = "1y", 
                                        stepmode = "backward"
                                    ),
                                    list(
                                        step = "year",
                                        count = 3, 
                                        label = "3y", 
                                        stepmode = "backward"
                                    ),
                                    list(
                                        step = "year",
                                        count = 5, 
                                        label = "5y", 
                                        stepmode = "backward"
                                    ),
                                    list(
                                        step = "year",
                                        count = 10, 
                                        label = "10y", 
                                        stepmode = "backward"
                                    ),
                                    list(
                                        step = "all",
                                        label = "All"
                                    )
                                ),
                                bgcolor = "#222",
                                font = list(color = '#eee')
                            )
                        ),
                        yaxis = list(
                            title = "Cumulative Return",
                            tickformat = ".2%",
                            gridcolor = '#444',
                            color = '#eee',
                            range = range_y # Apply the calculated default Y range
                        ),
                        legend = list(
                            title = list(text = "Order. | Annualized Return (Xclu Cash)%<br> | Instrument"),
                            font = list(color = '#eee')
                            #, x = 1.02, # Position slightly right of plot area
                            #xanchor = 'left' # Anchor legend's left edge to position x
                            #traceorder = "normal"
                        ),
                        showlegend = showLegend_rv() %>% isolate() %||% T, # Set initial state
                        # Set dark theme colors
                        plot_bgcolor = '#222',
                        paper_bgcolor = '#222',
                        font = list(color = '#eee')
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
            })

            plot_proxy <- plotlyProxy("plot", session)

            observeEvent(
                showLegend_rv(), 
                { plotlyProxyInvoke( plot_proxy, "relayout", list(showlegend = showLegend_rv()) ) },
                ignoreInit = T
            )

            observeEvent(
                showRangeSldr_rv(), 
                { 
                    plotlyProxyInvoke( 
                        plot_proxy, "relayout", 
                        list(
                            "xaxis.rangeslider.visible" = showRangeSldr_rv(),
                            "xaxis.rangeselector.visible" = showRangeSldr_rv()
                        ) 
                    ) 
                },
                ignoreInit = T
            )
        }
    ) # End moduleServer
}