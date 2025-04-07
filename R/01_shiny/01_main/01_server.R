# Server Logic
#consts
CYBER_COLORS <- c(
  "#00fff2", "#00b4ff", "#0051ff",   # Blue spectrum
  "#7700ff", "#b300ff", "#ff00f7",   # Purple spectrum
  "#1bffad", "#45ff70", "#c9ff33"    # Green spectrum
)
#end consts

options(
  page.spinner.type = 3,
  page.spinner.color = "#00fff2", # Your cyan color
  page.spinner.color.background = 'rgba(50, 50, 50, 0.2)',
  page.spinner.size = 1.5,
  page.spinner.background = "#222"
)

app_server <- function(input, output, session) {
  message("--- app_server function started ---")

  session$onSessionEnded(function() {
    message("--- Session ending, cleaning up ---")
    stopApp()
  })

  # spinner
  showPageSpinner()

  # sprintf("Available cores: %d", parallelly::availableCores(constraints = "cgroups2.cpu.max")) %>% message()
  
  # # Use 1 worker on shinyapps.io to avoid resource conflicts
  # tryCatch({
  #   future::plan(
  #     future::multiprocess,
  #     workers = 1,
  #     gc = T,
  #     earlySignal = T
  #   )

  #   message("Future plan configured successfully")
  # }, error = function(e) {
  #   message("Error configuring future plan: ", e$message)
  # })

  # --- Configure progressr handler for Shiny ---
  #progressr::handler_shiny(session = session) %>% progressr::handlers()
  # Ensure this runs once per session start within the module server

  # Update showLegend based on orientation when app starts
  # need this before data init as it runs once on window load
  observeEvent(
    input$showLegend_orientation, 
    { 
      updateCheckboxInput(
        session, "showLegend", 
        value = input$showLegend_orientation
      ) 
    }
  )

  # --- Call the Data Loader Module ---
  end_date_rv <- dataInitServer("dataInit")
  # This returns a list of vars: data$status, data$rtns_df, etc.

  rtnsServer(
    'rtns', end_date_rv, input$selected_chart, reactive(input$selectedFunds_rtns), reactive(input$selectedTkrs_rtns), reactive(input$inclCash), 
    reactive(input$showLegend), disableIpts, enableIpts
  )

  # Reactive val for filtering tickers breakdown by a fund click (NULL = overall)
  # Replace single selection with multiple selections
  selectedFunds_hldgs <- reactiveVal(c())

  #req specifies that the input must be true or has value
  # however, selected date can not be empty or false, so there is no point for this
  # this is left as an example
  # selectedDate_hldgs <- reactive({
  #   req(input$selectedDate_hldgs)

  #   input$selectedDate_hldgs
  # })

  # Add cache for holding values with pre-calculated data
  # Store pre-calculated overall view with empty key
  #cache_hldgVals_tkrs <- reactiveVal( list(op = calcHldgVals_tkrs()) )

  disableIpts <- function() {
    disable("selected_chart")
    disable("selectedFunds_rtns")
    disable('selectedFunds_hldgs')
    disable("inclCash")
    disable('inclClosed')
    disable('selecteDate_rtns')
    disable('selecteDate_hldgs')
  }

  enableIpts <- function() {
    enable("selected_chart")
    enable("selectedFunds_rtns")
    enable('selectedFunds_hldgs')
    enable("inclCash")
    enable("inclClosed")
    enable('selecteDate_rtns')
    enable("selecteDate_hldgs")
  }

  getHldgVals <- function(HLDG_VALS) {
    # filter values for the selected date
    d <- input$selectedDate_hldgs

    vals <- lapply(
      names(HLDG_VALS),
      function(n) {
        if (!input$inclCash && n == "Cash") return(NULL)

        v <- as.numeric(HLDG_VALS[[n]][d])

        # don't show zero or NA values
        if(is.na(v) || v == 0) return(NULL)

        data.frame(name = n, val = v)
      }
    )

    # Remove NULL entries and combine results
    df <- do.call(rbind, Filter(Negate(is.null), vals))

    if (is.null(df)) return(data.frame())

    df
  }

  # needed for reactive context which caches the results
  #selectedHldgVals_fnds <- reactive({ getHldgVals(hldgVals_fnds) })

  selectedHldgVals_tkrs <- reactive({
    # Tickers breakdown: if a fund is selected, use that trades subset; else overall
    #sfs <- selectedFunds_hldgs()

    #dynamic calculation for the selected funds
    # check if there is cached data for the selected funds combinations
    # if not, calculate it and store it in the cache
    # Create cache key by sorting and concatenating fund names
    # cache_key <- if_else(
    #   length(sfs) == 0,
    #   "op",
    #   paste(
    #     sort(sfs),
    #     collapse = "|"
    #   )
    # )
    #cached_vals <- cache_hldgVals_tkrs()[[cache_key]]

    # if (is.null(cached_vals)) {
    #   trades_sfs <- trades %>%
    #     filter(fnd %in% sfs)

    #   #if cash is a selected fund, then we use the overall portfolio cash value
    #   #otherwise we calculate the cash value for the selected funds
    #   if("Cash" %in% sfs) {
    #     #cached_vals <- calcHldgVals_tkrs(TRADES = trades_sfs)
    #   } else {
    #     cc <- calcCumCash(trades_sfs)
    #     #cached_vals <- calcHldgVals_tkrs(calcSttgCash(cc) + cc, trades_sfs)
    #   }

    #   # Update cache
    #   # something something about atomic and race conditions
    #   cache_hldgVals_tkrs() %>%
    #     within(
    #       {
    #         .[[cache_key]] <- cached_vals
    #       }
    #     ) %>%
    #     cache_hldgVals_tkrs()
    # }

    # # Calculate tkr values for the selected date
    # getHldgVals(cached_vals)
  })

  # Modify fundsPie and tickersPie outputs for a modern/futuristic appearance
  # output$fundsPie <- renderPlotly({
  #   disableIpts()

  #   # Ensure inputs are re-enabled
  #   on.exit({ enableIpts() })

  #   # Calculate fund values for the selected date
  #   f <- selectedHldgVals_fnds()

  #   if(nrow(f) == 0) {
  #     # Return empty plot if no data
  #     plot_ly() %>%
  #       layout(
  #         title = "No holdings data available",
  #         paper_bgcolor = '#222',
  #         plot_bgcolor = '#222',
  #         font = list(color = '#eee')
  #       )
  #   } else {
  #     # Add pull column only when we have data
  #     sfs <- selectedFunds_hldgs()

  #     f %>%
  #       mutate(pull = ifelse(name %in% sfs, 0.1, 0)) %>%
  #       plot_ly(
  #         labels = ~name,
  #         values = ~val,
  #         type = 'pie',
  #         source = "fundsPie",
  #         customdata = ~name,           # for click events
  #         pull = ~pull,                # pull out selected slice
  #         hole = 0.6,
  #         textinfo = 'label+percent',
  #         insidetextorientation = 'radial',
  #         marker = list(
  #           line = list(
  #             color = 'rgba(0, 255, 242, 0.3)',  # Cyan glow effect
  #             width = 1
  #           ),
  #           colors = CYBER_COLORS
  #         ),
  #         hoverinfo = 'label+percent+value',
  #         opacity = 0.9,
  #         direction = 'clockwise',
  #         key = ~name # Add unique key for each slice
  #       ) %>%
  #       htmlwidgets::onRender("
  #         function(el) {
  #           el.on('plotly_click', function(d) {
  #             console.log('Click event:', d);
  #             Shiny.setInputValue('pie_click', {
  #               customdata: d.points[0].customdata,
  #               curveNumber: d.points[0].curveNumber,
  #               timestamp: new Date()
  #             });
  #           });
  #         }
  #       ") %>%
  #       createPieLayout(
  #         "Holdings by Fund",
  #         FONT = list(
  #           color = '#00fff2',  # Cyan text
  #           family = "Arial"
  #         )
  #       )
  #   }
  # })

  # Replace plotly_click observer with pie_click observer
  observeEvent(
    input$pie_click,
    {isolate({
      clickedFund <- as.character(input$pie_click$customdata)# Ensure consistent type for comparison

      if(!is.null(clickedFund)) {
        sfs <- selectedFunds_hldgs()

        if (clickedFund %in% sfs) {
          s <- setdiff(sfs, clickedFund)
        } else {
          s <- c(sfs, clickedFund)
        }

        selectedFunds_hldgs(s)
      }
    })}
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

  availableFunds <- openFunds_sorted
  availableTkrs <- unlist(openTkrGrps)

  observeEvent(
      input$inclClosed,
      {
          ## Update the variables first thing
          # Update Fund Choices based on checkbox
          availableFunds <<- if (input$inclClosed) uniqueFunds_sorted # Use all funds
          else openFunds_sorted   # Use only open funds

          # Update Ticker Choices based on checkbox
          t <- if (input$inclClosed) uniqueTkrGrps # Use all tickers, grouped
          else openTkrGrps   # Use only open tickers, grouped
          availableTkrs <<- unlist(t)

          updateCheckboxGroupInput(
              session, "selectedFunds_rtns",
              choices = availableFunds,
              # Preserve current selection if items still exist in the new choices
              selected = intersect(input$selectedFunds_rtns, availableFunds)
          )

          updateSelectizeInput(
              session, "selectedTkrs_rtns",
              choices = t,
              # Preserve current selection if items still exist in the new choices
              selected = intersect(input$selectedTkrs_rtns, availableTkrs)
              # server = TRUE # Consider if list becomes extremely large
          )
      },
      # run once data is inited
      # because at UI level the data is not ready
      #ignoreInit = T # Prevent running on startup
  )

  observeEvent(
      input$tglAllFunds_btn,
      {
          f <- availableFunds

          updateCheckboxGroupInput(
              session, "selectedFunds_rtns",
              selected = if (length(input$selectedFunds_rtns) < length(f)) f
              else character(0)
          )
      }
  )

  observeEvent(
      input$tglAllTkrs_btn,
      {
          t <- availableTkrs

          updateSelectizeInput(
              session, "selectedTkrs_rtns",
              selected = if (length(input$selectedTkrs_rtns) < length(t)) t
              else character(0)
          )
      }
  )

  hidePageSpinner()
}