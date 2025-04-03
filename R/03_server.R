# Server Logic
#consts
CYBER_COLORS <- c(
  "#00fff2", "#00b4ff", "#0051ff",   # Blue spectrum
  "#7700ff", "#b300ff", "#ff00f7",   # Purple spectrum
  "#1bffad", "#45ff70", "#c9ff33"    # Green spectrum
)
#end consts

app_server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

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
    d <- selectedDate()

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
  selectedHldgVals_fnds <- reactive({ getHldgVals(hldgVals_fnds) })

  selectedHldgVals_tkrs <- reactive({
    # Tickers breakdown: if a fund is selected, use that trades subset; else overall
    sfs <- selectedFunds_hldgs()

    #dynamic calculation for the selected funds
    # check if there is cached data for the selected funds combinations
    # if not, calculate it and store it in the cache
    # Create cache key by sorting and concatenating fund names
    cache_key <- if_else(
      length(sfs) == 0,
      "op",
      paste(
        sort(sfs),
        collapse = "|"
      )
    )
    #cached_vals <- cache_hldgVals_tkrs()[[cache_key]]

    if (is.null(cached_vals)) {
      trades_sfs <- trades %>%
        filter(fnd %in% sfs)

      #if cash is a selected fund, then we use the overall portfolio cash value
      #otherwise we calculate the cash value for the selected funds
      if("Cash" %in% sfs) {
        #cached_vals <- calcHldgVals_tkrs(TRADES = trades_sfs)
      } else {
        cc <- calcCumCash(trades_sfs)
        #cached_vals <- calcHldgVals_tkrs(calcSttgCash(cc) + cc, trades_sfs)
      }

      # Update cache
      # something something about atomic and race conditions
      cache_hldgVals_tkrs() %>%
        within(
          {
            .[[cache_key]] <- cached_vals
          }
        ) %>%
        cache_hldgVals_tkrs()
    }

    # Calculate tkr values for the selected date
    getHldgVals(cached_vals)
  })

  # Modify fundsPie and tickersPie outputs for a modern/futuristic appearance
  output$fundsPie <- renderPlotly({
    disableIpts()

    # Ensure inputs are re-enabled
    on.exit({ enableIpts() })

    # Calculate fund values for the selected date
    f <- selectedHldgVals_fnds()

    if(nrow(f) == 0) {
      # Return empty plot if no data
      plot_ly() %>%
        layout(
          title = "No holdings data available",
          paper_bgcolor = '#222',
          plot_bgcolor = '#222',
          font = list(color = '#eee')
        )
    } else {
      # Add pull column only when we have data
      sfs <- selectedFunds_hldgs()

      f %>%
        mutate(pull = ifelse(name %in% sfs, 0.1, 0)) %>%
        plot_ly(
          labels = ~name,
          values = ~val,
          type = 'pie',
          source = "fundsPie",
          customdata = ~name,           # for click events
          pull = ~pull,                # pull out selected slice
          hole = 0.6,
          textinfo = 'label+percent',
          insidetextorientation = 'radial',
          marker = list(
            line = list(
              color = 'rgba(0, 255, 242, 0.3)',  # Cyan glow effect
              width = 1
            ),
            colors = CYBER_COLORS
          ),
          hoverinfo = 'label+percent+value',
          opacity = 0.9,
          direction = 'clockwise',
          key = ~name # Add unique key for each slice
        ) %>%
        htmlwidgets::onRender("
          function(el) {
            el.on('plotly_click', function(d) {
              console.log('Click event:', d);
              Shiny.setInputValue('pie_click', {
                customdata: d.points[0].customdata,
                curveNumber: d.points[0].curveNumber,
                timestamp: new Date()
              });
            });
          }
        ") %>%
        createPieLayout(
          "Holdings by Fund",
          FONT = list(
            color = '#00fff2',  # Cyan text
            family = "Arial"
          )
        )
    }
  })

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
  output$tickersPie <- renderPlotly({
    disableIpts()

    # Ensure inputs are re-enabled
    on.exit({ enableIpts() })

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

  ########################################################## Returns

  selectedRtns_raw <- reactive({
    req(input$selected_chart == 'Returns')

    # we will try to always trigger slider update and use slider update to trigger rebase
    # to avoid double update on the plot
    rtns_df %>%
      filter(istmt %in% c('Overall Portfolio', NAMES_BMS, input$selectedFunds_rtns, input$selectedTkrs_rtns)) %>%
      mutate(
        rtn = if (input$inclCash) cmltvRtn_inclCash
        else cmltvRtn_xcluCash
      ) %>%
      select(istmt, istmt_legend, type, date, rtn)
  })

  # normal variable as no reaction is needed
  # if using reactive val, it would trigger the related funciton again
  # when it's val is updated by the function
  minDate_rtns_prv <- start_date
  needUd_rtns <- reactiveVal(NULL)

  observeEvent(
    selectedRtns_raw(),
    {
      # --- Update the NEW returns slider ---
      min_new <- selectedRtns_raw() %>%
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
      if (min_new == minDate_rtns_prv) (
        Sys.time() %>%
          needUd_rtns() %>%
          return()
      )

      updateSliderInput(
        session, "selectedDate_rtns", # Target the correct slider
        min = min_new                 # Set the new minimum
      )

      crt <- input$selectedDate_rtns # Current selected value

      if (min_new <= crt) {
        # if crt = old min, at default position
        # and new min < old min, if new min is > old min, then the selection would automatically be pushed up
        if (
          crt == minDate_rtns_prv
          & min_new < crt
        ) {
          updateSliderInput(
            session, "selectedDate_rtns", # Target the correct slider
            value = min_new               # Set the calculated value
          )
        } else Sys.time() %>% needUd_rtns()
      }

      minDate_rtns_prv <<- min_new
  })

  # Reactive: rebase all selected return series to a common base date.
  selectedRtns_rebased <- eventReactive(
    list(needUd_rtns(), input$selectedDate_rtns),
    {
      # change in data frame does not trigger re-run
      # only changes in date can trigger re-run
      # and they we try to force change in date every time.
      df <- selectedRtns_raw()

      #common base date
      cbd <- input$selectedDate_rtns

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

  output$rtnsPlot <- renderPlotly({
    disableIpts()

    # Ensure inputs are re-enabled
    on.exit({ enableIpts() })

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
    x_sd <- isolate(input$selectedDate_rtns)

    # only need custom x & y range if there is rebase
    if (x_sd > start_date) {
      x_ed <- END_DATE

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
          range = range_x
        ),
        yaxis = list(
          title = "Cumulative Return",
          tickformat = ".2%",
          gridcolor = '#444',
          color = '#eee',
          range = range_y # Apply the calculated default Y range
        ),
        legend = list(
          title = list(text = "Order. Annualized Return% Instrument"),
          font = list(color = '#eee')
          #traceorder = "normal"
        ),
        # Set dark theme colors
        plot_bgcolor = '#222',
        paper_bgcolor = '#222',
        font = list(color = '#eee')
      )
  })

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
    ignoreNULL = T,
    ignoreInit = T # Prevent running on startup
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

      updateCheckboxGroupInput(
        session, "selectedTkrs_rtns",
        selected = if (length(input$selectedTkrs_rtns) < length(t)) t
        else character(0)
      )
    }
  )

  ################################################################ Chart Type

  output$mainContent <- renderUI({
    if (input$selected_chart == "Returns") {
      plotlyOutput(
        "rtnsPlot",
        height = "100vh"
      )
    } else {
      fluidRow(
        column(
          6,
          plotlyOutput(
            "fundsPie",
            height = "100vh",
            width = "100%"
          )
        ),
        column(
          6,
          plotlyOutput(
            "tickersPie",
            height = "100vh",
            width = "100%"
          )
        )
      )
    }
  })
}