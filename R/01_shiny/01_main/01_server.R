# Server Logic
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

  # --- Call the Data Loader Module ---
  end_date <- dataInitServer("dataInit")
  # This returns a list of vars: data$status, data$rtns_df, etc.

  # init settings after data init, but before plotting
  # Update showLegend based on orientation when app starts
  observeEvent(
    input$isLandscape,
    {
      updateCheckboxInput(
        session, "showLegend",
        value = input$isLandscape
      )

      updateCheckboxInput(
        session, "showRangeSldr",
        value = !input$isLandscape
      )
    }
  )

  # Trigger initial orientation check after everything is set up
  session$sendCustomMessage("check-orientation", list())

  # update selection settings once inited
  slctGrps <- list(
    list(
      suffix = "Ctgs", 
      choices = unlist(openCtgGrps)
    ),
    list(
      suffix = "Funds", 
      choices = unlist(openFundGrps)
    ),
    list(
      suffix = "Uas", 
      choices = unlist(openUaGrps)
    )
  )

  observeEvent(
      input$inclClosed,
      {
          ## Update the variables first thing
          if (input$inclClosed) {
            # sources
            avlbSrcs <- uniqueSrcs_sorted # Use all sources

            cgs <- uniqueCtgGrps # Use all categories
            fgs <- uniqueFundGrps # Use all funds
            uags <- uniqueUaGrps # Use all tickers
          } else {
            avlbSrcs <- openSrcs_sorted   # Use only open sources

            cgs <- openCtgGrps # Use only open categories
            fgs <- openFundGrps # Use only open funds
            uags <- openUaGrps # Use only open tickers
          }

          slctGrps[[1]]$grps <<- cgs
          slctGrps[[2]]$grps <<- fgs
          slctGrps[[3]]$grps <<- uags

          slctGrps[[1]]$choices <<- unlist(cgs)
          slctGrps[[2]]$choices <<- unlist(fgs)
          slctGrps[[3]]$choices <<- unlist(uags)

          updateCheckboxGroupInput(
              session, "selectedSrcs_rtns",
              choices = avlbSrcs,
              # Preserve current selection if items still exist in the new choices
              selected = intersect(input$selectedSrcs_rtns, avlbSrcs)
          )

          for (g in slctGrps) {
            id <- paste0("selected", g$suffix, "_rtns")

            updateSelectizeInput(
                session, id,
                choices = g$grps,
                # Preserve current selection if items still exist in the new choices
                selected = intersect(input[[id]], g$choices)
                # server = TRUE # Consider if list becomes extremely large
            )
          }
      },
      # run once data is inited
      # because at UI level the data is not ready
      #ignoreInit = T # Prevent running on startup
  )

  # Create observers using a loop (lapply)
  for (idx in seq_along(slctGrps)) {
    local({
      # create local copies
      i_crt <- idx
      s_crt <- slctGrps[[idx]]$suffix
      input_id <- paste0("selected", s_crt, "_rtns")

      observeEvent(
        # the input id is generated at execution, not at definition,
        # and the execution time and definition time are different
        # so it will point to the last idx and bind to the last button
        # without the local copy
        input[[paste0("tglAll", s_crt, "_btn")]],
        {
          a <- slctGrps[[i_crt]]$choices

          updateSelectizeInput(
              session, input_id, # Use the input_id to avoid code duplication
              selected = if (length(input[[input_id]]) < length(a)) a
              else character(0)
          )
        }
      )
    })
  }

  disableIpts <- function() {
    # need asis because when passed to module, the module will add namespace for the module
    # and we don't want that because these are not module inputs
    disable(
      "selected_chart",
      asis = T
    )


    disable(
      'selectedFunds_hldgs',
      asis = T
    )

    disable(
      "selectedSrcs_rtns",
      asis = T
    )

    disable(
      'tglAllCtgs_btn',
      asis = T
    )
    disable(
      "selectedCtgs_rtns",
      asis = T
    )

    disable(
      'tglAllFunds_btn',
      asis = T
    )
    disable(
      "selectedFunds_rtns",
      asis = T
    )

    disable(
      'tglAllUas_btn',
      asis = T
    )
    disable(
      "selectedUas_rtns",
      asis = T
    )

    disable(
      "inclCash",
      asis = T
    )
    disable(
      'inclClosed',
      asis = T
    )
    disable(
      'showLegend',
      asis = T
    )
  }

  enableIpts <- function() {
    enable(
      "selected_chart",
      asis = T
    )

    enable(
      "selectedFunds_hldgs",
      asis = T
    )

    enable(
      "selectedSrcs_rtns",
      asis = T
    )

    enable(
      'tglAllCtgs_btn',
      asis = T
    )
    enable(
      "selectedCtgs_rtns",
      asis = T
    )

    enable(
      'tglAllFunds_btn',
      asis = T
    )
    enable(
      'selectedFunds_rtns',
      asis = T
    )

    enable(
      'tglAllUas_btn',
      asis = T
    )
    enable(
      "selectedUas_rtns",
      asis = T
    )

    enable(
      "inclCash",
      asis = T
    )
    enable(
      "inclClosed",
      asis = T
    )
    enable(
      'showLegend',
      asis = T
    )
  }

  rtnsServer(
    'rtns', end_date, reactive(input$selected_chart), reactive(input$selectedSrcs_rtns), reactive(input$selectedCtgs_rtns),
    reactive(input$selectedFunds_rtns), reactive(input$selectedUas_rtns), reactive(input$inclCash), reactive(input$showLegend),
    reactive(input$showRangeSldr), disableIpts, enableIpts
  )

  hldgsServer(
    'hldgs', end_date, reactive(input$selected_chart), reactive(input$selectedFunds_hldgs), reactive(input$inclCash),
    disableIpts, enableIpts
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

  hidePageSpinner()
}