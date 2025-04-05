# UI Function for the Returns Chart Module
returnsChartUI <- function(id, start_date, END_DATE) {
  ns <- NS(id) # Namespace function
  
  tagList(
    # Slider specific to the returns chart
    sliderInput(
      ns("selectedDate_rtns"), # Namespaced ID
      "Select Rebase Date:",
      min = start_date,
      max = END_DATE,
      value = start_date,
      timeFormat = "%Y-%m-%d", 
      width = "100%",
      animate = animationOptions(interval = 1500, loop = TRUE)
    ),
    # Plot output
    plotlyOutput(ns("rtnsPlot"), height = "calc(100vh - 100px)") # Adjust height if needed
  )
}

# Server Function for the Returns Chart Module
returnsChartServer <- function(id, data_status, rtns_data, 
                               selected_funds, selected_tickers, inclCash, show_legend,
                               odr, NAMES_BMS, start_date, END_DATE) {
                                 
  moduleServer(id, function(input, output, session) {
    
    # --- Reactives moved from main server ---
    
    selectedRtns_raw <- reactive({
      req(data_status() == "Ready") # Wait for data
      current_rtns_df <- rtns_data()
      req(current_rtns_df)
      
      # Use reactive arguments passed to module
      selected_instruments <- c('Overall Portfolio', NAMES_BMS, selected_funds(), selected_tickers())
      
      current_rtns_df %>%
        filter(istmt %in% selected_instruments) %>%
        mutate(
          rtn = if (inclCash()) cmltvRtn_inclCash else cmltvRtn_xcluCash
        ) %>%
        # Keep original istmt for matching, add legend version later
        select(istmt, istmt_legend, type, date, rtn) 
    })

    reactive_cmmBaseDate <- reactive({
      df_raw <- selectedRtns_raw()
      req(df_raw) 

      valid_series <- df_raw %>% filter(!is.na(rtn) & rtn != 0)
      
      calculated_base_date <- if(nrow(valid_series) == 0) {
                                start_date 
                              } else {
                                valid_series %>%
                                  group_by(istmt) %>% # Group by original istmt
                                  summarise(startDate = min(date, na.rm = TRUE), .groups = 'drop') %>%
                                  pull(startDate) %>%
                                  max(na.rm = TRUE) 
                              }

      if (!is.finite(calculated_base_date)) {
        calculated_base_date <- start_date 
      }
      
      # Optional adjustment (e.g., -1 day) - ensure consistency
      # calculated_base_date <- max(calculated_base_date - 1, start_date) 

      return(calculated_base_date)
    })

    # Use standard variable for previous min state within the module instance
    minDate_rtns_prv <- start_date 

    observe({
      # This observer reacts to changes in the calculated base date
      new_min <- reactive_cmmBaseDate() 
      
      if (!is.null(new_min) && inherits(new_min, "Date") && is.finite(new_min)) {
         old_min <- minDate_rtns_prv # Read standard variable
         
         if (!identical(new_min, old_min)) { 
             current_value <- isolate(input$selectedDate_rtns) # Read namespaced input
             if (!inherits(old_min, "Date") || !is.finite(old_min)) { old_min <- start_date } 
             if (!inherits(current_value, "Date") || !is.finite(current_value)) { current_value <- new_min } 

             if (new_min < current_value) {
               new_value <- if (current_value == old_min) new_min else current_value
             } else {
               new_value <- new_min
             }
             new_value <- min(max(new_value, new_min, na.rm = TRUE), END_DATE, na.rm = TRUE)

             updateSliderInput(session, "selectedDate_rtns", # Use namespaced ID
                               min = new_min,    
                               value = new_value)  
                            
             minDate_rtns_prv <<- new_min # Update standard variable in module scope
         }
      } 
    }) 

    selectedRtns_rebased <- reactive({
      df_raw <- selectedRtns_raw() 
      cbd <- reactive_cmmBaseDate() 
      req(df_raw, cbd) 

      if (cbd > start_date) { 
        df_rebased <- df_raw %>%
          filter(date >= cbd) %>% 
          group_by(istmt) %>% # Group by original istmt
          mutate(
            baseVal = rtn[date == cbd][1], 
            baseVal = if_else(is.na(baseVal), 0, baseVal),
            rtn_rebased = if_else(is.na(rtn), NA_real_, (1 + rtn) / (1 + baseVal) - 1)
          ) %>%
          ungroup() %>%
          select(istmt, istmt_legend, type, date, rtn = rtn_rebased) # Keep legend column
      } else {
        df_rebased <- df_raw %>% select(istmt, istmt_legend, type, date, rtn) 
      }
      
      # Apply prefixing using the legend column
      # Ensure odr is passed correctly or recalculated if needed
      df_final <- df_rebased %>% 
          mutate(
            # Use the pre-calculated legend column for plotting/coloring
            istmt_plot = istmt_legend 
          ) 

      return(df_final) 
    })

    # --- Plot Rendering moved here ---
    output$rtnsPlot <- renderPlotly({
      req(data_status() == "Ready") # Ensure data is loaded globally
      
      df_rebased <- selectedRtns_rebased() 
      cmmBaseDate <- reactive_cmmBaseDate()
      current_slider_val <- input$selectedDate_rtns # Read namespaced input

      validate(
        need(df_rebased, message = FALSE), need(cmmBaseDate, message = FALSE), need(current_slider_val, message = FALSE),
        need(inherits(cmmBaseDate, "Date") && is.finite(cmmBaseDate), message = FALSE),
        need(inherits(current_slider_val, "Date") && is.finite(current_slider_val), message = FALSE),
        need(current_slider_val >= cmmBaseDate, message = "Waiting for slider update...") 
      )
          
      df_to_plot <- df_rebased %>% filter(date >= current_slider_val)
      
      validate(need(nrow(df_to_plot) > 0, "No data available for selected period.")) 

      # --- Color Generation ---
      instruments_in_plot <- unique(df_to_plot$istmt_plot) %>% sort() 
      num_instruments <- length(instruments_in_plot)
      plot_colors <- scales::hue_pal(l = 75)(num_instruments) 
      color_map <- setNames(plot_colors, instruments_in_plot)

      # --- Plotting ---
      df_to_plot %>% 
        plot_ly(
          x = ~date, y = ~rtn,
          color = ~istmt_plot, # Use the prefixed legend column
          colors = color_map, 
          linetype = ~type,  
          text = ~paste0("<b>", istmt, "</b>"), # Show original name on hover
          hoverinfo = 'x+y+text', 
          type = 'scatter', mode = 'lines'        
        ) %>% 
        layout( 
           title = "Cumulative Daily USD Returns",
           xaxis = list(title = "Date", gridcolor = '#444', color = '#eee', range = c(current_slider_val, END_DATE)), # Set range dynamically
           yaxis = list(title = "Cumulative Return", tickformat = ".2%", gridcolor = '#444', color = '#eee'), # Keep autoscale Y
           legend = list(
             title = list(text = "Order. | Ann. Rtn% | Instrument"), # Update title
             font = list(color = '#eee'),
             showlegend = show_legend() # Use reactive value passed in
           ),
           plot_bgcolor = '#222', paper_bgcolor = '#222', font = list(color = '#eee')
        )
    }) # End renderPlotly

    # --- Proxy and Legend Toggle Observer moved here ---
    plot_proxy <- plotlyProxy(session$ns("rtnsPlot"), session)

    observeEvent(show_legend(), { # Observe the reactive value passed in
      plotlyProxyInvoke(plot_proxy, "relayout", list(showlegend = show_legend()))
    }, ignoreNULL = FALSE) 
    
  }) # End moduleServer
}