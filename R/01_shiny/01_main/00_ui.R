# UI Components

# needed for running the app without the run app function from RStudio
addResourcePath(
  prefix = "static",
  directoryPath = "www"
)

options(
  # 1 by defaul, but 2 can be use for circle / round charts
  spinner.type = 1,
  spinner.color = "#00fff2", # Your cyan color
  spinner.color.background = 'rgba(50, 50, 50, 0.2)'
)

app_ui <- tagList(
  useShinyjs(),

  tags$head(
    # Link to the external CSS file
    tags$link(
      rel = "stylesheet",
      type = "text/css", 
      href = "static/styles.css"
    ),
    # Link to the external JavaScript file
    tags$script(src = "static/script.js")
  ),

  dashboardPage(
    #the first element must be header
    dashboardHeader(
      title = "Interactive Portfolio Tracker",
      # Chart Menu remains unchanged
      tags$li(
        class = "dropdown",
        selectInput(
          "selected_chart", 'Select Chart:',
          choices = c("Returns", "Holdings"),
          selected = "Returns"
        )
      ),
      # Settings Menu remains unchanged
      tags$li(
        class = "dropdown",
        tags$a(
          href = "#",
          class = "dropdown-toggle",
          `data-toggle` = "dropdown",
          "Settings",
          tags$b(class = "caret")
        ),
        tags$ul(
          class = "dropdown-menu",
          # Container for columns, prevent closing on click
          tags$div(
            class = "settings-container", 
            onclick = "if (!event.target.classList.contains('glyphicon')) event.stopPropagation();",
            # Date Sliders
            conditionalPanel(
              condition = "input.selected_chart == 'Returns'",
              rtnsUI_sldr("rtns")
            ), # end conditionalPanel
            conditionalPanel(
              condition = "input.selected_chart == 'Holdings'",
              hldgsUI_sldr("hldgs")
            ), # end conditionalPanel
            # Container for the three columns
            tags$div(
              class = "settings-columns",
              conditionalPanel(
                  condition = "input.selected_chart == 'Holdings'",
                  tags$div(
                      class = "settings-column",
                      hldgsUI_slct_plotType('hldgs')
                  )
              ),
              # Column 1: sources
              tags$div(
                class = "settings-column",
                tags$li(
                  checkboxGroupInput(
                    "selectedSrcs_rtns", "Select Sources (Rebased):", 
                    choices = character(0),
                    selected = character(0)
                  )
                )
              ),
              # col 2 : ctgs
              genSlctCol("Ctgs", "Categories"),
              # col 3 : funds
              genSlctCol("Funds", "Funds"),
              # col 4 : tickers
              genSlctCol("Uas", "Underlying Assets"),
              # Column 3: Other Settings
              tags$div(
                class = "settings-column",
                tags$li(
                  checkboxInput(
                    "inclCash", "Include Cash", 
                    value = F
                  )
                ),
                conditionalPanel(
                  condition = "input.selected_chart == 'Returns'",
                  tags$li(  
                    checkboxInput(
                      "inclClosed", "Include Closed Instruments", 
                      value = F
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.selected_chart == 'Returns'",
                  tags$li(  
                    checkboxInput(
                      "showLegend", "Show Legend", 
                      value = T
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.selected_chart == 'Returns'",
                  tags$li(  
                    checkboxInput(
                      "showRangeSldr", "Show Range Slider", 
                      value = F
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.selected_chart == 'Holdings'",
                  tags$li(  
                    checkboxInput(
                      "showColorBar", "Show Color Bar", 
                      value = F
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),

    # very fixed structure expected, the second must be aside
    # No sidebar
    dashboardSidebar(disable = T),
    
    # Main content area for the plot
    dashboardBody(
      conditionalPanel(
        condition = "input.selected_chart == 'Returns'",
        shinycssloaders::withSpinner(
          rtnsUI_plot("rtns")
        )
      ),
      conditionalPanel(
        condition = "input.selected_chart == 'Holdings'",
        shinycssloaders::withSpinner(
          hldgsUI_plot("hldgs"),
          type = 2
        )
      ),

      # Add the activation button here, positioned fixed
      tags$button(
        id = "header-toggle-btn",
        class = "header-toggle",
        icon("bars") # Using a Font Awesome icon (requires fontawesome library)
        # Or use text: "Menu"
      ),
    ) # end dashboardBody
  )
)