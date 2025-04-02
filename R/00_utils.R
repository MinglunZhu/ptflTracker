# Utility functions
# consts
RCDS_DIR <- 'rcds/'
# end consts

# Add a function to safely download symbols with retries:
safeGetSymbols <- function(TKR, MAX_ATTEMPTS = 3, DELAY = 15) {
  attempt <- 1
  fp <- paste0(RCDS_DIR, TKR, '.csv')

  repeat {
    result <- try(
      getSymbols(
        TKR,
        auto.assign = F, 
        from = start_date, 
        to = END_DATE
      ), 
      silent = T
    )

    if (!inherits(result, "try-error")) {
      # save the result to the RCDS_DIR
      write.zoo(
        result, fp, 
        sep = ","
      )

      return(result)
    } 

    if(attempt >= MAX_ATTEMPTS) {
      if (file.exists(fp)) {
        message(sprintf("Using local CSV file for %s due to download failure.", TKR))

        read.zoo(fp) %>% return()
      } else stop("No local data available and download failed.")
    }

    message(sprintf(
      '[%s] Attempt %d/%d failed, retrying in %ds: %s',
      format(Sys.time(), "%H:%M:%S"),
      attempt, MAX_ATTEMPTS, DELAY, TKR
    ))
    Sys.sleep(DELAY)

    attempt <- attempt + 1
  }
}

# Usage would be:
# tkrs_df <- vals_df %>%
#   summarizeValuesByCols(tkr)
summariseValCfBy <- function(DF, ...) {
  join_cols <- quos(...)
  group_cols <- c(quo_name(join_cols[[1]]), "date")
  
  DF %>%
    group_by(!!!syms(group_cols)) %>%
    summarise(
      val = sum(
        val, 
        na.rm = T
      )
    ) %>%
    left_join(
      trades %>%
        group_by(!!!syms(group_cols)) %>%
        summarise(cf = sum(amt_usd)),
      by = group_cols
    ) %>%
    replace(., is.na(.), 0) %>%
    rename(istmt = !!join_cols[[1]])
}

calcCmltvRtns <- function(RTN) cumprod(1 + RTN) - 1

calcCmltvCf_cashAcc <- function(DF) {
  DF %>%
    arrange(date) %>%
    mutate(cmltvCf_cashAcc = cumsum(-cf))
}

calcSttgCash <- function(DF) {
  DF %>%
    summarise(
      minCmltvCf = min(
        cmltvCf_cashAcc,
        na.rm = T
      )
    ) %>%
    mutate(sttgCash = if_else(minCmltvCf < 0, -minCmltvCf, 0))
}

sortFunds <- function(vctr) {
  if ("Misc." %in% vctr) {
    vctr <- vctr %>%
      setdiff("Misc.") %>%
      c("Misc.")
  }

  vctr
}

getTkrGrps <- function(DF, TKR_COL) {
  # Capture the TKR_COL expression without evaluating it
  tkr_expr <- rlang::enquo(TKR_COL)
  # Get the string name of the column for use where needed
  tkr_colName <- rlang::quo_name(tkr_expr)

  df <- DF %>%
    ungroup() %>%
    # Use only relevant columns and distinct ticker/fund pairs
    distinct(fund, !!tkr_expr)%>%
    # Filter out any rows where fund might be NA or empty if they exist
    #filter(!is.na(fund) & fund != "") %>% 
    # Count funds per ticker
    add_count(
      !!tkr_expr, 
      name = "fundCnt"
    ) %>%
    # Assign group name
    mutate(grp = if_else(fundCnt > 1, "Multiple Funds", fund)) %>%
    # Keep only one row per ticker (the group name is now consistent for multi-fund tickers)
    distinct(grp, !!tkr_expr)
    # If a ticker had no fund assignment in trades, group_name will be NA. 
    # Decide how to handle these - perhaps they shouldn't be selectable? 
    # For now, let's filter them out if they don't have a group.
    #filter(!is.na(group_name)) %>% 
    # Use the ticker symbol as the display label
    # Arrange for consistent order within groups
    #arrange(group_name, label)

  df <- df %>%
    mutate(
      grp = factor(
        grp,
        levels = df$grp %>% unique() %>% sortFunds()
      )
    ) %>%
    arrange(grp)
  
  #must use the sorted grp, that's why we save the sorted df first
  df %>%
    #Create the named list structure: list("GroupName" = c("Label"="Value", ...), ...)
    { setNames(.[[tkr_colName]], .[[tkr_colName]]) } %>% # Create the named vector part (Label=Value)
    split(df$grp)   # Split into list based on group_name,
}

createPieLayout <- function(
  PLOT, TITLE, 
  TITLE_FONT = list(
    family = "Arial", 
    size = 18, 
    color = "#eee"
  ),
  FONT = list(color = '#eee'), 
  BG_COLOR = '#222'
) {
  layout(
    PLOT,
    title = list(
      text = TITLE,
      font = TITLE_FONT
    ),
    paper_bgcolor = BG_COLOR,
    plot_bgcolor = BG_COLOR,
    font = FONT,
    showlegend = F,
    # Add these for complete dark mode
    scene = list(bgcolor = BG_COLOR)
  )
}
