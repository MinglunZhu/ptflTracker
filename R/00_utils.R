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

asFctrCol <- function(DF, COL, LVLS) {
  # Capture the COL expression without evaluating it
  col_xpr <- rlang::enquo(COL)

  DF %>%
    mutate(
      !!col_xpr := factor(
        !!col_xpr,
        levels = LVLS
      )
    )
}

getTkrGrps <- function(DF, TKR_COL) {
  # Capture the TKR_COL expression without evaluating it
  tkr_expr <- rlang::enquo(TKR_COL)
  # Get the string name of the column for use where needed
  tkr_colName <- rlang::quo_name(tkr_expr)

  df <- DF %>%
    ungroup() %>%
    # Use only relevant columns and distinct ticker/fund pairs
    distinct(fund, !!tkr_expr) %>%
    # Count funds per ticker
    add_count(
      !!tkr_expr,
      name = "fundCnt"
    ) %>%
    # Assign group name
    mutate(grp = if_else(fundCnt > 1, "Multiple Funds", fund)) %>%
    # Keep only one row per ticker (the group name is now consistent for multi-fund tickers)
    distinct(grp, !!tkr_expr) %>%
    asFctrCol(grp, c(uniqueFunds_sorted, "Multiple Funds")) %>%
    asFctrCol(!!tkr_expr, uniqueTkrs_sorted) %>%
    arrange(grp, !!tkr_expr)

  df %>%
    # Use standard evaluation [[ ]] with the *string name* of the column
    # Use the ticker column for both the label (name) and the value
    { stats::setNames(.[[tkr_colName]], .[[tkr_colName]]) } %>% 
    # Split the named vector into a list based on the ordered factor 'grp'
    base::split(df$grp)   
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
