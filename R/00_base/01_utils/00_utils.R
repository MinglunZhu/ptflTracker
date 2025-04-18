# Utility functions
# consts
RCDS_DIR <- 'rcds/prices/'
# end consts

################################################################## Copied from stkVal
# unfortunately, for shinyapps.io we can not use custom packages
# so, we will copy needed functions here
reformatTkr <- function(t) {
  tkr_guru <- t %>%
    #for lse iob
    str_remove('\\.il') %>%
    str_replace('\\/', '.') %>%
    str_replace('\\^([a-zA-z]*)(\\.PFD)?$', 'p\\1.PFD')

  #for US se
  tkr_yh <- t %>%
    str_replace('\\/', '-') %>%
    str_replace('\\^', '-P') %>%
    str_remove('\\.PFD$')

  tkr_fmp <- tkr_yh

  #for other se
  tmp <- str_match(t, '([a-z]{3,4})\\:([a-zA-Z0-9\\.]+)')
  xchg <- tmp[1, 2]#exchange code
  tmp <- tmp[1, 3]#ticker code

  if (!is.na(xchg)) {
    if (xchg == 'hkse') {
      suffix_fmp <- suffix_yh <- 'HK'

      #if hkse, and first digit is 0, remove it
      #else keep it
      # if (str_sub(tmp, 1, 1) == '0') {
      #   tmp <- str_sub(tmp, 2)
      # }
      tmp <- str_remove(tmp, '^0')
    } else if (xchg == 'fra') {
      suffix_yh <- 'F'
      suffix_fmp <- 'de'
    } else if (xchg == 'xpar') suffix_fmp <- suffix_yh <- 'PA'
    else if (xchg == 'lse') {
      #remove .PFD at the end
      # if (str_sub(tmp, -4, -1) == '.PFD') {
      #   tmp <- str_sub(tmp, 1, -5)
      # }
      #
      # #remove last .
      # if (str_sub(tmp, -1, -1) == '.') {
      #   tmp <- str_sub(tmp, 1, -2)
      # }
      tmp <- str_remove(tmp, '\\.PFD$')

      if (str_detect(tmp, RX_PTN_LSE_IOB)) {
        suffix_yh <- 'IL'

        tmp <- str_remove(tmp, RX_PTN_LSE_IOB)
      } else suffix_yh <- 'L'

      suffix_fmp <- suffix_yh

      tmp <- str_remove(tmp, '\\.$')
    } else if (xchg == 'shse') suffix_fmp <- suffix_yh <- 'SS'
    else if (xchg == 'szse') suffix_fmp <- suffix_yh <- 'SZ'
    else if (xchg == 'jse') suffix_fmp <- suffix_yh <- 'JO'

    tkr_yh <- paste0(tmp, '.', suffix_yh)
    tkr_fmp <- paste0(tmp, '.', suffix_fmp)
  }

  c(
    guru = tkr_guru,
    yh = tkr_yh,
    fmp = tkr_fmp
  )
}
################################################################## End stkVal

genSlctCol <- function(VAR_NAME, NAME) {
  tags$div(
    class = "settings-column",
    tags$li(actionButton(paste0("tglAll", VAR_NAME, "_btn"), paste("Toggle All", NAME))),
    tags$li(
      selectizeInput(
        paste0("selected", VAR_NAME, "_rtns"), paste("Select", NAME, "(Rebased):"),
        choices = list(),
        selected = character(0),
        multiple = TRUE,
        options = list(
          placeholder = paste('Type or click to select', tolower(NAME), '...'),
          plugins = list('remove_button')
          # Consider 'maxItems' if you want to limit selections
          # maxItems = 10
          #, onInitialize = I('function() { this.setValue(""); }') # Ensure placeholder shows
        )
      )
    )
  )
}

incProg <- function(DETAILS) {
    if (missing(DETAILS)) {
        incProgress(amount = progStepAmt)

        return()
    }

    incProgress(
        amount = progStepAmt,
        detail = DETAILS
    )
}

read_xts <- function(FP) {
  read_csv(FP) %>%
    column_to_rownames('Index') %>%
    as.xts()
}

# Add a function to safely download symbols with retries:
safeGetSymbols <- function(TKR, END_DATE = RUN_DATE, MAX_ATTEMPTS = 3, DELAY = 15, TIMEOUT = 120) {
  fp <- paste0(RCDS_DIR, TKR, '.csv')
  
  attempt <- 1
  
  repeat {
    result <- R.utils::withTimeout(
      {
        getSymbols(
          TKR,
          auto.assign = F,
          from = start_date,
          to = END_DATE
        )
      },
      timeout = TIMEOUT
    ) %>%
      try(silent = T)

    if (!inherits(result, "try-error")) {
      # save the result to the RCDS_DIR
      write.zoo(
        result, fp,
        sep = ",",
        quote = F  # Add this to prevent quoting
      )

      return(result)
    }

    if(attempt >= MAX_ATTEMPTS) {
      if (file.exists(fp)) {
        message(sprintf("Using local CSV file for %s due to download failure.", TKR))

        read_xts(fp) %>% return()
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
      trades_inited_df %>%
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

genSlctGrps <- function(DF, GRP_COL, SLCT_COL, GRP_LVLS, SLCT_LVLS, GRP_NAME) {
  # Capture expressions without evaluating
  grp_expr <- rlang::enquo(GRP_COL)
  slct_expr <- rlang::enquo(SLCT_COL)

  # Get string names for standard evaluation where needed
  #grp_colName <- rlang::quo_name(grp_expr)
  slct_colName <- rlang::quo_name(slct_expr)

  # Dynamic label for items in multiple groups
  multiple_label <- paste("Multiple", GRP_NAME)

  df <- DF %>%
    ungroup() %>%
    # Use only relevant columns and distinct pairs
    distinct(!!grp_expr, !!slct_expr) %>%
    # Count groups per selection item
    add_count(
      !!slct_expr,
      name = "grpCnt"
    ) %>%
    # Assign group name: use the dynamic label if count > 1, else the group column value
    mutate(grp = if_else(grpCnt > 1, multiple_label, as.character(!!grp_expr))) %>%
    # Keep only one row per selection item (group name is now consistent)
    distinct(grp, !!slct_expr) %>%
    # Apply factor levels for sorting
    asFctrCol(grp, c(GRP_LVLS, multiple_label)) %>%
    asFctrCol(!!slct_expr, SLCT_LVLS) %>%
    # Arrange based on the factor levels
    arrange(grp, !!slct_expr)

  df %>%
    # Use standard evaluation [[ ]] with the *string name* of the selection column
    # Use the selection column for both the label (name) and the value
    { stats::setNames(.[[slct_colName]], .[[slct_colName]]) } %>%
    # Split the named vector into a list based on the ordered factor 'grp'
    base::split(df$grp)
}