# consts
DEBUG <- T

TOLERANCE <- .25

BENCH_MARKS <- list(
  `S&P 500` = "^GSPC",
  `Nasdaq 100` = "^NDX",
  `S&P 500 Xclu Info Tech` = 'SPXT'
)

NAMES_BMS <- names(BENCH_MARKS)
# end consts

# 1. Read in the trades CSV file
trades <- read_csv("ipts/trades.csv") %>%
  # manage exclusions here
  filter(
    !str_starts(tkr, 'USD|(coro|cs|etr|mg|of|sgn):')
    & tkr != 'lse:HYUD.il'
  ) %>%
  mutate(
    # Apply the cleaning function and store as a new column for Yahoo Finance tickers.
    yahoo_tkr = sapply(
      tkr,
      function(tkr) reformatTkr(tkr)['yh']
    ),
    fund = case_when(
      !is.na(fund) ~ fund,
      tkr %in% c('shse:513100', 'shse:513500', 'ACWI', 'IWY', 'QQQ', 'TECL', 'TQQQ', 'UPRO', 'VOO', 'XLK') ~ 'Indexes',
      tkr %in% c('AIQ', 'IETC', 'LRNZ') ~ 'AI',
      tkr %in% c('ARKQ', 'DRIV') ~ 'Self-Driving',
      tkr %in% c('BAR', 'STIP', 'TIP') ~ 'Inflation',
      tkr %in% c('CQQQ', 'MCHI') ~ 'China',
      tkr %in% c('SMH', 'SOXQ') ~ 'Semiconductor',
      T ~ 'Misc.'
    )
  )

# also must copy over ticker list
tkrs_df <- read_csv("rcds/tkrLst.csv")

#join currencies
trades <- trades %>%
  left_join(
    tkrs_df %>% select(tkr_new, cur),
    by = c("tkr" = "tkr_new")
  ) %>%
  mutate(
    cur = if_else(
      is.na(cur),
      case_when(
        grepl(
          "^hkse:", tkr,
          ignore.case = T
        ) ~ "HKD",
        grepl(
          "^(fra:|xpar:)", tkr,
          ignore.case = T
        ) ~ "EUR",
        grepl(
          "^jse:", tkr,
          ignore.case = T
        ) ~ "ZAR",
        grepl(
          "^(shse:|szse:)", tkr,
          ignore.case = T
        ) ~ "CNY",
        T ~ "USD"
      ),
      cur
    )
  )

runDate <- Sys.Date()
# Define the date range based on your trades.
start_date <- min(trades$date)
unique_tickers <- unique(trades$yahoo_tkr)
uniqueFunds <- unique(trades$fund)

currencies <- trades %>%
  distinct(cur) %>%
  filter(cur != "USD") %>%
  pull(cur)

progStepAmt <- 1 / (1 + length(unique_tickers) + 1 + length(currencies) + 1 + length(NAMES_BMS) + 1)

# Analyze trades.csv for maximum decimal places
# Remove any extraneous whitespace, then count the characters after the decimal point if present.
maxDecimals <- trades$unitCnt %>%
  as.character() %>%
  strsplit("\\.") %>%
  sapply(function(ps) {
      if (length(ps) == 1) return(0)

      nchar(ps[2])
  }) %>%
  max(na.rm = T)