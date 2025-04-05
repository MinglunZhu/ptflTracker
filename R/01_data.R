#consts
TOLERANCE <- .25

BENCH_MARKS <- list(
  `S&P 500` = "^GSPC",
  `Nasdaq 100` = "^NDX",
  `S&P 500 Xclu Info Tech` = 'SPXT'
)

NAMES_BMS <- names(BENCH_MARKS)

END_DATE <- Sys.Date()
#end consts

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

# Define the date range based on your trades.
start_date <- min(trades$date)
unique_tickers <- unique(trades$yahoo_tkr)
uniqueFunds <- unique(trades$fund)
all_dates <- seq(
  start_date, END_DATE,
  by = "days"
)
dayCnt <- length(all_dates)
#Using empty xts to hold downloaded prices for all dates, then convert to dataframe is more perfomrance friendly
mtXts_mtx <- matrix(
  nrow = dayCnt,
  ncol = 0
) %>% xts(all_dates)
zeroXts_vctr <- rep(0, dayCnt) %>% xts(order.by = all_dates)