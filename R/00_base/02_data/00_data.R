# consts
DEBUG <- F

TOLERANCE <- .25

BENCH_MARKS <- list(
  `S&P 500` = "^GSPC",
  `Nasdaq 100` = "^NDX",
  `S&P 500 Xclu Info Tech` = 'SPXT',
  `Mag 7` = 'MAGS'
)

NAMES_BMS <- names(BENCH_MARKS)

RUN_DATE <- Sys.Date()
# end consts

# 1. Read in the trades CSV file
trades <- read_csv("ipts/trades.csv") %>%
  # manage exclusions here
  filter(
    !str_starts(tkr, 'USD|(coro|cs|etr|mg|of|sgn):')
    & !( tkr %in% c('lse:HYUD.il', 'CIVI') )
  ) %>%
  mutate(
    # Apply the cleaning function and store as a new column for Yahoo Finance tickers.
    yahoo_tkr = sapply(
      tkr,
      function(tkr) reformatTkr(tkr)['yh']
    ),
    # because the rest of the code uses yahoo_tkr, we want to also use yahoo_tkr to match
    fund = coalesce(fund, yahoo_tkr), # returns whichever is not NA
    ctg = case_when(
      fund %in% c('513100.SS', '513500.SS', 'ACWI', 'QQQ', 'TQQQ', 'UPRO', 'VOO', 'STXNDQ.JO', 'SYG500.JO') ~ 'Market Indexes',
      fund %in% c('IWY') ~ 'General Growth',
      fund %in% c('TECL', 'XLK') ~ 'General Tech',
      fund %in% c('AIQ', 'IETC', 'LRNZ') ~ 'AI',
      fund %in% c('ARKQ', 'DRIV') ~ 'Self-Driving',
      fund %in% c('BAR', 'STIP', 'TIP') ~ 'Inflation',
      fund %in% c('CQQQ', 'MCHI', 'MCH') ~ 'China',
      fund %in% c('SMH', 'SOXQ') ~ 'Semiconductor',
      fund %in% c('of:SRA') ~ 'Retirement',
      fund %in% c('of:TNIF', 'of:TNNIF') ~ 'Mega Cap',
      fund %in% c('of:UCGF', 'SVF', 'of:TNUCGF') ~ 'Value',
      T ~ 'Misc.'
    ),
    src = if_else(
      str_starts(fund, 'of:')
      | fund == 'SVF',
      'Internal', 'External'
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

currencies <- trades %>%
  distinct(cur) %>%
  filter(cur != "USD") %>%
  pull(cur)

#                   dl price + prices +          dl fx + fxs +           calc vals + dl bms + bms + done
progStepAmt <- 1 / (1 + length(unique_tickers) + 1 + length(currencies) + 1 + 1 + length(NAMES_BMS) + 1)

# # tkrs that are not the same as any fund
# # not just the tkr's own fund
# uniqueTkrs_actl <- trades %>%
#   filter(!(yahoo_tkr %in% trades$fund)) %>%
#   distinct(yahoo_tkr) %>%
#   pull(yahoo_tkr)

# Analyze trades.csv for maximum decimal places
# Remove any extraneous whitespace, then count the characters after the decimal point if present.
# read col as text bypass R's default truncation when converting to text
maxDecimals <- read_csv(
    "ipts/trades.csv",
    col_types = cols( unitCnt = col_character() ),
    col_select = unitCnt
  )$unitCnt |>
  na.omit() |>
  strsplit("\\.") |>
  sapply(
    function(PS) {
      if (length(PS) == 1) return(0)

      nchar(PS[2])
    }
  ) |>
  max()