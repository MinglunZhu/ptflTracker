# Don't seem necessary
#handlers(global = TRUE) # Enable global progress handlers
#handlers("shiny")       # Use the Shiny-specific handler

# Module UI (Minimal - can be expanded later if needed)
dataInitUI <- function(id) {
    ns <- NS(id)
    # We might not need specific UI elements if using a global overlay/spinner
    # But could add a uiOutput here for module-specific status messages
    tagList()
}

# Module Server
dataInitServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            # --- Trigger Async Load Once ---
            # observeEvent(
            #     once = TRUE, 
            #     session$ns("trigger"), { # Use namespaced trigger
            #         # Return NULL from observeEvent
            #         NULL
            #     }
            # ) # End observeEvent trigger

            # Use session$ns("trigger") to ensure the event is namespaced if UI is used
            # Trigger the observer manually once the session starts
            # observe({
            #     session$sendCustomMessage("trigger-data-init", list(id = session$ns("trigger")))
            # })
            
            # if weekend, and was updated friday or later
            cd <- Sys.Date()
            # end date is set on init and will not change after, so no need for reactive val
            #end_date <- reactiveVal(cd)
            msg <- NULL

            if (
                weekdays(cd) %in% c("Saturday", "Sunday")
                && exists("last_initDate", envir = .GlobalEnv)
                && last_initDate >= cd - ((format(cd, "%u") %>% as.integer() - 5) %% 7)
            ) {
                if (!DEBUG) {
                    msg <- paste("Data Initialization Module: Skipping data initialization on weekend. Last initialization date:", last_initDate)
                }
            } else if (DEBUG) {
                msg <- "Data Initialization Module: Skipping data initialization in debug mode."
            }

            if (!is.null(msg)) {
                message(msg)
                return(last_initDate)
            }

            message("Data Initialization Module: Session started, beginning data initialization...")

            # Wrap future chain in withProgress
            withProgressShiny(
                message = 'Initializing Data...',
                value = 0,
                {
                    tryCatch(
                        {
                            all_dates <- seq(
                                start_date, cd,
                                by = "days"
                            )

                            dayCnt <- length(all_dates)
                            #Using empty xts to hold downloaded prices for all dates, then convert to dataframe is more perfomrance friendly
                            mtXts_mtx <- matrix(
                                nrow = dayCnt,
                                ncol = 0
                            ) %>% xts(all_dates)
                            zeroXts_vctr <- rep(0, dayCnt) %>% xts(order.by = all_dates)

                            # 3. Download daily price data for each ticker from Yahoo Finance.
                            prices_xts <- NULL

                            message("Future: Downloading prices...")
                            incProg("Downloading prices...")

                            #even passing a vector to getSybmols
                            #it still downloads one by one due to yahoo's API only support 1 stock
                            #we only need to introduce a delay / wait if we download more than 5-10 stocks per second

                            # Merge all price series sequentially for better memory efficiency
                            for (t in unique_tickers) {
                                cat("Downloading price data for", t, "\n")

                                # Must use close price because the unit count is split adjusted
                                # and close price in yahoo is split adjusted
                                # adjusted price in yahoo is both split and dividend adjusted
                                p <- t %>%
                                    safeGetSymbols(cd) %>%
                                    Cl()

                                if (unique(trades$cur[trades$yahoo_tkr == t]) == 'ZAR') p <- p / 100

                                colnames(p) <- t

                                prices_xts <- if (is.null(prices_xts)) p
                                    else {
                                        merge(
                                            prices_xts, p,
                                            join = "outer",
                                            check.names = F
                                        )
                                    }

                                incProg()
                            }

                            prices_xts <- prices_xts %>%
                                na.locf() %>%                      # Fill NAs
                                .[!duplicated(index(.)), ]         # Remove duplicate timestamps

                            prices_daily <- mtXts_mtx %>%
                                merge(
                                    prices_xts,
                                    all = T,
                                    check.names = F
                                ) %>%
                                na.locf()

                            # 1. Convert prices_daily xts to a long dataframe for joining
                            prices_daily_df <- prices_daily %>%
                                as.data.frame() %>%
                                rownames_to_column(var = "date") %>%
                                mutate(date = as.Date(date)) %>%
                                pivot_longer(
                                    cols = -date,
                                    names_to = "tkr",
                                    values_to = "price",
                                    values_drop_na = TRUE # Optional: drop rows where price is NA early
                                )

                            #download exchange rates
                            message("Future: Downloading FX rates...")
                            # ... FX download logic ... -> xchgRates_df
                            incProg("Downloading FX rates...")

                            # download exchange rates and store in a dataframe in 3 columns date, cur, xchgRate_usd
                            # 3. Create a full grid of all dates and all currencies
                            xchgRates_df <- expand_grid(
                                date = all_dates, # Assumes all_dates is available
                                cur = c(currencies, "USD")
                            ) %>%
                                # 4. Join raw rates onto the full grid
                                left_join(
                                    currencies %>%
                                        lapply(function(curr) {
                                            cat("Downloading USD to", curr, "FX data\n")

                                            rates <- paste0(curr, "=X") %>%
                                                safeGetSymbols(cd) %>%
                                                Cl()

                                            # Convert xts to dataframe
                                            df <- data.frame(
                                                date = index(rates),
                                                xchgRate_usd2Lc = coredata(rates)
                                            ) %>%
                                                mutate(cur = curr)

                                            colnames(df)[2] <- "xchgRate_usd2Lc"

                                            incProg()

                                            df
                                        }) %>%
                                        bind_rows() %>% # Combine list of dataframes into one
                                        filter(!is.na(xchgRate_usd2Lc)), # Remove rows with NA rates if any
                                        #arrange(cur, date) # Optional: sort for clarity
                                    by = c("date", "cur")
                                ) %>%
                                # 5. Set USD rate to 1
                                mutate(xchgRate_usd2Lc = ifelse(cur == "USD", 1, xchgRate_usd2Lc)) %>%
                                # 6. Fill missing rates using LOCF within each currency group
                                arrange(cur, date) %>%
                                group_by(cur) %>%
                                fill(
                                    xchgRate_usd2Lc, 
                                    .direction = "down"
                                ) %>% # Last Observation Carried Forward
                                # Optional: Fill initial NAs if any currency has no data before the first date
                                # fill(xchgRate_usd, .direction = "up") %>%
                                ungroup()
                                # Ensure no NAs remain (shouldn't happen if LOCF worked and USD is handled)
                                # filter(!is.na(xchgRate_usd)) # Reconsider if initial NAs are possible/problematic

                            message("Future: Processing trades...")

                            # Create a new column for adjusted unit count (default to raw unitCnt)
                            trades_inited_df <<- trades %>%
                                left_join(
                                    xchgRates_df,
                                    by = c("date", "cur")
                                ) %>%
                                mutate(
                                    # For HKD stocks, the trade amount is in USD. Convert it back to HKD.
                                    amt_lclCur = if_else(cur == 'HKD', amt * xchgRate_usd2Lc, amt), #amt in local currency
                                    amt_usd = amt_lclCur / xchgRate_usd2Lc
                                ) %>%
                                #adjust for stock splits
                                # 2. Join Yahoo prices onto the trades dataframe
                                left_join(
                                    prices_daily_df,
                                    by = c("date", "yahoo_tkr" = "tkr")
                                ) %>%
                                mutate(
                                    # Compute the trade price (ensure amt_lclCur is calculated first if needed)
                                    # Handle division by zero or NA unitCnt if necessary
                                    price_trade = amt_lclCur / unitCnt,

                                    # Calculate the ratio (only if p_trade and yahoo_p are valid)
                                    ratio = ifelse(
                                        price_trade != 0
                                        & !is.na(price),
                                        price / price_trade,
                                        NA
                                    ),

                                    # Calculate the adjusted unit count
                                    adj_unitCnt = unitCnt * case_when(
                                        (
                                            # No adjustment if ratio is NA or amount is 0
                                            is.na(ratio)
                                            | amt == 0
                                            # Special tolerance for AMTD on specific date
                                            | abs(ratio - 1) < if_else(tkr == 'AMTD' & date == '2023-11-17', .5, TOLERANCE)
                                        ) ~ 1,
                                        # Reverse split
                                        ratio > 1 ~ 1 / round(ratio),
                                        # Forward split
                                        ratio < 1 ~ round(1 / ratio),
                                        # Default case (should ideally not be hit if logic above is complete)
                                        T ~ 1
                                    )
                                )
                                # # Clean up intermediate columns if desired
                                # select(-p_trade, -ratio, -adj_factor, -yahoo_p) # Remove yahoo_p if joined temporarily

                            # Convert non-USD prices to USD using the appropriate FX rate.
                            prices_daily_df <- prices_daily_df %>%
                                left_join(
                                    trades %>% distinct(yahoo_tkr, cur),
                                    by = c("tkr" = "yahoo_tkr")
                                ) %>%
                                left_join(
                                    xchgRates_df,
                                    by = c("date", "cur")
                                ) %>%
                                mutate(price_usd = price / xchgRate_usd2Lc)

                            message("Future: Calculating values...")
                            incProg("Calculating values...")

                            # 1. Create a complete grid of all dates and tickers in the subset
                            vals_df <- trades_inited_df %>%
                                distinct(src, ctg, fund, yahoo_tkr) %>%
                                tidyr::crossing(date = all_dates) %>%
                                # 3. Join trades onto the grid and calculate cumulative holdings
                                left_join(
                                    # 2. Prepare trades data: select relevant columns and sum units for same day/ticker
                                    trades_inited_df %>%
                                        group_by(src, ctg, fund, date, yahoo_tkr) %>%
                                        summarise(
                                            adj_unitCnt = sum(
                                                adj_unitCnt, 
                                                na.rm = T
                                            ) %>% round(maxDecimals),
                                            .groups = 'drop'
                                        ) %>%
                                        filter(adj_unitCnt != 0),
                                    by = c('src', 'ctg', 'fund', "date", "yahoo_tkr")
                                ) %>%
                                # Replace NA trade units with 0
                                replace(., is.na(.), 0) %>%
                                rename(tkr = yahoo_tkr) %>%
                                #remove empty fund ticker combo
                                group_by(src, ctg, fund, tkr) %>%
                                filter(abs(adj_unitCnt) %>% sum() > 0) %>%
                                # Arrange and calculate cumulative sum within each ticker group
                                arrange(date) %>%
                                mutate(cmltvUnitCnt = cumsum(adj_unitCnt) %>% round(maxDecimals)) %>%
                                # 4. Join holdings with daily prices
                                left_join(
                                    prices_daily_df,
                                    by = c("date", "tkr")
                                ) %>%
                                # Calculate market value for each holding on each day
                                # If price (yahoo_p) is NA, the value will be NA
                                mutate(val = cmltvUnitCnt * price_usd) %>%
                                ungroup() %>%
                                select(src, ctg, fund, tkr, date, cmltvUnitCnt, val) 

                            vals_df %>%
                                filter(
                                    tkr == '3988.HK'
                                ) %>%
                                write_csv("opts/debug/vals_df.csv")

                            message("Future: Calculating returns...")

                            funds_df <- vals_df %>%
                                summariseValCfBy(fund)
                                
                            rtns_df <- funds_df %>%
                                mutate(type = 'Fund') %>%
                                bind_rows(
                                    #append overall ptfl
                                    funds_df %>%
                                        group_by(date) %>%
                                        summarise(
                                            val = sum(val),
                                            cf = sum(cf)
                                        ) %>%
                                        mutate(
                                            istmt = 'Overall Portfolio',
                                            type = 'Portfolio / Benchmark'
                                        ),
                                    # append srcs
                                    vals_df %>%
                                        summariseValCfBy(src) %>%
                                        mutate(type = 'Source'),
                                    # append ctgs
                                    vals_df %>%
                                        summariseValCfBy(ctg) %>%
                                        mutate(type = 'Category'),
                                    #append tkrs
                                    #only tkrs that are not already included in funds
                                    #because those are going to be included in funds
                                    vals_df %>%
                                        filter(!(tkr %in% trades$fund)) %>%
                                        # yahoo_tkr needed for trades df
                                        rename(yahoo_tkr = tkr) %>%
                                        summariseValCfBy(yahoo_tkr) %>%
                                        mutate(type = 'Underlying Asset')
                                        # ungroup() %>%
                                        # arrange(date) # Ensure dates are sorted
                                ) %>%
                                group_by(istmt) %>%
                                # Compute daily returns adjusted for CF:
                                # Vectorized return calculation
                                mutate(val_prv = lag(val)) %>%
                                mutate(
                                    rtn_xcluCash = if_else(
                                        is.na(val_prv)
                                        | val_prv == 0,
                                        0,
                                        (val - pmin(cf, 0)) / (val_prv + pmax(cf, 0)) - 1
                                    )
                                ) %>%
                                # Compute cumulative returns
                                mutate(cmltvRtn_xcluCash = calcCmltvRtns(rtn_xcluCash)) %>%
                                calcCmltvCf_cashAcc()

                            #rtns df already grouped by istmt
                            sc_df <- calcSttgCash(rtns_df)

                            rtns_df <- rtns_df %>%
                                left_join(
                                    sc_df,
                                    by = c('istmt')
                                ) %>%
                                mutate(
                                    val_cash = sttgCash + cmltvCf_cashAcc,
                                    cmltvRtn_inclCash = ((val + val_cash) / sttgCash) - 1
                                ) %>%
                                ungroup()

                            incProg("Downloading benchmark index prices...")

                            # Download S&P 500 and Nasdaq 100 index data and calculate cumulative returns
                            for (n in NAMES_BMS) {
                                t <- BENCH_MARKS[[n]]

                                cat("Downloading price data for", t, "\n")

                                r <- (
                                    mtXts_mtx %>%
                                        merge(
                                            t %>%
                                                safeGetSymbols(cd) %>%
                                                Cl(),
                                            all = T
                                        ) %>%
                                        na.locf() %>%
                                        na.omit()
                                )[, 1] %>%
                                    dailyReturn()

                                df <- data.frame(
                                    date = index(r),
                                    istmt = n,
                                    type = 'Portfolio / Benchmark',
                                    rtn_xcluCash = coredata(r)
                                )

                                colnames(df)[4] <- "rtn_xcluCash"

                                rtns_df <- bind_rows(
                                    rtns_df,
                                    mutate(
                                        df,
                                        cmltvRtn_xcluCash = calcCmltvRtns(rtn_xcluCash),
                                        cmltvRtn_inclCash = cmltvRtn_xcluCash
                                    )
                                )

                                incProg()
                            }

                            message("Future: Preparing choice lists...")

                            rtns_anlzed_df <- rtns_df %>%
                                # Ensure we only use days where a return could be calculated
                                filter(rtn_xcluCash != 0) %>%
                                # Group by instrument to calculate per-instrument metrics
                                group_by(istmt, type) %>%
                                summarise(
                                    # Calculate the sum of log growth factors (more stable than product)
                                    # Add a very small number to handle potential returns of exactly -1 (100% loss)
                                    # Or filter out rtn_xcluCash <= -1 if that's more appropriate.
                                    log_growth_factors = sum( log(1 + pmax(rtn_xcluCash, -0.999999)) ),
                                    dayCnt = n(), # Count the number of valid return days
                                    .groups = 'drop' # Drop grouping after summarising
                                ) %>%
                                # Annualize using 252 trading days
                                mutate(rtn_anlzed = exp(log_growth_factors / dayCnt)^252 - 1) %>%
                                select(istmt, type, rtn_anlzed)

                            # Create the ordered vector according to the specified requirements
                            uniqueSrcs_sorted <<- rtns_anlzed_df %>%
                                filter(type == 'Source') %>%
                                arrange(desc(rtn_anlzed)) %>%
                                pull(istmt)

                            # Order the ctgs by annualized returns (descending), but keep "Misc." at the end
                            uniqueCtgs_sorted <- rtns_anlzed_df %>%
                                filter(
                                    type == 'Category' 
                                    & istmt != "Misc."
                                ) %>%
                                arrange(desc(rtn_anlzed)) %>%
                                pull(istmt) %>%
                                c("Misc.")

                            uniqueFunds_sorted <- rtns_anlzed_df %>%
                                filter(type == 'Fund') %>%
                                arrange(desc(rtn_anlzed)) %>%
                                pull(istmt)

                            # Order the tickers by annualized returns (descending)
                            # needed by util functions
                            uniqueUas_sorted <- rtns_anlzed_df %>%
                                filter(type == 'Underlying Asset') %>%
                                arrange(desc(rtn_anlzed)) %>%
                                pull(istmt)

                            # Join rtns_df with rtns_anlzed_df to get the annualized returns
                            rtns_df <<- rtns_df %>%
                                left_join(
                                    select(rtns_anlzed_df, -type), 
                                    by = "istmt"
                                ) %>%
                                # Create istmt_legend with format: number. annualized return% (2 decimals) istmt
                                mutate(
                                    istmt_legend = sprintf(
                                        "%03d. | %.2f%% | %s",
                                        match(
                                            istmt,
                                            c(
                                                "Overall Portfolio", uniqueSrcs_sorted, uniqueCtgs_sorted, uniqueFunds_sorted,
                                                uniqueUas_sorted, NAMES_BMS
                                            )
                                        ),
                                        rtn_anlzed * 100,
                                        istmt
                                    )
                                )

                            # Get latest holdings for each fund/ticker combination
                            latest_hldgs_df <- vals_df %>%
                                filter(
                                    date == cd
                                    & cmltvUnitCnt > 0
                                ) %>% # Filter for the last calculated date
                                distinct(src, ctg, fund, tkr)

                            # Identify open source (positive holding for the source itself, excluding individual tickers)
                            # This requires summarizing vals_df by scource first
                            openSrcs_sorted <<- latest_hldgs_df %>%
                                distinct(src) %>%
                                asFctrCol(src, uniqueSrcs_sorted) %>%
                                arrange(src) %>%
                                pull(src)

                            uniqueCtgGrps <<- genSlctGrps(vals_df, src, ctg, uniqueSrcs_sorted, uniqueCtgs_sorted, "Sources")
                            openCtgGrps <<- genSlctGrps(latest_hldgs_df, src, ctg, uniqueSrcs_sorted, uniqueCtgs_sorted, "Sources")

                            uniqueFundGrps <<- genSlctGrps(vals_df, ctg, fund, uniqueCtgs_sorted, uniqueFunds_sorted, "Categories")
                            openFundGrps <<- genSlctGrps(latest_hldgs_df, ctg, fund, uniqueCtgs_sorted, uniqueFunds_sorted, "Categories")

                            # Determine fund assignments for each ticker
                            # only include tickers that are not the same as the fund
                            # this basically has to be the same logic as returns
                            rtns_tkrs <- rtns_df %>%
                                filter(type == 'Underlying Asset') %>%
                                pull(istmt)

                            uniqueUaGrps <<- vals_df %>%
                                filter(tkr %in% rtns_tkrs) %>%
                                genSlctGrps(fund, tkr, uniqueFunds_sorted, uniqueUas_sorted, "Funds")

                            # Identify open tickers (positive holding in ANY fund)
                            openUaGrps <<- latest_hldgs_df %>%
                                filter(tkr %in% rtns_tkrs) %>%
                                genSlctGrps(fund, tkr, uniqueFunds_sorted, uniqueUas_sorted, "Funds")

                            message("Future: Calculating holdings...")

                            # add cash for overall portfolio only
                            # vals_df has each tkr in each fund in each ctg in each src where tkr has units
                            hldgs_srcs_df <- bind_rows(
                                # add each src with portfolio as parent
                                rename(
                                    vals_df,
                                    lbl = src
                                ) %>%
                                    left_join(
                                        rtns_anlzed_df %>%
                                            filter(type == 'Source'),
                                        by = c('lbl' = 'istmt')
                                    ),
                                # add cash for portfolio
                                rtns_df %>%
                                    filter(istmt == 'Overall Portfolio') %>%
                                    select(istmt, date, val_cash) %>%
                                    rename(val = val_cash) %>%
                                    mutate(
                                        lbl = 'Cash',
                                        isInclCash = T
                                        # if NA, then it defaults to 0
                                        #rtn_anlzed = 0
                                    )
                            )

                            # lbl parent val
                            hldgs_df <<- hldgs_srcs_df %>%
                                mutate(parent = 'Portfolio') %>%
                                bind_rows(
                                    # add portfolio with cash
                                    hldgs_srcs_df %>%
                                        mutate(
                                            isInclCash = T,
                                            # anlzed rtn only have xclu cash
                                            # for with cash, we will make it NA and see how it goes
                                            rtn_anlzed = NA
                                        ) %>%
                                        bind_rows(
                                            # without cash
                                            hldgs_srcs_df %>%
                                                filter(lbl != 'Cash') %>%
                                                mutate(
                                                    isInclCash = F,
                                                    rtn_anlzed = rtns_anlzed_df %>%
                                                        filter(istmt == 'Overall Portfolio') %>%
                                                        pull(rtn_anlzed)
                                                )
                                        ) %>%
                                        mutate(
                                            lbl = 'Portfolio',
                                            parent = ''
                                        ),
                                    # add funds
                                    # funds' parent (ctg) has compound id (src + ctg)
                                    vals_df %>%
                                        rename(lbl = fund) %>%
                                        mutate(parent = paste(src, ctg)) %>%
                                        left_join(
                                            filter(
                                                rtns_anlzed_df,
                                                type == 'Fund'
                                            ),
                                            by = c('lbl' = 'istmt')
                                        )
                                ) %>%
                                mutate(id = lbl) %>%
                                bind_rows(
                                    # add ctgs
                                    vals_df %>%
                                        rename(
                                            lbl = ctg,
                                            parent = src
                                        ) %>%
                                        left_join(
                                            filter(
                                                rtns_anlzed_df,
                                                type == 'Category'
                                            ),
                                            by = c('lbl' = 'istmt')
                                        ) %>%
                                        # add underlying assets
                                        # only tkrs that are not the same as the fund
                                        # if a tkr is the same as one of the funds, but not the same as its own fund
                                        # then we still want to show it
                                        bind_rows(
                                            vals_df %>%
                                                filter(tkr != fund) %>%
                                                rename(
                                                    lbl = tkr,
                                                    parent = fund
                                                ) %>%
                                                left_join(
                                                    filter(
                                                        rtns_anlzed_df,
                                                        type == 'Underlying Asset'
                                                    ),
                                                    by = c('lbl' = 'istmt')
                                                )
                                        ) %>%
                                        mutate(id = paste(parent, lbl))
                                ) %>%
                                filter(val > 0) %>%
                                group_by(id, parent, lbl, date, isInclCash, rtn_anlzed) %>%
                                summarise(
                                    val = sum(
                                        val,
                                        na.rm = T
                                    ),
                                    .groups = 'drop'
                                ) %>%
                                arrange(val) %>%
                                addIcon_pfmc()

                            hldgs_flat_df <<- hldgs_df %>%
                                filter(lbl %in% c(unique_tickers, 'Cash')) %>%
                                group_by(lbl, date, isInclCash) %>%
                                summarise(
                                    val = sum(val),
                                    rtn_anlzed = mean(
                                        rtn_anlzed,
                                        na.rm = T
                                    ),
                                    .groups = 'drop'
                                ) %>%
                                addIcon_pfmc() %>%
                                select(-lbl)

                            # calculate cash val dynamically depending on the selected fund
                            # because there are too many possible choices, it's difficult to pre calculate for all of them
                            # so we will calculate them dynamically
                            # calcHldgVals_tkrs <- function(FUNDS, INCL_CASH) {
                            #     r <- filter(hldgs_df, fund %in% FUNDS)

                            #     if (!INCL_CASH) r <- filter(r, tkr != 'Cash')

                            #     if (length(FUNDS) == 1) return(r)

                            #     r <- r %>%
                            #         group_by(tkr, date) %>%
                            #         summarise(val = sum(val))
                            # }

                            last_initDate <<- cd
                            message("Future: Data processing complete.")
                            # --- END: Data Loading & Processing Logic ---
                        },
                        error = function(e) {
                            # Update existing progress with error message
                            setProgress(
                                message = 'Initialization Failed!',
                                detail = HTML(paste('<span class="err-msg">', e$message, '</span>'))
                            )

                            # if we don't stop, then the code completes without error
                            # and the progress bar will close, and you won't see the message
                            stop(e)
                        }
                    )
                }
            ) # End withProgress

            cd
        }
    ) # End moduleServer
}
