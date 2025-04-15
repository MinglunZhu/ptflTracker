# Utility functions
# consts
RCDS_DIR <- 'rcds/prices/'

# Arranged in color wheel progression
CYBER_BASE_COLORS <- c(
    "#00fff2",  # Cyan
    "#00b4ff",  # Electric Blue
    "#7700ff",  # Purple
    "#ff00f7"  # Hot Pink
)

# make these color exclusive for performance indicators
CYBER_PFMC_COLORS <- c(
  "#1bffad",   # Neon Green (cycles back towards cyan)
  "#ff3370"  # Neon Red
)

MAX_COLOR_DIFF <- .1
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

read_xts <- function(fp) {
  read_csv(fp) %>%
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

# Convert RGB to hex color
rgb2hex <- function(r, g, b) {
    sprintf("#%02X%02X%02X", round(r * 255), round(g * 255), round(b * 255))
}

# Function to get color from wheel based on percentage
# Subtract small epsilon to ensure 100% maps to last color
color_len <- length(CYBER_BASE_COLORS) - 1.000001

# returns a vector of RGB values
getCyberColor <- function(pct) {
    # Map percentage to color index (0% -> 1, 100% -> n_colors)
    color_pos <- 1 + (pct * color_len)
    
    # Get the two neighboring colors
    lower_idx <- floor(color_pos)
    upper_idx <- ceiling(color_pos)
    
    # Calculate mix ratio between the two colors
    mix_ratio <- color_pos - lower_idx
    
    # Get RGB values for both colors
    rgb_lower <- col2rgb(CYBER_BASE_COLORS[lower_idx]) / 255
    rgb_upper <- col2rgb(CYBER_BASE_COLORS[upper_idx]) / 255
    
    # Interpolate between colors
    rgb_lower * (1 - mix_ratio) + rgb_upper * mix_ratio
}

# color generation function
genCyberColors <- function(df) {
    # Start with root nodes (empty parent)
    df <- df %>%
        mutate(lvl = if_else(parent == "", 0L, NA_integer_))
    
    # Process nodes until all levels are assigned
    while(any(is.na(df$lvl))) {
      df <- df %>%
        mutate(
            # Find nodes whose parents have levels assigned
            lvl_parent = lvl[match(parent, id)],

            # Assign level to nodes whose parents have levels
            lvl = if_else(
                is.na(lvl) 
                & !is.na(lvl_parent),
                lvl_parent + 1,
                lvl
            )
        )
    }
    
    # Calculate percentage within each level
    df <- df %>%
        group_by(parent) %>%
        mutate(pctg = val / sum(val)) %>%
        ungroup()

    # Generate colors with cyber theme
    max_level <- max(df$lvl)

    # Store final adjusted colors
    node_colors <- list()
    
    # Process nodes level by level
    for (current_level in 0:max_level) {
        # Get nodes at current level
        level_nodes <- filter(df, lvl == current_level)
        
        for (i in 1:nrow(level_nodes)) {
            node <- level_nodes[i, ]
            
            # Get base color from wheel
            rgb_node <- getCyberColor(node$pctg)
            rgb_parent <- node_colors[[node$parent]]

            node_colors[[node$id]] <- (
              if (node$lvl <= 1) rgb_node
              # Blend with parent's adjusted color
              else rgb_parent + (rgb_node - rgb_parent) * MAX_COLOR_DIFF * ((node$lvl - 1) / (max_level - 1))
            ) * node$pctg
        }
    }

    mapply(
      function(lvl, id) {
        rgb_adjed <- node_colors[[id]] *
          # Apply adjustments
          # Modify the color based on level
          # Create a slightly glowing effect by adjusting saturation
          # Increase saturation with depth
          (1 - MAX_COLOR_DIFF) + (lvl / max_level) * MAX_COLOR_DIFF
        
        # Add a slight glow effect by boosting the dominant channel
        max_channel <- which.max(rgb_adjed)
        rgb_adjed[max_channel] <- min(1, rgb_adjed[max_channel] * 1.2)
        
        # Always return rgba format since we're using alpha
        sprintf(
          "rgba(%d, %d, %d, %f)", 
          round(rgb_adjed[1] * 255),
          round(rgb_adjed[2] * 255),
          round(rgb_adjed[3] * 255),
          # Make portfolio level (level 0) hollow by setting alpha to 0
          lvl / max_level
        )
      }, 
      df$lvl, df$id
    )
}

# Function to convert an "rgba(r, g, b, a)" string to "#RRGGBBAA" hex format
rgba_to_hex <- function(rgba_string) {
  # Extract numeric values using regex
  vals <- regmatches(rgba_string, gregexpr("[0-9.]+", rgba_string))[[1]]
  
  # Check if we got 4 values
  if (length(vals) != 4) {
    warning(paste("Could not parse 4 values from:", rgba_string))
    return(NA_character_) 
  }
  
  # Convert to numeric
  r <- as.numeric(vals[1])
  g <- as.numeric(vals[2])
  b <- as.numeric(vals[3])
  a <- as.numeric(vals[4]) # Alpha is typically 0-1
  
  # Check for valid ranges (optional but good practice)
  if (anyNA(c(r, g, b, a)) || 
      any(c(r, g, b) < 0 | c(r, g, b) > 255) || 
      a < 0 || a > 1) {
     warning(paste("Invalid RGBA values parsed:", rgba_string))
     return(NA_character_)
  }

  # Use base rgb function to convert to hex with alpha
  rgb(r, g, b, alpha = a * 255, maxColorValue = 255) 
}

green <- CYBER_PFMC_COLORS[1]
red <- CYBER_PFMC_COLORS[2]
default_color <- "rgba(128, 128, 128, 0.1)" # For 0, NA
#default_color_hex <- rgba_to_hex(default_color)

alpha_hex_10pct <- "1A"

genLightHex <- function(COLOR) {
  colorspace::mixcolor(0.7, colorspace::hex2RGB(COLOR), colorspace::RGB(1,1,1)) %>%
    colorspace::hex() %>%
    paste0(alpha_hex_10pct)
}

light_green <- genLightHex(green)
light_red <- genLightHex(red)

#light_red <- colorspace::mixcolor(0.7, colorspace::hex2RGB(red), colorspace::RGB(1,1,1)) %>% colorspace::hex()

genCyberColors_pfmc <- function(RTNS, MAX, MIN) {
  # Create separate palettes
  green_palette <- scales::col_numeric(
      palette = c(light_green, green),
      # Add small epsilon to avoid zero-range domains if only one value exists
      domain = if (MAX > 0) c(0, MAX + 1e-9)
        else c(0, 1e-9),
      na.color = default_color
  )
  red_palette <- scales::col_numeric(
      palette = c(red, light_red),
      domain = if (MIN < 0) c(MIN - 1e-9, 0)
        else c(-1e-9, 0),
      na.color = default_color
  )

  # it's generating warnings because case_when puts the entire vector through the functions
  # not just the ones meeting the condition
  # but that's ok
  case_when(
    RTNS > 0 ~ green_palette(RTNS),
    RTNS < 0 ~ red_palette(RTNS),
    .default = default_color
  )
}