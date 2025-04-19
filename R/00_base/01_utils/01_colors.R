# consts
# make these color exclusive for performance indicators
# Reserved performance indicator colors - DO NOT USE IN REGULAR PALETTES
PFMC_GREEN <- "#1bffad"   # Neon Green
PFMC_RED <- "#ff3370"  # Neon Red
DFT_COLOR <- "rgba(128, 128, 128, 0.6)" # For 0, NA

# Similar color families are positioned far apart in the list to maximize separation
# no neutral because those turn red after saturation increase
CYBER_BASE_COLORS <- list(
    # "#b200ff",  # Neon Purple
    # "#9d00ff",  # Electric Purple
    # "#7700ff",  # Bright Purple
    # "#2b00ff",  # Electric Indigo
    # "#0080ff",  # Neon Blue
    # "#00b4ff",  # Electric Blue
    # "#00e5ff",  # Electric Cyan
    # "#00fff2",  # Bright Cyan
    
    # # Cyan -> Pink/Magenta
    # "#ff00f7",  # Hot Pink
    # "#ff00d4",  # Neon Pink
    # "#ff00aa",  # Electric Pink
    # "#ff0095",  # Cyber Pink
    
    # # Magenta -> Yellow/Orange
    # "#ffee00",  # Electric Yellow
    # "#ffd100",  # Cyber Gold
    # "#ff9500",  # Neon Orange
    # "#ff7300"   # Electric Orange

    # designed for close colors to be far from each other
    Magentas = c(
      "#FF00FF",  # Pure Magenta
      "#CC00CC",  # Dark Magenta
      "#FF33FF",  # Bright Magenta
      "#990099",  # Deep Tech Magenta
      "#FF66FF",  # Light Magenta
      "#EE33EE"   # Electric Magenta
    ),
  Blues_Cyans = c(
    "#00FFFF",  # Pure Cyan
    "#0033CC",  # Royal Tech Blue
    "#00E8E8",  # Electric Cyan
    "#0066FF",  # Bright Tech Blue
    "#00B8CC",  # Deep Cyan
    "#002299",  # Dark Tech Blue
    "#00D8E8",  # Tech Cyan
    "#0050A0",  # Deep Tech Blue
    "#00A8BB",  # Dark Cyan
    "#0044BB",  # Medium Tech Blue
    "#00C8DD",  # Medium Cyan
    "#0055DD"   # Electric Deep Blue
  ),
  Purples = c(
    "#7700FF",  # Bright Purple
    "#9D00FF",  # Neon Purple
    "#6600EE",  # Deep Purple
    "#8800DD",  # Tech Purple
    "#5500CC",  # Dark Purple
    "#7700EE"   # Electric Purple
  ),
  Oranges_Yellows = c(
    "#FF7300",  # Tech Orange
    "#FFEE00",  # Electric Yellow
    "#FF9500",  # Neon Orange
    "#FFD100",  # Cyber Gold
    "#FF8800",  # Electric Orange
    "#FFE200",  # Tech Yellow
    "#FFA500",  # Bright Orange
    "#FF7700",  # Deep Orange
    "#FF9900"   # Cyber Orange
  ),
  Pinks = c(
    "#FF00AA",  # Electric Pink
    "#FF0095",  # Cyber Pink
    "#FF0077",  # Deep Pink
    "#FF66AA",  # Bright Pink
    "#FF0088",  # Tech Pink
    "#FF55AA"   # Neon Pink
  )
)

MAX_COLOR_DIFF <- .1

MAX_HUE_ROTATION <- 5  # Maximum degrees to rotate
# end consts

# Convert RGB to hex color
rgb2hex <- function(R, G, B) {
    sprintf("#%02X%02X%02X", round(R * 255), round(G * 255), round(B * 255))
}

# Function to get color from wheel based on percentage
# color_len <- length(CYBER_BASE_COLORS)

# # returns a vector of RGB values
# getCyberColor <- function(pct) {
#     # Map percentage to color index (0% -> 1, 100% -> n_colors)
#     color_pos <- 1 + (pct * color_len)
    
#     # Get the two neighboring colors
#     lower_idx <- floor(color_pos)
#     upper_idx <- ceiling(color_pos)
    
#     if (lower_idx == upper_idx) return(col2rgb(CYBER_BASE_COLORS[color_pos]) / 255)

#     # Calculate mix ratio between the two colors
#     mix_ratio <- color_pos - lower_idx
    
#     # Get RGB values for both colors
#     rgb_lower <- col2rgb(CYBER_BASE_COLORS[lower_idx]) / 255
#     rgb_upper <- col2rgb(CYBER_BASE_COLORS[upper_idx]) / 255
    
#     # Interpolate between colors
#     rgb_lower * (1 - mix_ratio) + rgb_upper * mix_ratio
# }

#default_color_hex <- rgba_to_hex(default_color)

getLuminance <- function(RGB) {
  0.299 * RGB[1] + 0.587 * RGB[2] + 0.114 * RGB[3]
}

parseRgb <- function(RGB_STR) {
  (
    if (str_starts(RGB_STR, "rgba")) str_match(RGB_STR, "(\\d+),\\s*(\\d+),\\s*(\\d+),\\s*([0-9.]+)")[2:4]
    else col2rgb(RGB_STR)
  ) %>% as.numeric() / 255
}

adjContrast <- function(RGB, FCT, AMT) {
  rgb(RGB[1], RGB[2], RGB[3]) %>%
    FCT(AMT) %>%
    parseRgb()
}

names_colorGrps <- names(CYBER_BASE_COLORS)
len_colorGrps <- length(names_colorGrps)

# Color variation methods
colorVariationMethods <- list(
    # Method 3: Adaptive hue rotation
    function(COLOR, IDX, TTL) {
        current_hue <- clr_extract_hue(COLOR)
        
        # Calculate safe rotation amount based on neighboring colors
        base_rotation <- (IDX / TTL * MAX_HUE_ROTATION)
        rotation <- if (IDX %% 2 == 0) base_rotation else -base_rotation
        
        # Ensure rotation doesn't overlap with performance colors
        new_hue <- (current_hue + rotation) %% 360
        
        # Avoid red (0/360°) and green (120°) zones
        red_zone <- abs(new_hue - 0) < 20 || abs(new_hue - 360) < 20
        green_zone <- abs(new_hue - 120) < 20
        
        # Skip problematic zone by adding offset
        if (red_zone || green_zone) rotation <- rotation + if (red_zone) 25 else -25
        
        clr_rotate(
            COLOR, 
            degrees = rotation
        )
    },
    # Method 2: Adaptive saturation
    function(COLOR, IDX, TTL) {
        # extract return 0 - 100
        current_sat <- clr_extract_saturation(COLOR)
        adjPctg <- IDX / TTL

        # If already low saturation, increase it
        case_when(
            current_sat < 30 ~ {
                clr_saturate(
                    COLOR, 
                    shift = adjPctg
                )
            },
            current_sat > 70 ~ {
                # If high saturation, decrease it
                clr_desaturate(
                    COLOR,
                    shift =  0.6 * adjPctg
                )
            },
            IDX %% 2 == 0 ~ {
                clr_saturate(
                    COLOR,
                    shift = 0.8 * adjPctg
                )
            },
            T ~ {
                clr_desaturate(
                    COLOR,
                    shift = 0.5 * adjPctg
                )
            }

        )
    },
    # Method 1: Adaptive lightness
    function(COLOR_HEX, IDX, TTL) {
        current_lum <- clr_extract_lightness(COLOR_HEX) / 100
        
        # Calculate available room for lightness adjustment
        room_to_lighten <- 1 - current_lum
        adjPctg <- IDX / TTL
        
        # If very little room to lighten, reverse direction
        if (room_to_lighten < 0.3) {
            # Darken instead, using available room to darken
            amount <- (current_lum * 0.8) * adjPctg
            colorspace::darken(COLOR_HEX, amount)
        } else {
            # Lighten with available room
            amount <- room_to_lighten * 0.8 * adjPctg
            colorspace::lighten(COLOR_HEX, amount)
        }
    },
    # Method 4: Adaptive contrast
    function(COLOR_HEX, IDX, TTL) {
        current_lum <- clr_extract_lightness(COLOR_HEX) / 100
        current_sat <- clr_extract_saturation(COLOR_HEX) / 100
        adjPctg <- IDX / TTL
        
        # Create contrasting variations that maintain visibility
        if (current_lum > 0.5) {
            # Darker variations for light colors
            amount <- (current_lum * 0.7) * adjPctg
            new_color <- colorspace::darken(COLOR_HEX, amount)
        } else {
            # Lighter variations for dark colors
            amount <- ((1 - current_lum) * 0.7) * adjPctg
            new_color <- colorspace::lighten(COLOR_HEX, amount)
        }
        
        # Adjust saturation to maintain vibrancy
        if (current_sat < 0.3) {
            clr_saturate(
                new_color,
                shift = (.3 - current_sat) / (1 - current_sat) * adjPctg
            )
        } else {
            new_color
        }
    }
)

# color generation function
genCyberColors <- function(df) {
    # Start with root nodes (empty parent)
    df <- mutate(
      df, 
      lvl = if_else(parent == "", 0L, NA_integer_)
    )
    
    # Process nodes until all levels are assigned
    while (any(is.na(df$lvl))) {
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
    # 0 for the first node, 1 for the last, and evenly spaced in between.
    df <- df %>%
        group_by(parent) %>%
        mutate(
          nodeCnt = n(),
          pstnPctg = ifelse(nodeCnt == 1, 0, (row_number() - 1) / (nodeCnt - 1))
        ) %>%
        ungroup()

    # Generate colors with cyber theme
    max_level <- max(df$lvl)

    # Store final adjusted colors
    node_colors <- list()
    node_grps <- c()
    
    # Process nodes level by level
    for (current_level in 0:max_level) {
        # Get nodes at current level
        level_nodes <- filter(
            df, 
            lvl == current_level
        )
        #pctg_node <- MAX_COLOR_DIFF * ((current_level - 1) / (max_level - 1))
        
        for (i in 1:nrow(level_nodes)) {
            node <- level_nodes[i, ]

            # switch must use character if there is 0 or negative numbers
            c <- switch(
                as.character(current_level),
                '0' = DFT_COLOR,
                # lvl 1
                '1' = {
                    gn <- node_grps[node$id] <- names_colorGrps[((i - 1) %% len_colorGrps) + 1]

                    CYBER_BASE_COLORS[[gn]][1]
                },
                # lvl 2
                '2' = {
                    group_colors <- unlist(CYBER_BASE_COLORS[[ node_grps[node$parent] ]])

                    group_colors[((i - 1) %% length(group_colors)) + 1]
                },
                # default
                {
                    # Get sibling count for this parent
                    siblings_df <- filter(
                        level_nodes, 
                        parent == node$parent
                    )
                    
                    siblingsCnt <- nrow(siblings_df)
                    pc <- node_colors[[node$parent]]
                    pc_hex <- rgb(pc[1], pc[2], pc[3])

                    if (siblingsCnt <= 1) pc_hex
                    else {
                        # # Generate variation
                        colorVariationMethods[[((current_level - 3) %% length(colorVariationMethods)) + 1]](
                        #colorVariationMethods[[1]](
                            pc_hex, 
                            which(siblings_df$id == node$id), 
                            siblingsCnt
                        )
                    }
                }
            ) %>% parseRgb()

            # Add a slight glow effect by boosting the dominant channel
            max_channel <- which.max(c)
            c[max_channel] <- min(1, c[max_channel] * 1.2)

            # make text readable
            lum_c <- getLuminance(c)
            lum_text <- parseRgb(node$color_bdr) %>% getLuminance()

            if (abs(lum_text - lum_c) < 0.5) {
                fct_adj <- if (lum_text > .5) darken else lighten

                rslt <- uniroot(
                    function(amt) { abs(adjContrast(c, fct_adj, amt) %>% getLuminance() - lum_text) - .5 },
                    lower = 0,
                    upper = 1,
                    tol = 1e-2
                )

                c <- adjContrast(c, fct_adj, rslt$root)
            }

            node_colors[[node$id]] <- c
        }
    }

    # level adjustements
    mapply(
      function(lvl, id) {
        rgb_adjed <- node_colors[[id]] *
            # Apply adjustments
            # Modify the color based on level
            # Create a slightly glowing effect by adjusting saturation
            # Increase saturation with depth
            (1 - MAX_COLOR_DIFF) + (lvl / max_level) * MAX_COLOR_DIFF
        
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

alpha_hex_10pct <- "1A"

genLightHex <- function(COLOR) {
  colorspace::mixcolor(0.7, colorspace::hex2RGB(COLOR), colorspace::RGB(1,1,1)) %>%
    colorspace::hex() %>%
    paste0(alpha_hex_10pct)
}

light_green <- genLightHex(PFMC_GREEN)
light_red <- genLightHex(PFMC_RED)

#light_red <- colorspace::mixcolor(0.7, colorspace::hex2RGB(red), colorspace::RGB(1,1,1)) %>% colorspace::hex()

genCyberColors_pfmc <- function(RTNS, MAX, MIN) {
  # Create separate palettes
  green_palette <- scales::col_numeric(
      palette = c(light_green, PFMC_GREEN),
      # Add small epsilon to avoid zero-range domains if only one value exists
      domain = if (MAX > 0) c(0, MAX + 1e-9)
        else c(0, 1e-9),
      na.color = DFT_COLOR
  )
  red_palette <- scales::col_numeric(
      palette = c(PFMC_RED, light_red),
      domain = if (MIN < 0) c(MIN - 1e-9, 0)
        else c(-1e-9, 0),
      na.color = DFT_COLOR
  )

  # it's generating warnings because case_when puts the entire vector through the functions
  # not just the ones meeting the condition
  # but that's ok
  case_when(
    RTNS > 0 ~ green_palette(RTNS),
    RTNS < 0 ~ red_palette(RTNS),
    .default = DFT_COLOR
  )
}