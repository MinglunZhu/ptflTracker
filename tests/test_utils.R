# Test Utility Functions
source("R/00_utils.R")

# Test cleanTicker()
test_cleanTicker <- function() {
  test_cases <- list(
    "hkse:0001" = "0001.HK",
    "xpar:AIR" = "AIR.PA", 
    "fra:BMW" = "BMW.DE",
    "BRK/B" = "BRK-B"
  )
  
  for (input in names(test_cases)) {
    output <- cleanTicker(input)
    expected <- test_cases[[input]]
    cat(sprintf("Test %s => %s: %s\n",
              input,
              expected,
              ifelse(output == expected, "PASS", "FAIL")))
  }
}

# Test safeGetSymbols() (mock version)
test_safeGetSymbols <- function() {
  # Mock failing then succeeding download
  with_mock(
    `getSymbols` = function(...) {
      if (runif(1) > 0.5) stop("Mock error") else xts(1:10, Sys.Date()-10:1)
    },
    {
      result <- safeGetSymbols("TEST", MAX_ATTEMPTS = 2, DELAY = 0)
      cat("safeGetSymbols test:", ifelse(is.xts(result), "PASS", "FAIL"), "\n")
    }
  )
}

# Run tests
test_cleanTicker()
test_safeGetSymbols()