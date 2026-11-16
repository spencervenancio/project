# ---------------------------------------------------------------------------
# Stat 479/679 Fall 2026 -- Final Project: "Your 30 Rivers"
#
# Hands you YOUR personal subset of the HCDN stream gauge network, determined by
# your 10-digit UW campus ID. Do not use anyone else's.
#
#   source("get_my_sites.R")
#   dat <- get_my_sites("0123456789")     # <-- your campus ID, in quotes
#   dat                                   # prints your dataset code
#
# Put the DATASET CODE that this prints, not your campus ID, on your slides.
#
# Requires HCDN_annual_max.RData in the working directory (posted on Canvas).
# ---------------------------------------------------------------------------

## R seeds must fit in a signed 32-bit integer (max 2,147,483,647), and a
## 10-digit campus ID does not: set.seed() would coerce it to NA. This folds any
## digit string down to a legal seed. Sequential IDs still give uncorrelated
## datasets, because R's Mersenne Twister scrambles the seed before use.
dataset_code <- function(id) {
  s <- gsub("[^0-9]", "", paste(format(id, scientific = FALSE, trim = TRUE),
                                collapse = ""))
  if (!nchar(s))
    stop("No digits found in '", id, "'. Pass your campus ID as a string.",
         call. = FALSE)
  ## Leading zeros are absorbed by the fold, so losing them to an unquoted
  ## literal is harmless. A short ID means a genuine mistyping.
  if (nchar(s) < 8)
    warning("'", id, "' has only ", nchar(s), " digits. A UW campus ID has 10. ",
            "Check that you typed yours correctly, or you will analyze the ",
            "wrong rivers.", call. = FALSE)
  h <- 0
  for (x in as.integer(strsplit(s, "")[[1]])) h <- (h * 31 + x) %% 2147483647
  as.integer(h)
}

## The raw HUC02 region codes are inconsistently zero-padded: "2" and "02" are
## the same region, so a naive grouping would split 19 real regions into 25.
## This normalizes them. "10L" and "10U" are genuinely distinct (lower and
## upper Missouri) and are left alone.
normalize_region <- function(h) {
  h   <- toupper(trimws(as.character(h)))
  num <- suppressWarnings(as.integer(h))
  ifelse(is.na(num), h, sprintf("%02d", num))
}

get_my_sites <- function(id, k = 30, path = "HCDN_annual_max.RData") {

  if (missing(id) || length(id) != 1)
    stop("Pass your campus ID as a single quoted string, e.g. \"0123456789\".",
         call. = FALSE)
  seed <- dataset_code(id)
  if (!file.exists(path))
    stop("Cannot find ", path, ". Download it from Canvas into ", getwd(),
         call. = FALSE)

  e <- new.env(); load(path, envir = e)

  n_years <- rowSums(!is.na(e$Y))
  has_zero <- apply(e$Y, 1, function(y) any(y == 0, na.rm = TRUE))

  ## Only well-observed gauges, so nobody gets an impossible record.
  eligible <- which(n_years >= 50)

  ## Every student gets exactly one gauge that records a zero annual maximum,
  ## an intermittent stream that ran dry.
  dry <- intersect(eligible, which(has_zero))

  set.seed(seed)
  must <- sample(dry, 1)
  rest <- sample(setdiff(eligible, must), k - 1)
  idx  <- sort(c(must, rest))

  out <- list(
    Y      = e$Y[idx, , drop = FALSE],   # k x 72 annual maximum streamflow (cfs)
    year   = e$year,                     # 1950 ... 2021
    site   = e$ID[idx],                  # USGS gauge id
    drain  = e$drain[idx],               # drainage area (sq mi)
    region = normalize_region(e$HUC02[idx]),
    lon    = e$s$LONG_GAGE[idx],
    lat    = e$s$LAT_GAGE[idx],
    code   = seed          # your dataset code; put THIS on your slides
  )
  rownames(out$Y) <- out$site
  colnames(out$Y) <- out$year
  class(out) <- "myrivers"
  out
}

print.myrivers <- function(x, ...) {
  cat("Your rivers\n")
  cat("  DATASET CODE     :", x$code, " <- put this on your slides\n")
  cat("  sites            :", length(x$site), "\n")
  cat("  years            :", min(x$year), "-", max(x$year), "\n")
  cat("  regions          :", length(unique(x$region)),
      paste0("(", paste(sort(unique(x$region)), collapse = ", "), ")"), "\n")
  cat("  observations     :", sum(!is.na(x$Y)),
      sprintf("(%.1f%% missing)", 100 * mean(is.na(x$Y))), "\n")
  cat("  drainage area    :", sprintf("%.1f to %.0f sq mi", min(x$drain), max(x$drain)), "\n")
  cat("  annual max range :", sprintf("%.2f to %.0f cfs", min(x$Y, na.rm = TRUE),
                                      max(x$Y, na.rm = TRUE)), "\n")
  invisible(x)
}
