# =============================================================================
# 01_collect_data.R
# FE Final Project: state-dependent spillovers from US Treasury yields to
# Korean government bond yields (see docs/조건별전이_연구개요_확정.md, section 2).
#
# Builds data/Data_team.csv: one row per calendar day on which Korea or the US
# traded, with raw daily LEVELS of every series. Derived variables (shock s,
# target Δy, gaps, z-scores, volatility) are built in the analysis code.
#
# In the submitted single R file this whole script goes inside
#   if (FALSE) { ... }
# so the TA can start from the csv without API keys.
#
# Run from the "Final Project" folder:
#   Rscript code/01_collect_data.R
#
# Requirements
#   - ECOS API key in the environment variable ECOS_API_KEY
#     (free: https://ecos.bok.or.kr/api/). Put it in ~/.Renviron, never in git.
#   - Manual files in data_raw/ (optional; missing files become NA columns):
#       original/krx_futures/fut3_YYYY.csv, fut10_YYYY.csv : KRX downloads as-is
#                             (foreign net buying of 3y / 10y KTB futures, contracts)
#       original/krx_vkospi/vkospi_YYYY.csv : KRX VKOSPI downloads as-is (close)
#       fgn_share.csv       : pub_date, fgn_share (built by data_raw/fss_fgn_share.py)
#                             (foreign share of listed bonds, %, by publication date,
#                              YYYY-MM-DD)
#
# Optional overrides (for quick tests): COLLECT_START, COLLECT_END (YYYY-MM-DD),
# ECOS_PAGE_SIZE (rows per ECOS request; use 10 with the "sample" key).
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(readxl)
  library(rvest)
})

# ---- settings -----------------------------------------------------------------
# Collect from 2009 so 250-day z-scores and 20-day volatility exist on 2010-01-04.
START <- as.Date(Sys.getenv("COLLECT_START", "2009-01-01"))
END   <- as.Date(Sys.getenv("COLLECT_END",   "2026-10-30"))
ECOS_KEY  <- Sys.getenv("ECOS_API_KEY")
ECOS_PAGE <- as.integer(Sys.getenv("ECOS_PAGE_SIZE", "10000"))
UA <- user_agent("Mozilla/5.0 (FE final project data collection)")
UA_FRED <- user_agent("curl/8.7.1")   # FRED rejects browser-like agents from scripts

OUT_DIR <- "data"
RAW_DIR <- "data_raw"
OUT_CSV <- file.path(OUT_DIR, "Data_team.csv")   # rename to Data_<team number>.csv

if (ECOS_KEY == "") stop("Set ECOS_API_KEY (see header).")
dir.create(OUT_DIR, showWarnings = FALSE)

fmt <- function(d, f = "%Y%m%d") format(d, f)

# ---- ECOS (Bank of Korea) ------------------------------------------------------
# One item of one daily table, paged. Returns data.frame(date, value).
ecos_get <- function(stat, item, name) {
  base <- "https://ecos.bok.or.kr/api/StatisticSearch"
  rows <- list(); from <- 1
  repeat {
    to  <- from + ECOS_PAGE - 1
    url <- sprintf("%s/%s/json/kr/%d/%d/%s/D/%s/%s/%s",
                   base, ECOS_KEY, from, to, stat, fmt(START), fmt(END), item)
    res <- RETRY("GET", url, UA, times = 3, timeout(60))
    js  <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (is.null(js$StatisticSearch)) {
      msg <- js$RESULT$MESSAGE
      if (!is.null(js$RESULT$CODE) && js$RESULT$CODE == "INFO-200") break  # no data
      stop(sprintf("ECOS %s/%s: %s", stat, item, msg))
    }
    rows[[length(rows) + 1]] <- js$StatisticSearch$row[, c("TIME", "DATA_VALUE")]
    total <- js$StatisticSearch$list_total_count
    if (to >= total) break
    from <- to + 1
  }
  d <- do.call(rbind, rows)
  out <- data.frame(date  = as.Date(d$TIME, "%Y%m%d"),
                    value = suppressWarnings(as.numeric(d$DATA_VALUE)))
  names(out)[2] <- name
  out
}

# ---- FRED (no key needed for the csv endpoint) --------------------------------
fred_get <- function(id, name) {
  url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s&cosd=%s&coed=%s",
                 id, fmt(START, "%Y-%m-%d"), fmt(END, "%Y-%m-%d"))
  res <- RETRY("GET", url, UA_FRED, times = 3, timeout(60))
  stop_for_status(res)
  d <- read.csv(text = content(res, "text", encoding = "UTF-8"),
                na.strings = c(".", ""), stringsAsFactors = FALSE)
  out <- data.frame(date = as.Date(d[[1]]), value = as.numeric(d[[2]]))
  names(out)[2] <- name
  out
}

# ---- ACM 10-year term premium (New York Fed, daily sheet) ---------------------
acm_get <- function() {
  url <- "https://www.newyorkfed.org/medialibrary/media/research/data_indicators/ACMTermPremium.xls"
  tmp <- tempfile(fileext = ".xls")
  RETRY("GET", url, UA, times = 3, timeout(120), write_disk(tmp, overwrite = TRUE))
  d <- read_excel(tmp, sheet = "ACM Daily")
  # DATE looks like "12-Nov-2026"; parse month names without relying on locale
  p <- strsplit(d$DATE, "-")
  date <- as.Date(sprintf("%s-%02d-%s", sapply(p, `[`, 3),
                          match(sapply(p, `[`, 2), month.abb), sapply(p, `[`, 1)))
  out <- data.frame(date = date, tp_acm = as.numeric(d$ACMTP10))
  out[out$date >= START & out$date <= END, ]
}

# ---- FOMC statement dates (Federal Reserve website) ---------------------------
# Recent years: fomccalendars.htm. Older years: fomchistoricalYYYY.htm.
# Keeps scheduled and unscheduled meetings; drops notation votes and cancelled
# meetings. The statement date is the last day of the meeting.
fomc_dates <- function(years) {
  month_num <- function(m) match(tolower(substr(m, 1, 3)), tolower(month.abb))
  last_day <- function(year, month_txt, day_txt) {
    m <- tail(strsplit(month_txt, "/")[[1]], 1)
    d <- tail(regmatches(day_txt, gregexpr("[0-9]+", day_txt))[[1]], 1)
    as.Date(sprintf("%d-%02d-%02d", year, month_num(m), as.integer(d)))
  }
  out <- as.Date(character())

  cal <- read_html(RETRY("GET", "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm",
                         UA, times = 3, timeout(60)))
  cal_years <- integer()
  for (p in html_elements(cal, ".panel.panel-default")) {
    y <- as.integer(regmatches(html_text2(html_element(p, ".panel-heading")),
                               regexpr("[0-9]{4}", html_text2(html_element(p, ".panel-heading")))))
    if (is.na(y)) next
    cal_years <- c(cal_years, y)
    if (!y %in% years) next
    for (m in html_elements(p, ".fomc-meeting")) {
      mon <- html_text2(html_element(m, ".fomc-meeting__month"))
      day <- html_text2(html_element(m, ".fomc-meeting__date"))
      if (grepl("notation|cancel", day, ignore.case = TRUE)) next
      out <- c(out, last_day(y, mon, gsub("\\([^)]*\\)|\\*", "", day)))
    }
  }

  for (y in setdiff(years, cal_years)) {
    url <- sprintf("https://www.federalreserve.gov/monetarypolicy/fomchistorical%d.htm", y)
    pg  <- read_html(RETRY("GET", url, UA, times = 3, timeout(60)))
    for (h in html_text2(html_elements(pg, "h5"))) {
      if (!grepl("Meeting", h) || grepl("cancel|notation", h, ignore.case = TRUE)) next
      txt <- trimws(sub("Meeting.*$", "", gsub("\\([^)]*\\)", "", h)))   # "January 27-28"
      mons <- regmatches(txt, gregexpr("[A-Za-z]+", txt))[[1]]
      out <- c(out, last_day(y, tail(mons, 1), txt))
    }
    Sys.sleep(0.5)
  }
  # 2020-03-02 unscheduled call: the statement was released on 2020-03-03
  out[out == as.Date("2020-03-02")] <- as.Date("2020-03-03")
  sort(unique(out))
}

# ---- Monetary Policy Committee decision dates (Bank of Korea website) ---------
mpc_dates <- function(years) {
  out <- as.Date(character())
  for (y in years) {
    url <- sprintf("https://www.bok.or.kr/portal/singl/crncyPolicyDrcMtg/listYear.do?mtgSe=A&menuNo=200755&pYear=%d", y)
    txt <- content(RETRY("GET", url, UA, times = 3, timeout(60)), "text", encoding = "UTF-8")
    md  <- regmatches(txt, gregexpr("[0-9]{2}월 [0-9]{2}일\\(", txt))[[1]]
    if (length(md) == 0) warning("No MPC dates found for ", y)
    mm <- as.integer(substr(md, 1, 2)); dd <- as.integer(substr(md, 5, 6))
    out <- c(out, as.Date(sprintf("%d-%02d-%02d", y, mm, dd)))
    Sys.sleep(0.5)
  }
  sort(unique(out))
}

# ---- manual files ----------------------------------------------------------------
# KRX downloads saved unchanged as data_raw/original/krx_*/<prefix>_<start year>.csv (CP949,
# first column 일자 "YYYY/MM/DD"). Stacks every chunk and keeps one column:
#   futures (fut3, fut10): 외국인 합계 = foreign daily net buying, contracts, regular session
#   vkospi             : 종가 = daily close
read_krx <- function(prefix, name, col) {
  fs <- list.files(file.path(RAW_DIR, "original"), sprintf("^%s_[0-9]{4}\\.csv$", prefix),
                   full.names = TRUE, recursive = TRUE)   # krx_futures/, krx_vkospi/
  if (length(fs) == 0) return(NULL)
  d <- do.call(rbind, lapply(fs, function(f) {
    x <- read.csv(f, fileEncoding = "CP949", check.names = FALSE)
    if (!col %in% names(x)) stop(f, ": no column ", col)
    data.frame(date = as.Date(x[[1]], "%Y/%m/%d"), value = as.numeric(x[[col]]))
  }))
  if (anyDuplicated(d$date)) stop(prefix, ": overlapping download periods")
  names(d)[2] <- name
  d[order(d$date), ]
}

read_manual <- function(file, cols) {
  path <- file.path(RAW_DIR, file)
  if (!file.exists(path)) {
    warning(path, " not found: columns ", paste(cols, collapse = ", "), " will be NA")
    return(NULL)
  }
  d <- read.csv(path, stringsAsFactors = FALSE)
  miss <- setdiff(c(names(d)[1], cols), names(d))
  if (length(miss)) stop(path, " is missing columns: ", paste(miss, collapse = ", "))
  d[[1]] <- as.Date(d[[1]])
  d[, c(names(d)[1], cols)]
}

# ---- collect --------------------------------------------------------------------
message("ECOS ...")
ecos <- list(
  ecos_get("817Y002", "010200000", "kr3y"),       # KTB 3y
  ecos_get("817Y002", "010210000", "kr10y"),      # KTB 10y
  ecos_get("817Y002", "010190000", "kr1y"),       # KTB 1y
  ecos_get("817Y002", "010502000", "cd91"),       # CD 91d
  ecos_get("722Y001", "0101000",   "base_rate"),  # BOK base rate
  # KRW/USD 15:30 close. Not 731Y001 (매매기준율): that rate is set from the previous
  # day's trades, so it lags the market by one day.
  ecos_get("731Y003", "0000003",   "krw_usd"),    # KRW/USD close 15:30
  ecos_get("802Y001", "0001000",   "kospi")       # KOSPI
)

message("FRED ...")
fred <- list(
  fred_get("DGS2",        "us2y"),
  fred_get("DGS3",        "us3y"),        # robustness R1 (same-maturity shock for KTB 3y)
  fred_get("DGS5",        "us5y"),        # robustness R1
  fred_get("DGS10",       "us10y"),
  fred_get("DGS3MO",      "us3m"),
  fred_get("DFF",         "effr"),        # value for day d is published on d+1 (see doc)
  fred_get("VIXCLS",      "vix"),
  fred_get("DTWEXBGS",    "usd_idx"),
  fred_get("BAA10Y",      "baa_spread"),
  fred_get("THREEFYTP10", "tp_kw"),
  fred_get("NASDAQCOM",   "nasdaq")       # shock type (Hoek, Kamin & Yoldas 2022)
)

message("ACM ...")
acm <- acm_get()

message("FOMC and MPC dates ...")
yrs  <- as.integer(format(START, "%Y")):as.integer(format(END, "%Y"))
fomc <- fomc_dates(yrs)
mpc  <- mpc_dates(yrs)
fomc <- fomc[fomc >= START & fomc <= END]
mpc_all <- mpc                                   # keeps scheduled meetings after END
mpc  <- mpc[mpc >= START & mpc <= END]

message("Manual files ...")
krx_files <- list(fut3 = c("fut3_net", "외국인 합계"), fut10 = c("fut10_net", "외국인 합계"),
                  vkospi = c("vkospi", "종가"))
krx <- Filter(Negate(is.null), lapply(names(krx_files), function(p) {
  d <- read_krx(p, krx_files[[p]][1], krx_files[[p]][2])
  if (is.null(d)) warning("No data_raw/original/", p, "_*.csv files: ",
                          krx_files[[p]][1], " will be NA")
  d
}))
fgn <- read_manual("fgn_share.csv", "fgn_share")

# ---- merge on the union of Korean and US trading days --------------------------
series <- c(ecos, fred, list(acm), krx)
df <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), series)
df <- df[df$date >= START & df$date <= END, ]

# Korean trading day = KTB quote AND KOSPI close. On the exchange's year-end
# holiday (last business day of December) ECOS still posts a stale KTB quote
# (change ~0) but no KOSPI, so those days count as closed.
df$kr_open <- as.integer(!is.na(df$kr3y) & !is.na(df$kospi))
df$us_open <- as.integer(!is.na(df$us10y))
df <- df[df$kr_open == 1 | df$us_open == 1, ]     # drops weekends and joint holidays

# Base rate is published for every calendar day; carry it to any remaining gap
locf <- function(x) { i <- cummax(seq_along(x) * !is.na(x)); x[i[i > 0]] -> v; c(rep(NA, sum(i == 0)), v) }
df$base_rate <- locf(df$base_rate)

# Monthly foreign share: value in force = latest publication on or before each date
if (!is.null(fgn)) {
  fgn <- fgn[order(fgn$pub_date), ]
  i <- findInterval(df$date, fgn$pub_date)
  df$fgn_share <- ifelse(i > 0, fgn$fgn_share[pmax(i, 1)], NA)
}

for (v in c("fut3_net", "fut10_net", "vkospi", "fgn_share"))
  if (!v %in% names(df)) df[[v]] <- NA_real_

# KRX data error: VKOSPI 2019-04-10 closes at 30.40 (open 12.91, prior and next close
# 12.67 / 12.29) while KOSPI and VIX are flat. Raw file kept as downloaded; value set NA.
df$vkospi[df$date == as.Date("2019-04-10")] <- NA

# FOMC action on a US non-trading day (2020-03-15, Sunday emergency cut): markets first
# react on the next US trading day, so the dummy goes there.
us_days <- df$date[df$us_open == 1]
fomc <- sort(unique(as.Date(sapply(fomc, function(f) min(us_days[us_days >= f])))))
df$fomc_day <- as.integer(df$date %in% fomc)   # US date of the FOMC statement
df$mpc_day  <- as.integer(df$date %in% mpc)    # Korean date of the MPC decision
# Next MPC decision strictly after each date (for days_to_mpc); may fall after END
df$next_mpc <- mpc_all[findInterval(df$date, mpc_all) + 1]

cols <- c("date", "kr_open", "us_open",
          "kr3y", "kr10y", "us2y", "us3y", "us5y", "us10y",         # targets, shocks
          "kr1y", "cd91", "base_rate", "effr", "us3m",              # H1, H2 inputs
          "fut3_net", "fut10_net", "fgn_share",                     # H2
          "vix", "krw_usd", "vkospi", "usd_idx", "baa_spread",      # H3
          "tp_kw", "tp_acm", "kospi", "nasdaq",                     # controls
          "fomc_day", "mpc_day", "next_mpc")
df <- df[order(df$date), cols]

# ---- checks ---------------------------------------------------------------------
stopifnot(!anyDuplicated(df$date), !is.unsorted(df$date))
stopifnot(all(df$kr3y > -1 & df$kr3y < 20, na.rm = TRUE),
          all(df$us10y > -1 & df$us10y < 20, na.rm = TRUE))
if (any(df$fomc_day == 1 & df$us_open == 0)) warning("FOMC date on a US non-trading day")
if (any(df$mpc_day == 1 & df$kr_open == 0))  warning("MPC date on a Korean non-trading day")
if (anyNA(df$next_mpc)) warning("next_mpc missing: MPC schedule ends before some dates")

cover <- data.frame(
  variable = cols[-1],
  first    = sapply(cols[-1], function(v) as.character(df$date[which(!is.na(df[[v]]))[1]])),
  na_share = round(sapply(cols[-1], function(v) mean(is.na(df[[v]]))), 3)
)
print(cover, row.names = FALSE)
message(sprintf("FOMC dates: %d, MPC dates: %d", length(fomc), length(mpc)))

write.csv(df, OUT_CSV, row.names = FALSE)
message("Wrote ", OUT_CSV, " (", nrow(df), " rows)")
