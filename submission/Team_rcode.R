# =============================================================================
# Team_rcode.R  (FE Final Project, single submission file)
# Part 1 (inside if (FALSE)): data collection. Needs an ECOS API key and the manual
#   KRX/FSS files, so it is not run by default. It produced the submitted csv.
# Part 2: all analyses, starting from the csv. Put the csv in the working directory.
# =============================================================================

if (FALSE) {
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
}  # end of data collection

# =============================================================================
# 02_analysis.R
# FE Final Project: state-dependent spillovers from US Treasury yields to
# Korean government bond yields. Implements section 4 of
# docs/조건별전이_연구개요_확정.md on Data_team.csv.
#
# Run from the "Final Project" folder:
#   Rscript code/02_analysis.R            # everything
#   STAGE=1 Rscript code/02_analysis.R    # 1st-round analyses only (4-1..4-5 M0-M2, 4-7)
#
# Outputs: output/Team_result.RData (rename to <team>_result.RData),
#          output/tables/*.csv, output/figures/*.png
#
# Decisions fixed before estimation (see docs/조건별전이_데이터전처리.md and the data review):
#   - All tests are two-sided. The outline's predicted signs are reported next to
#     each estimate, but a significant opposite sign is read as evidence, not a null.
#   - Main hypothesis model adds trend and s x trend (trend = standardized date).
#     The spillover beta trends up over the sample, so slow-moving conditions would
#     otherwise pick up the trend. The no-trend model is reported alongside.
#   - Forecasting models (4-5) do not use the trend: a 1,000-day window does not
#     support extrapolating it.
#   - Variables follow docs/조건별전이_변수확정_선행연구.md: 3 representatives (H1-H3),
#     8 LASSO candidates, and the shock type mon (Hoek, Kamin & Yoldas 2022). mon = 1 when
#     the NASDAQ return over the same US days as s has the opposite sign to s.
#   - Design v6 (docs/조건별전이_연구설계_통합안.md) takes precedence:
#     part 1 = pre-registered model WITHOUT mon, specs A (trend) and B (no trend), plus
#     C/D with mon as robustness (4-3a); part 3 = N2 FLAT, N3 GARCH-X by shock type,
#     N5 M4 vs M1, N7 M4 vs M2 (no mon in M1/M2), Holm over the 8 tests.
#     The older 4-3 tables (with mon) and R1-R11 (spec C base) are kept for reference.
# =============================================================================

suppressPackageStartupMessages({
  library(sandwich); library(lmtest); library(zoo)
  library(urca); library(tseries); library(glmnet)
  library(strucchange); library(rugarch); library(ggplot2)
})

# ---- 0. settings -----------------------------------------------------------------
set.seed(2026)
STAGE   <- as.integer(Sys.getenv("STAGE", "3"))   # 1, 2 or 3 (see 4-0)
L_NW    <- 9                                       # Newey-West lag: floor(4(T/100)^(2/9))
W       <- 1000                                    # rolling window (pairs)
STEP_H  <- 20                                      # heatmap / M3 refit step
START   <- as.Date("2010-01-04")
START10 <- as.Date("2012-05-31")                   # fut10 z-score available (outline 2-5)
DATA    <- "Data_team.csv"
OUT     <- "output"
dir.create(file.path(OUT, "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT, "figures"), recursive = TRUE, showWarnings = FALSE)
res <- list(settings = list(L_NW = L_NW, W = W, STEP_H = STEP_H, START = START, START10 = START10))

TARGETS <- list(k3 = c(dy = "kr3y", s = "us2y"), k10 = c(dy = "kr10y", s = "us10y"))
REPS    <- c(H1 = "abs_kr1y_gap", H2 = "fut3_cum", H3 = "vix")
PRED    <- c(H1 = "+", H2 = "+ (rising US yields)", H3 = "+")
CANDS   <- c("abs_kr1y_gap", "policy_gap",                                     # H1
             "fut3_cum", "fut10_cum", "fgn_share",                             # H2
             "vix", "baa_spread", "krw_chg20")                                 # H3
ROB_ONLY <- c("kr1y_gap", "fx_vol", "st_spread")                               # R7, R8 only
DUMMIES <- c("hol", "nous", "fomc_d", "mpc_d")

save_tab <- function(x, name) write.csv(x, file.path(OUT, "tables", paste0(name, ".csv")), row.names = FALSE)
save_fig <- function(p, name, w = 8, h = 5) ggsave(file.path(OUT, "figures", paste0(name, ".png")), p, width = w, height = h, dpi = 150)
zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# ---- 1. data: build s, dy, conditions (outline section 3) -------------------------
d <- read.csv(DATA, stringsAsFactors = FALSE)
d$date <- as.Date(d$date); d$next_mpc <- as.Date(d$next_mpc)
d <- d[order(d$date), ]

us <- d[d$us_open == 1, ]
kr <- d[d$kr_open == 1, ]
n  <- nrow(kr)
for (v in c("us2y", "us3y", "us5y", "us10y", "tp_kw", "tp_acm"))
  us[[paste0("d_", v)]] <- c(NA, 100 * diff(us[[v]]))
# NASDAQ is closed on a few US bond-market days (Good Friday, 2012-10-29, 2025-01-09):
# carry the last close, so the return is 0 that day and the move lands on the next day
nq_i <- cummax(seq_len(nrow(us)) * !is.na(us$nasdaq))
us$d_nq <- c(NA, 100 * diff(log(ifelse(nq_i > 0, us$nasdaq[pmax(nq_i, 1)], NA))))

A <- data.frame(date = kr$date[-n], date1 = kr$date[-1])
for (v in c("kr3y", "kr10y")) {
  A[[paste0("dy_", v)]]  <- 100 * diff(kr[[v]])              # dy(t+1), bp
  A[[paste0("dy0_", v)]] <- c(NA, 100 * diff(kr[[v]]))[-n]   # dy(t), bp
}

# shock s(t): sum of US daily changes on US days dated t <= d < t+1
lo <- findInterval(A$date - 1, us$date) + 1
hi <- findInterval(A$date1 - 1, us$date)
A$n_us <- pmax(hi - lo + 1, 0)
sum_us <- function(x) mapply(function(a, b) if (b >= a) sum(x[a:b]) else 0, lo, hi)
for (v in c("us2y", "us3y", "us5y", "us10y", "tp_kw", "tp_acm")) A[[paste0("s_", v)]] <- sum_us(us[[paste0("d_", v)]])
A$eq_ret <- sum_us(us$d_nq)                       # NASDAQ log return (%) over the same US days as s
A$fomc_d <- 1 * (sum_us(us$fomc_day) > 0)
A$mpc_d  <- kr$mpc_day[-1]
A$nous   <- 1 * (A$n_us == 0)
# first day after a Korean holiday: a weekday between t and t+1 that is not a trading day
A$hol <- mapply(function(a, b) { if (b - a <= 1) return(0); s <- seq(a + 1, b - 1, by = "day")
                                 1 * any(!format(s, "%u") %in% c("6", "7")) }, A$date, A$date1)

# value known at the Korean close on t: last non-NA value dated <= t-1 (US series)
known <- function(dates, x, lag_extra = 0) {
  ok <- !is.na(x); i <- findInterval(A$date - 1, dates[ok]) - lag_extra
  ifelse(i > 0, x[ok][pmax(i, 1)], NA)
}
roll_z <- function(x, w = 250) (x - rollapplyr(x, w, mean, fill = NA)) / rollapplyr(x, w, sd, fill = NA)
cum_z  <- function(x, w = 250) { cs <- cumsum(ifelse(is.na(x), 0, x)); cs[is.na(x)] <- NA; roll_z(cs, w) }

k <- kr[-n, ]
A$kr1y_gap     <- k$kr1y - k$base_rate
A$abs_kr1y_gap <- abs(A$kr1y_gap)
A$cd_gap       <- k$cd91 - k$base_rate
A$policy_gap   <- k$base_rate - known(us$date, us$effr, lag_extra = 1)   # EFFR published on d+1
A$days_to_mpc  <- mapply(function(t, nx) {
  in_data <- sum(kr$date > t & kr$date <= nx)
  last <- max(kr$date)
  if (nx <= last) return(in_data)
  extra <- seq(last + 1, nx, by = "day"); in_data + sum(!format(extra, "%u") %in% c("6", "7"))
}, A$date, k$next_mpc)
A$fut3_cum  <- cum_z(kr$fut3_net)[-n]
A$fut3_cum_120 <- cum_z(kr$fut3_net, 120)[-n]   # R12: z-window sensitivity (design v6)
A$fut3_cum_500 <- cum_z(kr$fut3_net, 500)[-n]
f10 <- kr$fut10_net; f10[kr$date < as.Date("2011-06-01")] <- NA
z10 <- rep(NA_real_, n); ok10 <- kr$date >= as.Date("2011-06-01"); z10[ok10] <- cum_z(f10[ok10])
A$fut10_cum <- z10[-n]
A$st_spread <- k$cd91 - known(d$date, d$us3m)
A$fgn_share <- k$fgn_share
A$vix       <- log(known(d$date, d$vix))
A$fx_vol    <- (rollapplyr(c(NA, diff(log(kr$krw_usd))), 20, sd, fill = NA) * sqrt(250))[-n]
A$vkospi    <- k$vkospi
A$usd_idx   <- known(d$date, d$usd_idx)
A$krw_chg20 <- (100 * c(rep(NA, 20), diff(log(kr$krw_usd), lag = 20)))[-n]   # 20-day log change, + = KRW weaker
A$baa_spread <- known(d$date, d$baa_spread)

# alignment checks (outline 3, appendix B)
i928 <- which(A$date1 == as.Date("2026-09-28"))
stopifnot(length(i928) == 1, A$n_us[i928] == 3, A$hol[i928] == 1,
          abs(A$s_us2y[i928] - 100 * (us$us2y[us$date == as.Date("2026-09-25")] -
                                        us$us2y[us$date == as.Date("2026-09-22")])) < 1e-8)
stopifnot(all(A$date1 > A$date), all(A$n_us >= 0), !anyNA(A$s_us2y[A$date >= START]), !anyNA(A$eq_ret[A$date >= START]))
res$alignment_case <- A[i928, c("date", "date1", "n_us", "s_us2y", "dy_kr3y")]

A_all <- A                                      # keeps 2009 for rolling transforms
A <- A[A$date >= START, ]
A$trend <- zs(as.numeric(A$date))
message("pairs: ", nrow(A), " (", min(A$date), " .. ", max(A$date1), ")")

# ---- helpers: HAC regression ------------------------------------------------------
V_nw  <- function(m) NeweyWest(m, lag = L_NW, prewhite = FALSE)
coefs <- function(m, V = V_nw(m)) {
  b <- coef(m); b <- b[!is.na(b)]                     # aliased terms (e.g. a dummy that is all 0 in a subsample)
  se <- sqrt(diag(V))[names(b)]; t <- b / se
  data.frame(term = names(b), est = b, se = se, t = t, p = 2 * pnorm(-abs(t)), row.names = NULL)
}
wald <- function(m, V, R, r = rep(0, nrow(R))) {          # H0: R b = r, HAC chi-square
  b <- coef(m)[colnames(R)]; Vs <- V[colnames(R), colnames(R)]
  dlt <- R %*% b - r; W <- as.numeric(t(dlt) %*% solve(R %*% Vs %*% t(R)) %*% dlt)
  c(W = W, df = nrow(R), p = pchisq(W, nrow(R), lower.tail = FALSE))
}

# data frame for one target with standardized conditions (over the rows used)
prep <- function(tg, conds = CANDS, shock = TARGETS[[tg]]["s"], from = START, filter = NULL) {
  D <- data.frame(date = A$date, dy = A[[paste0("dy_", TARGETS[[tg]]["dy"])]],
                  dy0 = A[[paste0("dy0_", TARGETS[[tg]]["dy"])]], s = A[[paste0("s_", shock)]],
                  A[, c(DUMMIES, "trend")])
  D$mon <- 1 * (D$s * A$eq_ret < 0)               # shock type: yields and stocks move in opposite directions
  D$smon <- D$s * D$mon
  D$eq   <- A$eq_ret                              # NASDAQ return over the same US days (N1 noise check)
  # FLAT (design v6, 3-1): the 2y moved more than the 10y in absolute terms over the same US days
  D$flat <- 1 * (abs(A$s_us2y) > abs(A$s_us10y))
  for (v in conds) D[[v]] <- A[[v]]
  D <- D[D$date >= from, ]
  if (!is.null(filter)) D <- D[filter(D), ]
  D <- D[complete.cases(D[, c("dy", "dy0", "s", conds)]), ]
  for (v in conds) D[[v]] <- zs(D[[v]])
  D$sp <- pmax(D$s, 0); D$sm <- pmin(D$s, 0)
  D
}
hyp_formula <- function(shocks = "s", conds = REPS, trend = TRUE, asym_f = FALSE, shock_type = TRUE) {
  inter <- as.vector(outer(shocks, conds, paste, sep = ":"))
  terms <- c(shocks, conds, inter, "dy0", DUMMIES)
  if (asym_f) {                     # H2 split: s+ and s- each interacted with fut3_cum
    terms <- c("sp", "sm", conds, paste0("s:", setdiff(conds, REPS["H2"])),
               paste0(c("sp", "sm"), ":", REPS["H2"]), "dy0", DUMMIES)
  }
  if (trend) terms <- c(terms, "trend", paste0(if (asym_f) "s" else shocks, ":trend"))
  if (shock_type) terms <- c(terms, "mon", paste0(if (asym_f) "s" else shocks, ":mon"))
  reformulate(terms, "dy")
}

# ---- 4-1. basics: descriptive statistics, unit roots (table 1, appendix A) --------
message("4-1 basics")
desc_vars <- c(dy_kr3y = "dy_kr3y", dy_kr10y = "dy_kr10y", s_us2y = "s_us2y", s_us10y = "s_us10y", eq_ret = "eq_ret",
               setNames(CANDS, CANDS), setNames(ROB_ONLY, ROB_ONLY))
desc <- do.call(rbind, lapply(names(desc_vars), function(v) { x <- A[[desc_vars[v]]]; x <- x[!is.na(x)]
  data.frame(variable = v, n = length(x), mean = mean(x), sd = sd(x), min = min(x), p25 = quantile(x, .25),
             median = median(x), p75 = quantile(x, .75), max = max(x),
             skew = mean((x - mean(x))^3) / sd(x)^3, kurt = mean((x - mean(x))^4) / sd(x)^4, row.names = NULL) }))
res$desc <- desc; save_tab(desc, "table1_descriptive")

ur_row <- function(name, x) {
  x <- x[!is.na(x)]
  adf <- ur.df(x, type = "drift", lags = 10, selectlags = "AIC")
  kp  <- suppressWarnings(kpss.test(x, null = "Level"))
  data.frame(series = name, n = length(x), adf_tau = adf@teststat[1], adf_cv5 = adf@cval[1, "5pct"],
             adf_reject = adf@teststat[1] < adf@cval[1, "5pct"], kpss = unname(kp$statistic),
             kpss_p = kp$p.value, kpss_reject = kp$p.value < 0.05)
}
ur <- rbind(
  ur_row("kr3y (level)", kr$kr3y[kr$date >= START]), ur_row("kr10y (level)", kr$kr10y[kr$date >= START]),
  ur_row("us2y (level)", us$us2y[us$date >= START]), ur_row("us10y (level)", us$us10y[us$date >= START]),
  ur_row("dy_kr3y", A$dy_kr3y), ur_row("dy_kr10y", A$dy_kr10y),
  ur_row("s_us2y", A$s_us2y), ur_row("s_us10y", A$s_us10y),
  do.call(rbind, lapply(c(CANDS, ROB_ONLY), function(v) ur_row(v, A[[v]]))))
# outline 4-1: a condition that looks I(1) (ADF does not reject AND KPSS rejects) is
# replaced by its 250-day rolling z-score (same detrending as the futures positions)
ur$I1 <- !ur$adf_reject & ur$kpss_reject
transformed <- intersect(ur$series[ur$I1], c(CANDS, ROB_ONLY))
for (v in transformed) {                      # rolling z on the full calendar (2009 included)
  A[[paste0(v, "_raw")]] <- A[[v]]
  A[[v]] <- roll_z(A_all[[v]])[match(A$date, A_all$date)]
}
ur$used_as <- ifelse(ur$series %in% transformed, "250-day rolling z", ifelse(ur$series %in% c(CANDS, ROB_ONLY), "level", ""))
res$unit_root <- ur; res$transformed <- transformed; save_tab(ur, "appendixA_unit_root")
message("  I(1) conditions transformed to 250-day z: ", paste(transformed, collapse = ", "))

# ---- 4-2. baseline model with asymmetry (table 2) ---------------------------------
message("4-2 baseline")
base <- list()
for (tg in names(TARGETS)) {
  D <- prep(tg, conds = character(0))
  m <- lm(dy ~ s + dy0 + hol + nous + fomc_d + mpc_d, D); V <- V_nw(m)
  ma <- lm(dy ~ sp + sm + dy0 + hol + nous + fomc_d + mpc_d, D); Va <- V_nw(ma)
  R <- matrix(c(1, -1), 1, dimnames = list(NULL, c("sp", "sm")))
  # same-day check: the US close-to-close change s(t) also covers Asian hours of Korean day t
  # (US yields are taken near 3:30 pm New York time), so part of it may already be in y_KR(t)
  msd <- lm(dy0 ~ s, D); csd <- coefs(msd, V_nw(msd))
  base[[tg]] <- list(model = m, coef = coefs(m, V), r2 = summary(m)$r.squared, n = nobs(m),
                     asym = coefs(ma, Va), asym_test = wald(ma, Va, R), same_day = csd)
}
tab2 <- do.call(rbind, lapply(names(base), function(tg) { b <- base[[tg]]
  cb <- b$coef; ca <- b$asym
  data.frame(target = tg, beta0 = cb$est[cb$term == "s"], t_beta0 = cb$t[cb$term == "s"],
             rho = cb$est[cb$term == "dy0"], t_rho = cb$t[cb$term == "dy0"], r2 = b$r2, n = b$n,
             beta_up = ca$est[ca$term == "sp"], beta_down = ca$est[ca$term == "sm"],
             wald_up_eq_down = b$asym_test["W"], p_up_eq_down = b$asym_test["p"],
             same_day_beta = b$same_day$est[b$same_day$term == "s"], same_day_t = b$same_day$t[b$same_day$term == "s"]) }))
res$baseline <- base; res$table2 <- tab2; save_tab(tab2, "table2_baseline")
# exogeneity check (Granger, the US equation of a VAR): do past Korean changes predict the US shock?
# s(t) on its own lags and lags of dy_KR. Lag 1 of dy0 is Δy(t-1), which ends at the Korean close
# t-1, before any part of s(t); Δy(t) itself is left out because it overlaps s(t) (same-day check).
lagk <- function(x, k) c(rep(NA, k), head(x, -k))
granger <- do.call(rbind, lapply(names(TARGETS), function(tg) do.call(rbind, lapply(c(1, 5, 10), function(p) {
  D <- prep(tg, conds = character(0)); stopifnot(nrow(D) == nrow(A))     # consecutive pairs, so row lags are pair lags
  for (k in seq_len(p)) { D[[paste0("s_l", k)]] <- lagk(D$s, k); D[[paste0("kr_l", k)]] <- lagk(D$dy0, k) }
  m <- lm(reformulate(c(paste0("s_l", 1:p), paste0("kr_l", 1:p)), "s"), D); V <- V_nw(m)
  m0 <- lm(reformulate(paste0("s_l", 1:p), "s"), D[complete.cases(D[, c("s", paste0("kr_l", 1:p))]), ])
  kr <- paste0("kr_l", 1:p); R <- diag(p); dimnames(R) <- list(NULL, kr); w <- wald(m, V, R)
  # does it matter for the spillover? baseline (1) with the same Korean lags added
  mb <- lm(reformulate(c("s", "dy0", DUMMIES, kr), "dy"), D); cb <- coefs(mb, V_nw(mb))
  data.frame(target = tg, lags = p, n = nobs(m), sum_kr = sum(coef(m)[kr]), W = w["W"], df = p, p = w["p"],
             dR2 = summary(m)$r.squared - summary(m0)$r.squared,
             beta0_base = coef(base[[tg]]$model)["s"], beta0_with_lags = cb$est[cb$term == "s"], row.names = NULL) }))))
res$granger <- granger; save_tab(granger, "granger_exogeneity")
# shock type (Hoek, Kamin & Yoldas 2022): growth-type (mon = 0) vs monetary-type (mon = 1) shocks
tab2b <- do.call(rbind, lapply(names(TARGETS), function(tg) {
  D <- prep(tg, conds = REPS)
  m <- lm(dy ~ s + mon + s:mon + dy0 + hol + nous + fomc_d + mpc_d, D); cf <- coefs(m)
  b <- coef(m); V <- V_nw(m); g <- setNames(rep(0, length(b)), names(b)); g[c("s", "s:mon")] <- 1
  data.frame(target = tg, share_mon = mean(D$mon[D$s != 0]), corr_mon_vix = cor(D$mon, D$vix),
             beta_growth = cf$est[cf$term == "s"], beta_monetary = sum(g * b),
             se_monetary = sqrt(as.numeric(t(g) %*% V %*% g)),
             diff = cf$est[cf$term == "s:mon"], t_diff = cf$t[cf$term == "s:mon"], p_diff = cf$p[cf$term == "s:mon"]) }))
res$table2b <- tab2b; save_tab(tab2b, "table2b_shock_type")

# ---- 4-3. hypothesis tests (table 3, figure 1) -------------------------------------
message("4-3 hypothesis tests")
hyp <- list()
for (tg in names(TARGETS)) {
  D <- prep(tg, conds = REPS)
  out <- list()
  for (tr in c(TRUE, FALSE)) {
    m  <- lm(hyp_formula(trend = tr), D); V <- V_nw(m)
    ma <- lm(hyp_formula(trend = tr, asym_f = TRUE), D); Va <- V_nw(ma)
    out[[if (tr) "trend" else "notrend"]] <- list(model = m, V = V, coef = coefs(m, V),
                                                  asym = ma, Va = Va, coef_asym = coefs(ma, Va),
                                                  r2 = summary(m)$r.squared, n = nobs(m))
  }
  # economic size: beta at the 20th / 80th percentile of each condition (others at mean, trend 0)
  mm <- out$trend; b <- coef(mm$model); V <- mm$V
  size <- do.call(rbind, lapply(names(REPS), function(h) { v <- REPS[h]; term <- paste0("s:", v)
    do.call(rbind, lapply(c(0.2, 0.8), function(q) { zq <- quantile(D[[v]], q)
      g <- setNames(rep(0, length(b)), names(b)); g["s"] <- 1; g[term] <- zq; g["s:mon"] <- mean(D$mon)
      est <- sum(g * b); se <- sqrt(as.numeric(t(g) %*% V %*% g))
      data.frame(target = tg, hyp = h, condition = v, quantile = q, z = zq, beta = est, lo = est - 1.96 * se, hi = est + 1.96 * se) })) }))
  # figure 1: quintile betas, b5 - b1 = 0
  quint <- do.call(rbind, lapply(names(REPS), function(h) { v <- REPS[h]
    Q <- cut(D[[v]], quantile(D[[v]], 0:5 / 5), include.lowest = TRUE, labels = paste0("q", 1:5))
    X <- data.frame(D, Q = Q)
    m <- lm(dy ~ 0 + Q + Q:s + dy0 + hol + nous + fomc_d + mpc_d + trend + s:trend, X); V <- V_nw(m)   # spec A (design v6): no mon
    cf <- coefs(m, V); bq <- cf[grepl("^Q.*:s$", cf$term), ]
    R <- matrix(0, 1, length(coef(m)), dimnames = list(NULL, names(coef(m)))); R[, "Qq5:s"] <- 1; R[, "Qq1:s"] <- -1
    wt <- wald(m, V, R)
    data.frame(target = tg, hyp = h, condition = v, quintile = 1:5, beta = bq$est, lo = bq$est - 1.96 * bq$se,
               hi = bq$est + 1.96 * bq$se, n = as.vector(table(Q)), p_q5_q1 = unname(wt["p"]), row.names = NULL) }))
  hyp[[tg]] <- list(fits = out, size = size, quint = quint)
}
# table 3: interaction rows, two-sided p, Holm over 6 tests (H1, H3 symmetric; H2 = beta+ from split model)
tab3 <- do.call(rbind, lapply(names(hyp), function(tg) do.call(rbind, lapply(c("trend", "notrend"), function(sp) {
  f <- hyp[[tg]]$fits[[sp]]
  rows <- rbind(f$coef[f$coef$term %in% c("s", paste0("s:", REPS[c("H1", "H3")]), paste0("s:", REPS["H2"]), "s:mon"), ],
                transform(f$coef_asym[f$coef_asym$term %in% paste0(c("sp", "sm"), ":", REPS["H2"]), ]))
  rows$target <- tg; rows$spec <- sp; rows }))))
tab3$hyp <- ifelse(grepl(REPS["H1"], tab3$term), "H1", ifelse(grepl(REPS["H2"], tab3$term), "H2",
           ifelse(grepl(REPS["H3"], tab3$term), "H3", "")))
tab3$predicted <- ifelse(tab3$term %in% c(paste0("s:", REPS[c("H1", "H3")]), paste0("sp:", REPS["H2"])),
                         c(H1 = "+", H2 = "+", H3 = "+")[tab3$hyp], "")
tab3$test6 <- tab3$term %in% c(paste0("s:", REPS[c("H1", "H3")]), paste0("sp:", REPS["H2"]))
tab3$p_holm <- NA
for (sp in c("trend", "notrend")) { i <- which(tab3$test6 & tab3$spec == sp); tab3$p_holm[i] <- p.adjust(tab3$p[i], "holm") }
res$hyp <- hyp; res$table3 <- tab3; save_tab(tab3, "table3_hypothesis")
size_all <- do.call(rbind, lapply(hyp, `[[`, "size")); res$econ_size <- size_all; save_tab(size_all, "table3b_economic_size")
quint_all <- do.call(rbind, lapply(hyp, `[[`, "quint")); res$quintiles <- quint_all; save_tab(quint_all, "figure1_quintiles")
quint_all$panel <- paste0(ifelse(quint_all$target == "k3", "KTB 3y <- UST 2y", "KTB 10y <- UST 10y"), " | ",
                          quint_all$hyp, ": ", quint_all$condition)
p1 <- ggplot(quint_all, aes(quintile, beta)) + geom_hline(yintercept = 0, colour = "grey70") +
  geom_pointrange(aes(ymin = lo, ymax = hi)) + facet_wrap(~ panel, ncol = 3) +
  labs(x = "Condition quintile (1 = lowest)", y = "Spillover coefficient", title = "Figure 1. Spillover by condition quintile (95% HAC bands)") +
  theme_minimal(base_size = 10)
save_fig(p1, "figure1_quintiles", 10, 6)

# correlation and VIF of the representatives (appendix)
Dr <- prep("k10", conds = REPS)
res$rep_cor <- cor(Dr[, REPS])
res$rep_vif <- sapply(REPS, function(v) 1 / (1 - summary(lm(reformulate(setdiff(REPS, v), v), Dr))$r.squared))

# ---- 4-3a. Part 1 of design v6 (docs/조건별전이_연구설계_통합안.md, 1-1..1-3) -----------
# Pre-registered model WITHOUT the shock-type control. Spec A = trend (#12), B = no trend
# (11-14 outline); C/D add mon (post hoc, robustness). Holm over the 6 tests within a spec;
# "significant" only if Holm < 0.05 in both A and B. Joint HAC Wald chi2(3) per maturity.
message("4-3a part 1 (specs A-D), joint Wald, collinearity")
SPECS <- data.frame(spec = c("A", "B", "C", "D"), trend = c(TRUE, FALSE, TRUE, FALSE), mon = c(FALSE, FALSE, TRUE, TRUE))
P1_TERMS <- c(H1 = paste0("s:", REPS[["H1"]]), H2 = paste0("sp:", REPS[["H2"]]), H3 = paste0("s:", REPS[["H3"]]))
fit_part1 <- function(D, trend, mon) {
  m  <- lm(hyp_formula(trend = trend, shock_type = mon), D); V <- V_nw(m)
  ma <- lm(hyp_formula(trend = trend, asym_f = TRUE, shock_type = mon), D); Va <- V_nw(ma)
  list(m = m, V = V, ma = ma, Va = Va, cf = coefs(m, V), cfa = coefs(ma, Va))
}
part1_rows <- function(f) {             # H1, H3 from the symmetric model, H2 = s+ x fut3 from the split model
  rbind(f$cf[f$cf$term %in% P1_TERMS[c("H1", "H3")], ], f$cfa[f$cfa$term == P1_TERMS["H2"], ])
}
p1_fits <- list(); p1 <- list(); jw <- list()
for (tg in names(TARGETS)) {
  D <- prep(tg, conds = REPS)
  for (k in seq_len(nrow(SPECS))) {
    sp <- SPECS[k, ]; f <- fit_part1(D, sp$trend, sp$mon); p1_fits[[paste(tg, sp$spec)]] <- f
    r <- part1_rows(f); r$hyp <- names(P1_TERMS)[match(r$term, P1_TERMS)]
    p1[[length(p1) + 1]] <- data.frame(target = tg, spec = sp$spec, trend = sp$trend, mon = sp$mon, r, n = nobs(f$m), row.names = NULL)
    # joint Wald in the split model: all three pre-registered interactions = 0
    R <- matrix(0, 3, length(coef(f$ma)), dimnames = list(NULL, names(coef(f$ma))))
    # lm names s x condition "condition:s" in the split model (s first appears after the conditions)
    cname <- function(tm) { cn <- names(coef(f$ma)); if (tm %in% cn) tm else paste(rev(strsplit(tm, ":")[[1]]), collapse = ":") }
    for (j in 1:3) R[j, cname(unname(P1_TERMS[j]))] <- 1
    wj <- wald(f$ma, f$Va, R)
    # H2 mechanism check (part 2, item 1): s+ x fut3 = s- x fut3 in the split model
    R2 <- matrix(0, 1, length(coef(f$ma)), dimnames = list(NULL, names(coef(f$ma))))
    R2[1, P1_TERMS["H2"]] <- 1; R2[1, paste0("sm:", REPS[["H2"]])] <- -1
    w2 <- wald(f$ma, f$Va, R2)
    jw[[length(jw) + 1]] <- data.frame(target = tg, spec = sp$spec, joint_W = wj["W"], joint_df = wj["df"], joint_p = wj["p"],
                                       h2_up_eq_down_W = w2["W"], h2_up_eq_down_p = w2["p"], row.names = NULL)
  }
}
tab_p1 <- do.call(rbind, p1)
tab_p1$predicted <- "+"
tab_p1$p_holm <- ave(tab_p1$p, tab_p1$spec, FUN = function(p) p.adjust(p, "holm"))
vd <- function(tg, h) { a <- tab_p1$p_holm[tab_p1$target == tg & tab_p1$hyp == h & tab_p1$spec == "A"]
  b <- tab_p1$p_holm[tab_p1$target == tg & tab_p1$hyp == h & tab_p1$spec == "B"]
  e <- tab_p1$est[tab_p1$target == tg & tab_p1$hyp == h & tab_p1$spec == "A"]
  if (a < 0.05 && b < 0.05) paste0("significant (", ifelse(e > 0, "predicted", "opposite"), " sign)")
  else if (a < 0.05 || b < 0.05) "spec-dependent" else "not rejected" }
p1_verdict <- do.call(rbind, lapply(names(TARGETS), function(tg) data.frame(target = tg, hyp = names(P1_TERMS),
  verdict = sapply(names(P1_TERMS), function(h) vd(tg, h)), row.names = NULL)))
tab_jw <- do.call(rbind, jw)
tab_jw$joint_p_holm <- ave(tab_jw$joint_p, tab_jw$spec, FUN = function(p) p.adjust(p, "holm"))   # 2 maturities
res$part1 <- list(table = tab_p1, verdict = p1_verdict, joint_wald = tab_jw, specs = SPECS)
save_tab(tab_p1, "table3a_part1"); save_tab(p1_verdict, "table3a_part1_verdict"); save_tab(tab_jw, "table3c_joint_wald")

# part 2, item 6 (post hoc): does s x VIX weaken under mon because s x VIX and s x mon overlap?
coll <- do.call(rbind, lapply(names(TARGETS), function(tg) {
  fA <- p1_fits[[paste(tg, "A")]]; fC <- p1_fits[[paste(tg, "C")]]; X <- model.matrix(fC$m)
  vt <- P1_TERMS[["H3"]]; vif1 <- function(v) 1 / (1 - summary(lm(X[, v] ~ X[, setdiff(colnames(X), c("(Intercept)", v))]))$r.squared)
  data.frame(target = tg, corr_svix_smon = cor(X[, vt], X[, "s:mon"]), corr_mon_vix = cor(X[, "mon"], X[, REPS[["H3"]]]),
             vif_svix_A = { XA <- model.matrix(fA$m); 1 / (1 - summary(lm(XA[, vt] ~ XA[, setdiff(colnames(XA), c("(Intercept)", vt))]))$r.squared) },
             vif_svix_C = vif1(vt), vif_smon_C = vif1("s:mon"),
             se_svix_A = fA$cf$se[fA$cf$term == vt], se_svix_C = fC$cf$se[fC$cf$term == vt],
             est_svix_A = fA$cf$est[fA$cf$term == vt], est_svix_C = fC$cf$est[fC$cf$term == vt], row.names = NULL) }))
res$part2_collinearity <- coll; save_tab(coll, "table3d_collinearity")

# economic size per 10 bp US shock (design v6, 4-1): conditions in s.d. units (they are z-scored)
econ10 <- do.call(rbind, lapply(names(TARGETS), function(tg) {
  fA <- p1_fits[[paste(tg, "A")]]; r <- part1_rows(fA); b2 <- tab2b[tab2b$target == tg, ]
  D <- prep(tg, conds = character(0))
  rbind(data.frame(target = tg, item = "baseline: 10 bp US shock", bp = 10 * tab2[tab2$target == tg, "beta0"]),
        data.frame(target = tg, item = paste0("change per +1 s.d. of ", names(P1_TERMS)[match(r$term, P1_TERMS)], " (spec A)"), bp = 10 * r$est),
        data.frame(target = tg, item = c("growth-type day (mon = 0)", "monetary-type day (mon = 1)"),
                   bp = 10 * c(b2$beta_growth, b2$beta_monetary)),
        data.frame(target = tg, item = "s.d. of daily KTB change", bp = sd(D$dy)), row.names = NULL) }))
res$econ10 <- econ10; save_tab(econ10, "table3e_econ_10bp")

# ---- Part 3 OLS pieces (design v6, 3-2): N2 FLAT, N1 noise check -------------------
message("part 3: N2 (FLAT), N1 noise check")
part3_ols <- do.call(rbind, lapply(names(TARGETS), function(tg) {
  D <- prep(tg, conds = character(0))
  m2 <- lm(dy ~ s + flat + s:flat + dy0 + hol + nous + fomc_d + mpc_d, D); c2 <- coefs(m2)
  Dn <- D[abs(D$eq) >= 0.1, ]                                    # drop days with |NASDAQ| < 0.1%
  m1 <- lm(dy ~ s + mon + s:mon + dy0 + hol + nous + fomc_d + mpc_d, Dn); c1 <- coefs(m1)
  rbind(data.frame(target = tg, test = "N2", term = "s:flat", est = c2$est[c2$term == "s:flat"], se = c2$se[c2$term == "s:flat"],
                   t = c2$t[c2$term == "s:flat"], p_one = pnorm(-c2$t[c2$term == "s:flat"]), p_two = c2$p[c2$term == "s:flat"],
                   share = mean(D$flat[D$s != 0]), corr_with_mon = cor(D$flat, D$mon), n = nobs(m2)),
        data.frame(target = tg, test = "N1 noise check (|NASDAQ| >= 0.1%)", term = "s:mon", est = c1$est[c1$term == "s:mon"],
                   se = c1$se[c1$term == "s:mon"], t = c1$t[c1$term == "s:mon"], p_one = NA, p_two = c1$p[c1$term == "s:mon"],
                   share = mean(Dn$mon[Dn$s != 0]), corr_with_mon = NA, n = nobs(m1)), row.names = NULL) }))
res$part3_ols <- part3_ols; save_tab(part3_ols, "table8a_part3_ols")

# ---- 4-4. LASSO / Elastic Net (full sample, figure 2) ------------------------------
# unpenalized: s, dy0, dummies, mon, s x mon (and trend, s x trend); penalized: s x condition.
# Full-sample fits keep the trend like the hypothesis model; rolling forecasts drop it like M1/M2.
lasso_design <- function(D, trend = TRUE) {
  inter <- sapply(CANDS, function(v) D$s * D[[v]]); colnames(inter) <- paste0("s:", CANDS)
  ctrl <- cbind(s = D$s, dy0 = D$dy0, as.matrix(D[, DUMMIES]), mon = D$mon, "s:mon" = D$smon)
  if (trend) ctrl <- cbind(ctrl, trend = D$trend, "s:trend" = D$s * D$trend)
  X <- cbind(ctrl, as.matrix(D[, CANDS]), inter)
  pf <- c(rep(0, ncol(X) - length(CANDS)), rep(1, length(CANDS)))
  list(X = X, pf = pf)
}
bic_fit <- function(X, y, pf, alpha) {
  f <- glmnet(X, y, alpha = alpha, penalty.factor = pf, standardize = FALSE)
  rss <- colSums((y - predict(f, X))^2); Tn <- length(y)
  bic <- Tn * log(rss / Tn) + f$df * log(Tn); j <- which.min(bic)
  list(fit = f, lambda = f$lambda[j], beta = as.matrix(coef(f, s = f$lambda[j]))[, 1], bic = bic[j])
}
message("4-4 LASSO")
lasso <- list()
for (tg in names(TARGETS)) {
  D <- prep(tg, conds = CANDS, from = START10); L <- lasso_design(D)
  la <- bic_fit(L$X, D$dy, L$pf, 1); en <- bic_fit(L$X, D$dy, L$pf, 0.5)
  inter <- paste0("s:", CANDS)
  lasso[[tg]] <- list(n = nrow(D), p = ncol(L$X), lasso = la, enet = en,
                      selected = data.frame(target = tg, interaction = inter, lasso = la$beta[inter], enet = en$beta[inter], row.names = NULL))
}
tab_l <- do.call(rbind, lapply(lasso, `[[`, "selected")); res$lasso <- lasso; res$table_lasso <- tab_l; save_tab(tab_l, "table4_lasso")

if (STAGE >= 2) {
  message("  figure 2: rolling selection heatmap")
  heat <- list()
  for (tg in names(TARGETS)) {
    D <- prep(tg, conds = CANDS, from = START10); L <- lasso_design(D)
    ends <- seq(W, nrow(D), by = STEP_H)
    sel <- t(sapply(ends, function(e) { i <- (e - W + 1):e
      Xi <- L$X[i, ]; ci <- (ncol(Xi) - 2 * length(CANDS) + 1):(ncol(Xi) - length(CANDS))   # re-standardize conditions in window
      cz <- scale(Xi[, ci]); Xi[, ci] <- cz; Xi[, paste0("s:", CANDS)] <- Xi[, "s"] * cz
      b <- bic_fit(Xi, D$dy[i], L$pf, 1)$beta; 1 * (b[paste0("s:", CANDS)] != 0) }))
    heat[[tg]] <- data.frame(target = tg, end = rep(D$date[ends], length(CANDS)),
                             interaction = rep(CANDS, each = length(ends)), selected = as.vector(sel))
  }
  heat_all <- do.call(rbind, heat)
  freq <- aggregate(selected ~ target + interaction, heat_all, mean)
  res$heatmap <- heat_all; res$heat_freq <- freq; save_tab(freq, "figure2_selection_frequency")
  heat_all$interaction <- factor(heat_all$interaction, levels = rev(CANDS))
  heat_all$target <- factor(ifelse(heat_all$target == "k3", "KTB 3y <- UST 2y", "KTB 10y <- UST 10y"),
                            levels = c("KTB 3y <- UST 2y", "KTB 10y <- UST 10y"))
  p2 <- ggplot(heat_all, aes(end, interaction, fill = factor(selected))) + geom_tile(width = 30) + facet_wrap(~ target) +
    scale_fill_manual(values = c("0" = "grey92", "1" = "grey25"), labels = c("not selected", "selected"), name = NULL) +
    labs(x = "Window end (1,000-day rolling, step 20)", y = "s x condition", title = "Figure 2. Rolling LASSO selection (BIC)") +
    theme_minimal(base_size = 10)
  save_fig(p2, "figure2_heatmap", 10, 5)
}

# ---- 4-5. out-of-sample (table 5, figure 3) ----------------------------------------
message("4-5 out-of-sample")
# Clark-West: f = e1^2 - (e2^2 - (yhat1 - yhat2)^2); one-sided, HAC t
clark_west <- function(y, f1, f2) {
  adj <- (y - f1)^2 - ((y - f2)^2 - (f1 - f2)^2); m <- lm(adj ~ 1)
  t <- coef(m) / sqrt(V_nw(m)[1, 1]); c(t = unname(t), p = pnorm(-t))
}
# Giacomini-White with squared loss: unconditional (HAC t) and conditional, h = (1, d(t-1))
gw_sq <- function(y, f1, f2) {
  dl <- (y - f1)^2 - (y - f2)^2; m <- lm(dl ~ 1); tu <- coef(m) / sqrt(V_nw(m)[1, 1])
  h <- cbind(1, c(NA, head(dl, -1))); Z <- (h * dl)[-1, ]; Tn <- nrow(Z)
  zbar <- colMeans(Z); Om <- crossprod(Z) / Tn             # tau = 1: no HAC needed (Giacomini-White 2006)
  stat <- Tn * as.numeric(t(zbar) %*% solve(Om) %*% zbar)
  tu <- unname(tu)
  c(t_uncond = tu, p_uncond = 2 * pnorm(-abs(tu)), chi2_cond = stat, p_cond = pchisq(stat, 2, lower.tail = FALSE))
}
# course code: gwtest.R (absolute loss) and dmwtest.R, copied so the file stays self-contained
gw.test <- function(x, y, p, T, tau, method = c("HAC", "NeweyWest", "Andrews", "LumleyHeagerty"),
                    alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative); l1 <- abs(x - p); l2 <- abs(y - p); dif <- l1 - l2
  mod <- lm(dif ~ 0 + rep(1, length(dif)))
  STATISTIC <- summary(mod)$coefficients[1, 3]            # tau = 1 branch of the course function
  PVAL <- switch(alternative, two.sided = 2 * pnorm(-abs(STATISTIC)), less = pnorm(STATISTIC),
                 greater = pnorm(STATISTIC, lower.tail = FALSE))
  list(statistic = STATISTIC, p.value = PVAL)
}
dmwtest <- function(real, f1, f2) {
  se1 <- (real - f1)^2; se2 <- (real - f2)^2; dt <- se1 - se2; nf <- length(real)
  e <- dt - mean(dt); s0 <- sum(e^2) / nf; q <- round(nf^(1 / 3)); s2 <- 0
  for (L in seq_len(q)) s2 <- s2 + (1 - L / (q + 1)) * sum(e[(L + 1):nf] * e[1:(nf - L)]) / nf
  DMW <- sqrt(nf) * mean(dt) / sqrt(s0 + 2 * s2)
  c(DMW = DMW, p = 2 * pnorm(-abs(DMW)), mse1 = mean(se1), mse2 = mean(se2))
}
# rolling forecasts: window = previous W pairs (their outcomes are known at the Korean close on t)
# design v6: M1 = baseline without mon; M2 (= M2' in the design) adds the 3 conditions, no mon;
# M4 = M1 + mon, s x mon (N5 vs M1, N7 vs M2)
roll_ols <- function(D, conds, extra = character(0)) {
  base_cols <- c("s", "dy0", DUMMIES, extra); fc <- rep(NA_real_, nrow(D))
  for (i in (W + 1):nrow(D)) {
    w <- (i - W):(i - 1)
    if (length(conds)) { Zw <- as.matrix(D[w, conds, drop = FALSE]); mu <- colMeans(Zw); sdv <- apply(Zw, 2, sd) }
    mk <- function(r) { X <- cbind(1, as.matrix(D[r, base_cols]))
      if (length(conds)) { Z <- sweep(sweep(as.matrix(D[r, conds, drop = FALSE]), 2, mu), 2, sdv, "/"); X <- cbind(X, Z, D$s[r] * Z) }
      X }
    Xw <- mk(w); b <- lm.fit(Xw, D$dy[w])$coefficients; b[is.na(b)] <- 0
    fc[i] <- sum(mk(i) * b)
  }
  fc
}
roll_lasso <- function(D) {
  L <- lasso_design(D, trend = FALSE); fc <- rep(NA_real_, nrow(D)); ci <- match(CANDS, colnames(L$X))
  refit <- seq(W + 1, nrow(D), by = STEP_H)
  for (r in refit) {
    w <- (r - W):(r - 1); Xw <- L$X[w, ]
    mu <- colMeans(Xw[, ci]); sdv <- apply(Xw[, ci], 2, sd)
    stdz <- function(X) { Z <- sweep(sweep(X[, ci, drop = FALSE], 2, mu), 2, sdv, "/")
      X[, ci] <- Z; X[, paste0("s:", CANDS)] <- X[, "s"] * Z; X }
    b <- bic_fit(stdz(Xw), D$dy[w], L$pf, 1)$beta
    for (i in r:min(r + STEP_H - 1, nrow(D))) fc[i] <- sum(c(1, stdz(L$X[i, , drop = FALSE])) * b)
  }
  fc
}
oos_stats <- function(y, f1, f2) {
  e1 <- y - f1; e2 <- y - f2; nz <- y != 0
  c(n = length(y), msfe1 = mean(e1^2), msfe2 = mean(e2^2), mafe1 = mean(abs(e1)), mafe2 = mean(abs(e2)),
    hit1 = if (all(f1 == 0)) NA else mean(sign(f1[nz]) == sign(y[nz])),   # M0 has no direction
    hit2 = mean(sign(f2[nz]) == sign(y[nz])),
    r2_oos = 1 - sum(e2^2) / sum(e1^2))
}
oos <- list()
for (tg in names(TARGETS)) {
  D <- prep(tg, conds = REPS)
  f0 <- rep(0, nrow(D)); f1 <- roll_ols(D, character(0)); f2 <- roll_ols(D, unname(REPS))
  f4 <- roll_ols(D, character(0), extra = c("mon", "smon"))
  ok <- !is.na(f1)
  y <- D$dy[ok]
  cmp <- rbind(
    data.frame(target = tg, comparison = "M1 vs M0", t(oos_stats(y, f0[ok], f1[ok])), cw_t = NA, cw_p = NA,
               t(gw_sq(y, f0[ok], f1[ok])), gw_course_p = gw.test(f0[ok], f1[ok], y, T = length(y), tau = 1)$p.value, dm_p = NA),
    data.frame(target = tg, comparison = "M2 vs M1", t(oos_stats(y, f1[ok], f2[ok])), t(setNames(clark_west(y, f1[ok], f2[ok]), c("cw_t", "cw_p"))),
               t(gw_sq(y, f1[ok], f2[ok])), gw_course_p = gw.test(f1[ok], f2[ok], y, T = length(y), tau = 1)$p.value, dm_p = NA),
    data.frame(target = tg, comparison = "M4 vs M1 (N5)", t(oos_stats(y, f1[ok], f4[ok])), t(setNames(clark_west(y, f1[ok], f4[ok]), c("cw_t", "cw_p"))),
               t(gw_sq(y, f1[ok], f4[ok])), gw_course_p = gw.test(f1[ok], f4[ok], y, T = length(y), tau = 1)$p.value, dm_p = NA),
    data.frame(target = tg, comparison = "M4 vs M2 (N7, non-nested)", t(oos_stats(y, f2[ok], f4[ok])), cw_t = NA, cw_p = NA,
               t(gw_sq(y, f2[ok], f4[ok])), gw_course_p = gw.test(f2[ok], f4[ok], y, T = length(y), tau = 1)$p.value,
               dm_p = dmwtest(y, f2[ok], f4[ok])["p"]))
  fc <- data.frame(date = D$date, dy = D$dy, M0 = f0, M1 = f1, M2 = f2, M4 = f4)
  if (STAGE >= 2) {
    D3 <- prep(tg, conds = CANDS, from = START10)
    f3 <- roll_lasso(D3)
    fc <- merge(fc, data.frame(date = D3$date, M3 = f3), by = "date", all.x = TRUE)
    ok3 <- !is.na(fc$M3) & !is.na(fc$M2)
    y3 <- fc$dy[ok3]; dm <- dmwtest(y3, fc$M2[ok3], fc$M3[ok3])
    cmp <- rbind(cmp,
      data.frame(target = tg, comparison = "M3 vs M1", t(oos_stats(y3, fc$M1[ok3], fc$M3[ok3])),
                 t(setNames(clark_west(y3, fc$M1[ok3], fc$M3[ok3]), c("cw_t", "cw_p"))), t(gw_sq(y3, fc$M1[ok3], fc$M3[ok3])),
                 gw_course_p = gw.test(fc$M1[ok3], fc$M3[ok3], y3, T = length(y3), tau = 1)$p.value, dm_p = NA),
      data.frame(target = tg, comparison = "M3 vs M2 (non-nested)", t(oos_stats(y3, fc$M2[ok3], fc$M3[ok3])), cw_t = NA, cw_p = NA,
                 t(gw_sq(y3, fc$M2[ok3], fc$M3[ok3])), gw_course_p = gw.test(fc$M2[ok3], fc$M3[ok3], y3, T = length(y3), tau = 1)$p.value,
                 dm_p = dm["p"]))
  }
  oos[[tg]] <- list(forecasts = fc, compare = cmp, first_forecast = min(fc$date[!is.na(fc$M1)]))
}
tab5 <- do.call(rbind, lapply(oos, `[[`, "compare")); res$oos <- oos; res$table5 <- tab5; save_tab(tab5, "table5_oos")
cum <- do.call(rbind, lapply(names(oos), function(tg) { f <- oos[[tg]]$forecasts; f <- f[!is.na(f$M1), ]
  data.frame(target = ifelse(tg == "k3", "KTB 3y", "KTB 10y"), date = f$date, cum = cumsum((f$dy - f$M1)^2 - (f$dy - f$M2)^2)) }))
p3 <- ggplot(cum, aes(date, cum)) + geom_hline(yintercept = 0, colour = "grey70") + geom_line() + facet_wrap(~ target, scales = "free_y") +
  labs(x = NULL, y = "Cumulative (e1^2 - e2^2), bp^2", title = "Figure 3. Cumulative squared-error gain of M2 over M1 (rising = M2 better)") +
  theme_minimal(base_size = 10)
save_fig(p3, "figure3_cumulative_loss", 10, 4)

# ---- 4-7. diagnostics -------------------------------------------------------------
# design v6: diagnostics and robustness use the part 1 main model (spec A: trend, no mon)
hyp_A <- function(...) hyp_formula(..., shock_type = FALSE)
message("4-7 diagnostics")
arch_lm <- function(e, q = 5) { e2 <- e^2; X <- embed(e2, q + 1); m <- lm(X[, 1] ~ X[, -1])
  stat <- nrow(X) * summary(m)$r.squared; c(stat = stat, p = pchisq(stat, q, lower.tail = FALSE)) }
diag <- do.call(rbind, lapply(names(TARGETS), function(tg) {
  e <- residuals(p1_fits[[paste(tg, "A")]]$m)
  lb <- Box.test(e, lag = 10, type = "Ljung-Box"); ar <- arch_lm(e)
  D <- prep(tg, conds = REPS); cut99 <- quantile(abs(D$s), 0.99)
  m_inf <- lm(hyp_A(), D[abs(D$s) <= cut99, ]); ci <- coefs(m_inf)
  vif <- sapply(REPS, function(v) 1 / (1 - summary(lm(reformulate(setdiff(REPS, v), v), D))$r.squared))
  data.frame(target = tg, ljung_box_q10 = unname(lb$statistic), lb_p = lb$p.value, arch_lm5 = ar["stat"], arch_p = ar["p"],
             t(setNames(ci$est[ci$term %in% paste0("s:", REPS)], paste0("drop_top1pct_", names(REPS)))),
             t(setNames(vif, paste0("vif_", names(REPS)))), row.names = NULL) }))
res$diagnostics <- diag; save_tab(diag, "diagnostics")
# Ljung-Box significant -> outline says add a second lag of dy; report that model too
lag2 <- lapply(names(TARGETS), function(tg) {
  D <- prep(tg, conds = REPS); D$dy0b <- c(NA, head(D$dy0, -1))
  m <- lm(update(hyp_A(), . ~ . + dy0b), D); cf <- coefs(m); cf$target <- tg; cf[cf$term %in% paste0("s:", REPS), ] })
res$lag2 <- do.call(rbind, lag2)

# ---- 4-6. robustness (table 6, appendix D) -----------------------------------------
if (STAGE >= 2) {
  message("4-6 robustness")
  pick <- function(m, label, tg, terms = paste0("s:", REPS)) { cf <- coefs(m); cf <- cf[cf$term %in% terms, ]
    if (nrow(cf)) { cf$check <- label; cf$target <- tg }; cf }
  rob <- list()
  for (tg in names(TARGETS)) {
    # R1: alternative shock maturities
    for (sh in c("us3y", "us5y", "us10y", "us2y")) if (sh != TARGETS[[tg]]["s"])
      rob[[length(rob) + 1]] <- pick(lm(hyp_A(), prep(tg, REPS, shock = sh)), paste0("R1 shock ", sh), tg)
    # R3: drop pairs within 3 Korean trading days of an FOMC shock
    fom <- which(A$fomc_d == 1); near <- unique(unlist(lapply(fom, function(i) (i - 3):(i + 3))))
    keep_dates <- A$date[-near[near >= 1 & near <= nrow(A)]]
    rob[[length(rob) + 1]] <- pick(lm(hyp_A(), prep(tg, REPS, filter = function(D) D$date %in% keep_dates)), "R3 drop FOMC +-3 days", tg)
    # R4: split samples
    rob[[length(rob) + 1]] <- pick(lm(hyp_A(trend = FALSE), prep(tg, REPS, filter = function(D) D$date < as.Date("2020-01-01"))), "R4 2010-2019", tg)
    rob[[length(rob) + 1]] <- pick(lm(hyp_A(trend = FALSE), prep(tg, REPS, filter = function(D) D$date >= as.Date("2020-01-01"))), "R4 2020-2026", tg)
    # R5: winsorize dy, dy0, s at 1% / 99%
    D <- prep(tg, REPS); wz <- function(x) { q <- quantile(x, c(.01, .99)); pmin(pmax(x, q[1]), q[2]) }
    D$dy <- wz(D$dy); D$dy0 <- wz(D$dy0); D$s <- wz(D$s)
    rob[[length(rob) + 1]] <- pick(lm(hyp_A(), D), "R5 winsorized 1%", tg)
    # R6: conditions as 5-day averages (up to t)
    A5 <- A; for (v in REPS) A5[[v]] <- rollapplyr(A[[v]], 5, mean, fill = NA)
    Atmp <- A; A <- A5; D6 <- prep(tg, REPS); A <- Atmp
    rob[[length(rob) + 1]] <- pick(lm(hyp_A(), D6), "R6 5-day average conditions", tg)
    # R7: signed kr1y_gap for H1; fx_vol for H3
    alt <- c(H1 = "kr1y_gap", H2 = REPS[["H2"]], H3 = "fx_vol")
    rob[[length(rob) + 1]] <- pick(lm(hyp_A(conds = alt), prep(tg, alt)), "R7 signed gap, fx_vol", tg, paste0("s:", alt))
    # R8: st_spread replaces fut3_cum
    alt8 <- c(H1 = REPS[["H1"]], H2 = "st_spread", H3 = REPS[["H3"]])
    rob[[length(rob) + 1]] <- pick(lm(hyp_A(conds = alt8), prep(tg, alt8)), "R8 st_spread for H2", tg, paste0("s:", alt8))
    # R9 (outline 2-5): hypothesis model on the 2012-05-31 sample
    rob[[length(rob) + 1]] <- pick(lm(hyp_A(), prep(tg, REPS, from = START10)), "R9 sample from 2012-05-31", tg)
    # R10: nonlinearity in shock size. Is a condition effect only "large shocks pass through differently"?
    D <- prep(tg, REPS); D$abs_s <- abs(D$s)
    rob[[length(rob) + 1]] <- pick(lm(update(hyp_A(), . ~ . + abs_s + s:abs_s), D), "R10 add s x |s|", tg,
                                   c(paste0("s:", REPS), "s:abs_s"))
    rob[[length(rob) + 1]] <- pick(lm(update(hyp_A(asym_f = TRUE), . ~ . + abs_s + s:abs_s), D), "R10 add s x |s|", tg,
                                   paste0(c("sp", "sm"), ":", REPS["H2"]))
    # R11: year-specific spillover (year dummies and s x year replace trend and s x trend).
    # Identifies the condition effects from day-to-day variation within each year only.
    D$yr <- factor(format(D$date, "%Y"))
    rob[[length(rob) + 1]] <- pick(lm(update(hyp_A(trend = FALSE), . ~ . + yr + s:yr), D), "R11 within-year (s x year)", tg)
    rob[[length(rob) + 1]] <- pick(lm(update(hyp_A(trend = FALSE, asym_f = TRUE), . ~ . + yr + s:yr), D),
                                   "R11 within-year (s x year)", tg, paste0(c("sp", "sm"), ":", REPS["H2"]))
  }
  # R2: 10y shock split into expectations (s - s_tp) and term premium (s_tp), KW and ACM
  for (tp in c("tp_kw", "tp_acm")) {
    D <- prep("k10", REPS); D$s_tp <- A[[paste0("s_", tp)]][match(D$date, A$date)]; D$s_exp <- D$s - D$s_tp
    m <- lm(hyp_A(shocks = c("s_exp", "s_tp"), trend = FALSE), D)
    rob[[length(rob) + 1]] <- pick(m, paste0("R2 split by ", tp), "k10", c("s_exp", "s_tp", as.vector(outer(c("s_exp", "s_tp"), REPS, paste, sep = ":"))))
  }
  rob_tab <- do.call(rbind, rob); res$robustness <- rob_tab; save_tab(rob_tab, "table6_robustness")

  # R12 (design v6): fut3 z-score window 120 / 500 days instead of 250, part 1 specs A and B (no mon)
  r12 <- list()
  for (tg in names(TARGETS)) for (zw in c("120", "500")) {
    Atmp <- A; A$fut3_cum <- A[[paste0("fut3_cum_", zw)]]; D12 <- prep(tg, conds = REPS); A <- Atmp
    for (k in 1:2) { sp <- SPECS[k, ]; r <- part1_rows(fit_part1(D12, sp$trend, FALSE))
      r12[[length(r12) + 1]] <- data.frame(target = tg, z_window = zw, spec = sp$spec, r,
                                           hyp = names(P1_TERMS)[match(r$term, P1_TERMS)], n = nrow(D12), from = min(D12$date), row.names = NULL) }
  }
  r12 <- do.call(rbind, r12)
  r12$p_holm <- ave(r12$p, paste(r12$z_window, r12$spec), FUN = function(p) p.adjust(p, "holm"))
  res$r12 <- r12; save_tab(r12, "table6b_r12_zwindow")

  # R13 (exploratory, added 11-24 after the main results): each remaining daily candidate added
  # to the three representative conditions as a fourth s x condition term, specs A and B (no mon).
  # Holm over 6 candidates x 2 maturities within each spec, separate from the part 1 family.
  R13_CANDS <- c("krw_chg20", "fx_vol", "policy_gap", "fgn_share", "baa_spread", "fut10_cum")
  r13 <- do.call(rbind, lapply(names(TARGETS), function(tg) do.call(rbind, lapply(R13_CANDS, function(cv)
    do.call(rbind, lapply(1:2, function(k) { sp <- SPECS[k, ]
      D <- prep(tg, conds = c(REPS, cv))
      m <- lm(hyp_formula(conds = c(REPS, cv), trend = sp$trend, shock_type = FALSE), D); cf <- coefs(m)
      g <- function(term, col) { v <- cf[cf$term == term, col]; if (length(v)) v[1] else NA }
      data.frame(target = tg, cand = cv, spec = sp$spec, n = nobs(m), est = g(paste0("s:", cv), "est"), p = g(paste0("s:", cv), "p"),
                 vix_est = g("s:vix", "est"), vix_p = g("s:vix", "p"), cor_vix = cor(D$vix, D[[cv]])) }))))))
  r13$p_holm <- ave(r13$p, r13$spec, FUN = function(p) p.adjust(p, "holm"))
  res$r13 <- r13; save_tab(r13, "table6c_r13_exploratory")

  # R4: sup-Wald test for a break in beta0 (baseline), HAC, 15% trimming
  supw <- do.call(rbind, lapply(names(TARGETS), function(tg) {
    D <- prep(tg, conds = character(0)); Tn <- nrow(D); grid <- seq(floor(0.15 * Tn), ceiling(0.85 * Tn), by = 5)
    Wt <- sapply(grid, function(b) { D$post <- 1 * (seq_len(Tn) > b)
      m <- lm(dy ~ s + post + s:post + dy0 + hol + nous + fomc_d + mpc_d, D); V <- V_nw(m)
      coef(m)["s:post"]^2 / V["s:post", "s:post"] })
    j <- which.max(Wt); lam <- (0.85 * 0.85) / (0.15 * 0.15)   # lambda = ((1-from)*to)/(from*(1-to)) with from = .15, to = .85
    data.frame(target = tg, supW = Wt[j], break_date = D$date[grid[j]],
               p = strucchange:::pvalue.Fstats(Wt[j], type = "supF", k = 1, lambda = lam)) }))
  res$supwald <- supw; save_tab(supw, "R4_supwald")
}

# ---- 4-8. GARCH-X (volatility spillover) ------------------------------------------
if (STAGE >= 3) {
  message("4-8 GARCH-X")
  garch <- do.call(rbind, lapply(names(TARGETS), function(tg) {
    D <- prep(tg, conds = character(0))
    mx <- as.matrix(D[, c("s", "dy0", DUMMIES)]); vx <- matrix(D$s^2 / 100, ncol = 1)   # s^2 in 100 bp^2
    mk_spec <- function(vxreg) {
      sp <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = vxreg),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, external.regressors = mx),
                       distribution.model = "std")
      # the default start sits on the lower bound of c and the optimizer stays there;
      # start inside the region (c >= 0 keeps the variance positive)
      if (!is.null(vxreg)) { setbounds(sp) <- list(vxreg1 = c(0, 100)); setstart(sp) <- list(vxreg1 = 0.1) }
      sp
    }
    fit  <- ugarchfit(mk_spec(vx), D$dy, solver = "hybrid")
    fit0 <- ugarchfit(mk_spec(NULL), D$dy, solver = "hybrid")
    lr <- 2 * (likelihood(fit) - likelihood(fit0))
    rc <- fit@fit$robust.matcoef
    nm <- c(mu = "intercept", setNames(c("s", "dy0", DUMMIES), paste0("mxreg", 1:6)), vxreg1 = "c (s^2/100)")
    data.frame(target = tg, term = rownames(rc), label = ifelse(rownames(rc) %in% names(nm), nm[rownames(rc)], rownames(rc)),
               est = rc[, 1], se = rc[, 2], t = rc[, 3], p = rc[, 4], loglik = likelihood(fit), loglik_no_c = likelihood(fit0),
               lr_c = lr, lr_p = 0.5 * pchisq(lr, 1, lower.tail = FALSE),   # c on the boundary: 50:50 chi-bar
               row.names = NULL) }))
  res$garchx <- garch; save_tab(garch, "table7_garchx")

  # N3 (design v6, 3-2-1): mean equation with s, mon, s x mon; variance with c_m s^2 mon + c_g s^2 (1 - mon).
  # Signed LR test of c_m = c_g against c_m > c_g; restricted model = common c (same mean equation).
  message("  N3: GARCH-X by shock type")
  n3 <- do.call(rbind, lapply(names(TARGETS), function(tg) {
    D <- prep(tg, conds = character(0))
    mx <- as.matrix(D[, c("s", "dy0", DUMMIES, "mon", "smon")])
    vx_u <- cbind(D$s^2 * D$mon, D$s^2 * (1 - D$mon)) / 100; vx_r <- matrix(D$s^2 / 100, ncol = 1)
    spec_n3 <- function(vxreg) {
      sp <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = vxreg),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, external.regressors = mx),
                       distribution.model = "std")
      k <- ncol(vxreg); setbounds(sp) <- setNames(rep(list(c(0, 100)), k), paste0("vxreg", 1:k))
      setstart(sp) <- setNames(as.list(rep(0.1, k)), paste0("vxreg", 1:k)); sp
    }
    fu <- ugarchfit(spec_n3(vx_u), D$dy, solver = "hybrid"); fr <- ugarchfit(spec_n3(vx_r), D$dy, solver = "hybrid")
    cu <- coef(fu); rc <- fu@fit$robust.matcoef; Vr <- fu@fit$robust.cvar; ix <- match(c("vxreg1", "vxreg2"), names(cu))
    lr <- max(0, 2 * (likelihood(fu) - likelihood(fr))); dc <- cu["vxreg1"] - cu["vxreg2"]
    se_d <- sqrt(Vr[ix[1], ix[1]] + Vr[ix[2], ix[2]] - 2 * Vr[ix[1], ix[2]])
    boundary <- any(cu[c("vxreg1", "vxreg2")] < 1e-6)
    data.frame(target = tg, c_m = cu["vxreg1"], se_c_m = rc["vxreg1", 2], c_g = cu["vxreg2"], se_c_g = rc["vxreg2", 2],
               c_common = coef(fr)["vxreg1"], beta_smon_mean = cu["mxreg8"], loglik_u = likelihood(fu), loglik_r = likelihood(fr),
               lr = lr, signed_root = sign(dc) * sqrt(lr), p_one = pnorm(-sign(dc) * sqrt(lr)),
               wald_t = dc / se_d, wald_p_one = pnorm(-dc / se_d), boundary = boundary, row.names = NULL) }))
  res$n3 <- n3; save_tab(n3, "table8b_n3_garchx")
}

# ---- Part 3 Holm family (design v6, 3-3): N2, N3, N5, N7 x 2 maturities ----------
p3 <- rbind(
  data.frame(test = "N2 s x FLAT (one-sided t)", target = part3_ols$target[part3_ols$test == "N2"],
             stat = part3_ols$t[part3_ols$test == "N2"], p = part3_ols$p_one[part3_ols$test == "N2"]),
  if (exists("n3")) data.frame(test = "N3 c_m > c_g (signed LR)", target = n3$target, stat = n3$signed_root, p = n3$p_one),
  data.frame(test = "N5 M4 vs M1 (Clark-West)", target = tab5$target[tab5$comparison == "M4 vs M1 (N5)"],
             stat = tab5$cw_t[tab5$comparison == "M4 vs M1 (N5)"], p = tab5$cw_p[tab5$comparison == "M4 vs M1 (N5)"]),
  data.frame(test = "N7 M4 vs M2 (GW uncond., one-sided)", target = tab5$target[tab5$comparison == "M4 vs M2 (N7, non-nested)"],
             stat = tab5$t_uncond[tab5$comparison == "M4 vs M2 (N7, non-nested)"],
             p = pnorm(-tab5$t_uncond[tab5$comparison == "M4 vs M2 (N7, non-nested)"])))
p3$p_holm <- p.adjust(p3$p, "holm")
# footnote: one family of 14 = part 1 spec A (6) + part 3 (8)
pa <- tab_p1[tab_p1$spec == "A", ]
all14 <- c(setNames(pa$p, paste("P1 spec A", pa$hyp, pa$target)), setNames(p3$p, paste(p3$test, p3$target)))
p3$p_holm_14 <- p.adjust(all14, "holm")[paste(p3$test, p3$target)]
res$part3 <- list(table = p3, holm14 = data.frame(test = names(all14), p = all14, p_holm_14 = p.adjust(all14, "holm"), row.names = NULL))
save_tab(p3, "table8_part3_holm"); save_tab(res$part3$holm14, "table8c_holm14_footnote")

# ---- extra figures for the report (figures 4-7) -----------------------------------
message("report figures")
COL <- c("KTB 3y <- UST 2y" = "#2a78d6", "KTB 10y <- UST 10y" = "#eb6834")   # validated categorical pair
lab_tg <- function(tg) ifelse(tg == "k3", "KTB 3y <- UST 2y", "KTB 10y <- UST 10y")
theme_r <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
        legend.position = "top", legend.title = element_blank(), plot.title = element_text(face = "bold"))

# figure 4: yield levels (two panels, Korea vs US)
lv <- rbind(data.frame(panel = "3y Korea vs 2y US", date = d$date, series = "Korea", y = d$kr3y),
            data.frame(panel = "3y Korea vs 2y US", date = d$date, series = "US", y = d$us2y),
            data.frame(panel = "10y Korea vs 10y US", date = d$date, series = "Korea", y = d$kr10y),
            data.frame(panel = "10y Korea vs 10y US", date = d$date, series = "US", y = d$us10y))
lv <- lv[!is.na(lv$y) & lv$date >= START, ]; lv$panel <- factor(lv$panel, levels = c("3y Korea vs 2y US", "10y Korea vs 10y US"))
p4 <- ggplot(lv, aes(date, y, colour = series)) + geom_line(linewidth = 0.5) + facet_wrap(~ panel) +
  scale_colour_manual(values = c(Korea = "#2a78d6", US = "#eb6834")) +
  labs(x = NULL, y = "Yield (%)", title = "Figure 4. Korean and US government bond yields") + theme_r
save_fig(p4, "figure4_levels", 10, 3.6)

# figure 5: rolling 250-day spillover coefficient (dy on s)
rb <- do.call(rbind, lapply(names(TARGETS), function(tg) {
  D <- prep(tg, conds = character(0))
  b <- rollapplyr(seq_len(nrow(D)), 250, function(i) coef(lm(dy ~ s, D[i, ]))[2], fill = NA)
  data.frame(target = lab_tg(tg), date = D$date, beta = b, full = coef(base[[tg]]$model)["s"]) }))
rb <- rb[!is.na(rb$beta), ]
p5 <- ggplot(rb, aes(date, beta, colour = target)) + geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 0.6) + geom_hline(aes(yintercept = full, colour = target), linetype = "dashed", linewidth = 0.4) +
  scale_colour_manual(values = COL, breaks = names(COL)) +
  labs(x = "End of 250-day window", y = "Spillover coefficient", title = "Figure 5. Rolling 250-day spillover coefficient (dashed = full sample)") + theme_r
save_fig(p5, "figure5_rolling_beta", 10, 4)
res$rolling_beta <- rb

# figure 6: interaction coefficients with 95% HAC intervals (main vs no-trend)
cp <- tab3[tab3$test6 | tab3$term == paste0("s:", REPS["H2"]), ]
cp$label <- paste0(cp$hyp, ": ", sub("^s:|^sp:", "", cp$term), ifelse(grepl("^sp:", cp$term), " (US up days)", ""))
cp$target <- lab_tg(cp$target); cp$spec <- ifelse(cp$spec == "trend", "with s x trend (main)", "without trend")
p6 <- ggplot(cp, aes(est, label, colour = target, shape = spec)) + geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), position = position_dodge(width = 0.7), size = 0.35) +
  scale_colour_manual(values = COL, breaks = names(COL)) + scale_shape_manual(values = c(16, 1)) +
  labs(x = "Change in spillover per 1 s.d. of the condition (95% HAC interval)", y = NULL,
       title = "Figure 6. Hypothesis tests: interaction coefficients") + theme_r + guides(colour = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
save_fig(p6, "figure6_coefficients", 10, 4)

if (STAGE >= 2) {
  # figure 7: robustness of the three representative interactions
  main <- do.call(rbind, lapply(names(TARGETS), function(tg) { cf <- p1_fits[[paste(tg, "A")]]$cf   # spec A
    cf <- cf[cf$term %in% paste0("s:", REPS), ]; cf$check <- "Main"; cf$target <- tg; cf }))
  rr <- rbind(main, rob_tab[!grepl("^R2", rob_tab$check), names(main)])
  rr <- rr[grepl("^s:", rr$term) & rr$term != "s:abs_s", ]      # symmetric interactions only (s+/s- and s x |s| are in the table)
  rr$hyp <- ifelse(grepl("kr1y_gap", rr$term), "H1 (|kr1y_gap|; R7 signed)", ifelse(grepl("fut3|st_spread", rr$term), "H2 (fut3; R8 st_spread)", "H3 (VIX; R7 fx_vol)"))
  rr$target <- lab_tg(rr$target)
  rr$check[rr$check %in% c("R1 shock us10y", "R1 shock us2y")] <- "R1 swap shock (3y: US10y, 10y: US2y)"
  rr$check <- factor(rr$check, levels = rev(unique(c("Main", rr$check))))
  p7 <- ggplot(rr, aes(est, check, colour = target)) + geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_pointrange(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), position = position_dodge(width = 0.6), size = 0.25) +
    facet_wrap(~ hyp, nrow = 1) + scale_colour_manual(values = COL, breaks = names(COL)) +
    scale_x_continuous(breaks = c(-0.1, -0.05, 0, 0.05)) + theme(panel.spacing.x = unit(1.2, "lines")) +
    labs(x = "Interaction coefficient (95% HAC interval)", y = NULL, title = "Figure 7. Robustness of the interaction coefficients") + theme_r
  save_fig(p7, "figure7_robustness", 11, 5.5)
}

# ---- paper tables 1-6 (report outline, docs/조건별전이_보고서_구성안.md) ---------
# Main-text tables of the paper. Built only from res, so every number equals one in the
# tables above. Files: output/tables/paper_table*.csv; list: res$paper.
message("paper tables")
pt  <- list(); tgs <- names(TARGETS); p1t <- res$part1$table
pick1 <- function(v) if (length(v)) v[1] else NA

# Table 1: descriptive statistics (targets, shocks, three conditions, shock-type shares)
v1 <- c(paste0("dy_", sapply(TARGETS, `[[`, "dy")), paste0("s_", sapply(TARGETS, `[[`, "s")), unname(REPS))
t1 <- res$desc[match(v1, res$desc$variable), c("variable", "n", "mean", "sd", "min", "median", "max")]
share_row <- function(lbl, x) data.frame(variable = lbl, n = NA, mean = x, sd = NA, min = NA, median = NA, max = NA)
n2 <- res$part3_ols[res$part3_ols$test == "N2", ]
pt$table1 <- rbind(t1,
  share_row(paste0("mon share (", tgs, ")"), res$table2b$share_mon[match(tgs, res$table2b$target)]),
  share_row(paste0("FLAT share (", tgs, ")"), n2$share[match(tgs, n2$target)]), make.row.names = FALSE)

# Table 2: baseline spillover, with the 10 bp translation and the s.d. of the daily change
t2 <- res$table2; rownames(t2) <- NULL
t2$bp_per_10bp <- 10 * t2$beta0
dy_var <- setNames(paste0("dy_", sapply(TARGETS, `[[`, "dy")), names(TARGETS))
t2$sd_dy_bp <- res$desc$sd[match(dy_var[t2$target], res$desc$variable)]
pt$table2 <- t2

# Table 3: part 1 (pre-registered), specs A and B, plus the joint Wald test per maturity
g1 <- function(tg, h, sp, col) pick1(p1t[p1t$target == tg & p1t$hyp == h & p1t$spec == sp, col])
pt$table3 <- do.call(rbind, lapply(tgs, function(tg) do.call(rbind, lapply(names(P1_TERMS), function(h)
  data.frame(target = tg, hyp = h, term = P1_TERMS[[h]],
             est_A = g1(tg, h, "A", "est"), p_A = g1(tg, h, "A", "p"), holm_A = g1(tg, h, "A", "p_holm"),
             est_B = g1(tg, h, "B", "est"), p_B = g1(tg, h, "B", "p"), holm_B = g1(tg, h, "B", "p_holm"),
             verdict = res$part1$verdict$verdict[res$part1$verdict$target == tg & res$part1$verdict$hyp == h])))))
jw <- res$part1$joint_wald
gj <- function(tg, sp, col) pick1(jw[jw$target == tg & jw$spec == sp, col])
pt$table3_joint <- do.call(rbind, lapply(tgs, function(tg)
  data.frame(target = tg, W_A = gj(tg, "A", "joint_W"), p_A = gj(tg, "A", "joint_p"), holm_A = gj(tg, "A", "joint_p_holm"),
             W_B = gj(tg, "B", "joint_W"), p_B = gj(tg, "B", "joint_p"), holm_B = gj(tg, "B", "joint_p_holm"))))

# Table 4: part 3 Holm family of 8, with the effect each test is about
eff <- function(test, tg) {
  o <- function(cmp) pick1(res$table5$r2_oos[res$table5$target == tg & res$table5$comparison == cmp])
  switch(substr(test, 1, 2),
         N2 = c("coef. s x FLAT", pick1(n2$est[n2$target == tg])),
         N3 = c("c_m - c_g", if (!is.null(res$n3)) with(res$n3[res$n3$target == tg, ], c_m - c_g) else NA),
         N5 = c("R2_OOS of M4 vs M1", o("M4 vs M1 (N5)")),
         N7 = c("R2_OOS of M4 vs M2", o("M4 vs M2 (N7, non-nested)"))) }
p3t <- res$part3$table
ef <- t(mapply(eff, p3t$test, p3t$target))
pt$table4 <- data.frame(test = p3t$test, target = p3t$target, effect = ef[, 1], effect_value = as.numeric(ef[, 2]),
                        stat = p3t$stat, p = p3t$p, p_holm = p3t$p_holm, p_holm_14 = p3t$p_holm_14, row.names = NULL)
if (!is.null(res$n3)) pt$table4_n3 <- res$n3[, c("target", "c_m", "se_c_m", "c_g", "se_c_g", "c_common")]

# Table 5: economic size in bp per 10 bp US shock (one column per maturity)
e10 <- res$econ10; t5 <- data.frame(item = unique(e10$item))
for (tg in tgs) t5[[tg]] <- e10$bp[e10$target == tg][match(t5$item, e10$item[e10$target == tg])]
pt$table5 <- t5

# Table 6: robustness of the two 10-year results (P3 s x VIX, P2 s+ x fut3). Robustness models
# R1-R9 use the symmetric fut3 interaction (s x fut3), so it gets its own column.
if (!is.null(res$robustness)) {
  rb <- res$robustness; tg6 <- "k10"
  gr <- function(chk, term, col) pick1(rb[rb$check == chk & rb$target == tg6 & rb$term == term, col])
  rrow <- function(check, vix, fp, fs) data.frame(check = check,
    vix_est = vix[1], vix_p = vix[2], vix_holm = vix[3], fut3p_est = fp[1], fut3p_p = fp[2], fut3p_holm = fp[3],
    fut3sym_est = fs[1], fut3sym_p = fs[2])
  t6 <- lapply(c("A", "B", "C", "D"), function(sp) {
    gv <- function(term) c(g1(tg6, names(P1_TERMS)[P1_TERMS == term], sp, "est"), g1(tg6, names(P1_TERMS)[P1_TERMS == term], sp, "p"),
                           g1(tg6, names(P1_TERMS)[P1_TERMS == term], sp, "p_holm"))
    rrow(paste("Part 1 spec", sp), gv(P1_TERMS[["H3"]]), gv(P1_TERMS[["H2"]]), c(NA, NA)) })
  chks <- c("R1 shock us2y", "R1 shock us3y", "R1 shock us5y", "R3 drop FOMC +-3 days", "R4 2010-2019", "R4 2020-2026",
            "R5 winsorized 1%", "R6 5-day average conditions", "R7 signed gap, fx_vol", "R8 st_spread for H2",
            "R9 sample from 2012-05-31", "R10 add s x |s|", "R11 within-year (s x year)")
  t6 <- c(t6, lapply(chks, function(ch) rrow(ch,
    c(gr(ch, "s:vix", "est"), gr(ch, "s:vix", "p"), NA), c(gr(ch, "sp:fut3_cum", "est"), gr(ch, "sp:fut3_cum", "p"), NA),
    c(gr(ch, "s:fut3_cum", "est"), gr(ch, "s:fut3_cum", "p")))))
  if (!is.null(res$r12)) for (zw in unique(res$r12$z_window)) for (sp in c("A", "B")) {
    z <- res$r12[res$r12$target == tg6 & res$r12$z_window == zw & res$r12$spec == sp, ]
    gz <- function(term) c(pick1(z$est[z$term == term]), pick1(z$p[z$term == term]), pick1(z$p_holm[z$term == term]))
    t6[[length(t6) + 1]] <- rrow(paste0("R12 fut3 z-window ", zw, " days, spec ", sp), gz("s:vix"), gz("sp:fut3_cum"), c(NA, NA))
  }
  pt$table6 <- do.call(rbind, t6)
}
res$paper <- pt
for (nm in names(pt)) save_tab(pt[[nm]], paste0("paper_", nm))

# ---- save -------------------------------------------------------------------------
res$pairs <- A
res$session <- sessionInfo()
save(res, file = file.path(OUT, "Team_result.RData"))
message("saved ", file.path(OUT, "Team_result.RData"))
