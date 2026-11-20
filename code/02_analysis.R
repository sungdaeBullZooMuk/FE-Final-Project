# =============================================================================
# 02_analysis.R
# FE Final Project: state-dependent spillovers from US Treasury yields to
# Korean government bond yields. Implements section 4 of
# 조건별전이_연구개요_확정.md on data/Data_team.csv.
#
# Run from the "Final Project" folder:
#   Rscript code/02_analysis.R            # everything
#   STAGE=1 Rscript code/02_analysis.R    # 1st-round analyses only (4-1..4-5 M0-M2, 4-7)
#
# Outputs: output/Team_result.RData (rename to <team>_result.RData),
#          output/tables/*.csv, output/figures/*.png
#
# Decisions fixed before estimation (see 조건별전이_데이터전처리.md and the data review):
#   - All tests are two-sided. The outline's predicted signs are reported next to
#     each estimate, but a significant opposite sign is read as evidence, not a null.
#   - Main hypothesis model adds trend and s x trend (trend = standardized date).
#     The spillover beta trends up over the sample, so slow-moving conditions would
#     otherwise pick up the trend. The no-trend model is reported alongside.
#   - Forecasting models (4-5) do not use the trend: a 1,000-day window does not
#     support extrapolating it.
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
DATA    <- "data/Data_team.csv"
OUT     <- "output"
dir.create(file.path(OUT, "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT, "figures"), recursive = TRUE, showWarnings = FALSE)
res <- list(settings = list(L_NW = L_NW, W = W, STEP_H = STEP_H, START = START, START10 = START10))

TARGETS <- list(k3 = c(dy = "kr3y", s = "us2y"), k10 = c(dy = "kr10y", s = "us10y"))
REPS    <- c(H1 = "abs_kr1y_gap", H2 = "fut3_cum", H3 = "vix")
PRED    <- c(H1 = "+", H2 = "+ (rising US yields)", H3 = "+")
CANDS   <- c("abs_kr1y_gap", "cd_gap", "policy_gap", "days_to_mpc",            # H1
             "fut3_cum", "fut10_cum", "st_spread", "fgn_share",                # H2
             "vix", "fx_vol", "vkospi", "usd_idx", "baa_spread")               # H3
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
for (v in c("us2y", "us5y", "us10y", "tp_kw", "tp_acm"))
  us[[paste0("d_", v)]] <- c(NA, 100 * diff(us[[v]]))

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
for (v in c("us2y", "us5y", "us10y", "tp_kw", "tp_acm")) A[[paste0("s_", v)]] <- sum_us(us[[paste0("d_", v)]])
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
cum_z  <- function(x) { cs <- cumsum(ifelse(is.na(x), 0, x)); cs[is.na(x)] <- NA; roll_z(cs) }

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
f10 <- kr$fut10_net; f10[kr$date < as.Date("2011-06-01")] <- NA
z10 <- rep(NA_real_, n); ok10 <- kr$date >= as.Date("2011-06-01"); z10[ok10] <- cum_z(f10[ok10])
A$fut10_cum <- z10[-n]
A$st_spread <- k$cd91 - known(d$date, d$us3m)
A$fgn_share <- k$fgn_share
A$vix       <- log(known(d$date, d$vix))
A$fx_vol    <- (rollapplyr(c(NA, diff(log(kr$krw_usd))), 20, sd, fill = NA) * sqrt(250))[-n]
A$vkospi    <- k$vkospi
A$usd_idx   <- known(d$date, d$usd_idx)
A$baa_spread <- known(d$date, d$baa_spread)

# alignment checks (outline 3, appendix B)
i928 <- which(A$date1 == as.Date("2026-09-28"))
stopifnot(length(i928) == 1, A$n_us[i928] == 3, A$hol[i928] == 1,
          abs(A$s_us2y[i928] - 100 * (us$us2y[us$date == as.Date("2026-09-25")] -
                                        us$us2y[us$date == as.Date("2026-09-22")])) < 1e-8)
stopifnot(all(A$date1 > A$date), all(A$n_us >= 0), !anyNA(A$s_us2y[A$date >= START]))
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
  for (v in conds) D[[v]] <- A[[v]]
  D <- D[D$date >= from, ]
  if (!is.null(filter)) D <- D[filter(D), ]
  D <- D[complete.cases(D[, c("dy", "dy0", "s", conds)]), ]
  for (v in conds) D[[v]] <- zs(D[[v]])
  D$sp <- pmax(D$s, 0); D$sm <- pmin(D$s, 0)
  D
}
hyp_formula <- function(shocks = "s", conds = REPS, trend = TRUE, asym_f = FALSE) {
  inter <- as.vector(outer(shocks, conds, paste, sep = ":"))
  terms <- c(shocks, conds, inter, "dy0", DUMMIES)
  if (asym_f) {                     # H2 split: s+ and s- each interacted with fut3_cum
    terms <- c("sp", "sm", conds, paste0("s:", setdiff(conds, REPS["H2"])),
               paste0(c("sp", "sm"), ":", REPS["H2"]), "dy0", DUMMIES)
  }
  if (trend) terms <- c(terms, "trend", paste0(if (asym_f) "s" else shocks, ":trend"))
  reformulate(terms, "dy")
}

# ---- 4-1. basics: descriptive statistics, unit roots (table 1, appendix A) --------
message("4-1 basics")
desc_vars <- c(dy_kr3y = "dy_kr3y", dy_kr10y = "dy_kr10y", s_us2y = "s_us2y", s_us10y = "s_us10y", setNames(CANDS, CANDS))
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
  do.call(rbind, lapply(CANDS, function(v) ur_row(v, A[[v]]))))
# outline 4-1: a condition that looks I(1) (ADF does not reject AND KPSS rejects) is
# replaced by its 250-day rolling z-score (same detrending as the futures positions)
ur$I1 <- !ur$adf_reject & ur$kpss_reject
transformed <- intersect(ur$series[ur$I1], CANDS)
for (v in transformed) {                      # rolling z on the full calendar (2009 included)
  A[[paste0(v, "_raw")]] <- A[[v]]
  A[[v]] <- roll_z(A_all[[v]])[match(A$date, A_all$date)]
}
ur$used_as <- ifelse(ur$series %in% transformed, "250-day rolling z", ifelse(ur$series %in% CANDS, "level", ""))
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
  base[[tg]] <- list(model = m, coef = coefs(m, V), r2 = summary(m)$r.squared, n = nobs(m),
                     asym = coefs(ma, Va), asym_test = wald(ma, Va, R))
}
tab2 <- do.call(rbind, lapply(names(base), function(tg) { b <- base[[tg]]
  cb <- b$coef; ca <- b$asym
  data.frame(target = tg, beta0 = cb$est[cb$term == "s"], t_beta0 = cb$t[cb$term == "s"],
             rho = cb$est[cb$term == "dy0"], t_rho = cb$t[cb$term == "dy0"], r2 = b$r2, n = b$n,
             beta_up = ca$est[ca$term == "sp"], beta_down = ca$est[ca$term == "sm"],
             wald_up_eq_down = b$asym_test["W"], p_up_eq_down = b$asym_test["p"]) }))
res$baseline <- base; res$table2 <- tab2; save_tab(tab2, "table2_baseline")

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
      g <- setNames(rep(0, length(b)), names(b)); g["s"] <- 1; g[term] <- zq
      est <- sum(g * b); se <- sqrt(as.numeric(t(g) %*% V %*% g))
      data.frame(target = tg, hyp = h, condition = v, quantile = q, z = zq, beta = est, lo = est - 1.96 * se, hi = est + 1.96 * se) })) }))
  # figure 1: quintile betas, b5 - b1 = 0
  quint <- do.call(rbind, lapply(names(REPS), function(h) { v <- REPS[h]
    Q <- cut(D[[v]], quantile(D[[v]], 0:5 / 5), include.lowest = TRUE, labels = paste0("q", 1:5))
    X <- data.frame(D, Q = Q)
    m <- lm(dy ~ 0 + Q + Q:s + dy0 + hol + nous + fomc_d + mpc_d + trend + s:trend, X); V <- V_nw(m)
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
  rows <- rbind(f$coef[f$coef$term %in% c("s", paste0("s:", REPS[c("H1", "H3")]), paste0("s:", REPS["H2"])), ],
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

# ---- 4-4. LASSO / Elastic Net (full sample, figure 2) ------------------------------
lasso_design <- function(D) {
  inter <- sapply(CANDS, function(v) D$s * D[[v]]); colnames(inter) <- paste0("s:", CANDS)
  X <- cbind(s = D$s, dy0 = D$dy0, as.matrix(D[, DUMMIES]), as.matrix(D[, CANDS]), inter)
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
  lasso[[tg]] <- list(n = nrow(D), lasso = la, enet = en,
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
roll_ols <- function(D, conds) {
  base_cols <- c("s", "dy0", DUMMIES); fc <- rep(NA_real_, nrow(D))
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
  L <- lasso_design(D); fc <- rep(NA_real_, nrow(D)); ci <- match(CANDS, colnames(L$X))
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
  ok <- !is.na(f1)
  y <- D$dy[ok]
  cmp <- rbind(
    data.frame(target = tg, comparison = "M1 vs M0", t(oos_stats(y, f0[ok], f1[ok])), cw_t = NA, cw_p = NA,
               t(gw_sq(y, f0[ok], f1[ok])), gw_course_p = gw.test(f0[ok], f1[ok], y, T = length(y), tau = 1)$p.value, dm_p = NA),
    data.frame(target = tg, comparison = "M2 vs M1", t(oos_stats(y, f1[ok], f2[ok])), t(setNames(clark_west(y, f1[ok], f2[ok]), c("cw_t", "cw_p"))),
               t(gw_sq(y, f1[ok], f2[ok])), gw_course_p = gw.test(f1[ok], f2[ok], y, T = length(y), tau = 1)$p.value, dm_p = NA))
  fc <- data.frame(date = D$date, dy = D$dy, M0 = f0, M1 = f1, M2 = f2)
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
message("4-7 diagnostics")
arch_lm <- function(e, q = 5) { e2 <- e^2; X <- embed(e2, q + 1); m <- lm(X[, 1] ~ X[, -1])
  stat <- nrow(X) * summary(m)$r.squared; c(stat = stat, p = pchisq(stat, q, lower.tail = FALSE)) }
diag <- do.call(rbind, lapply(names(TARGETS), function(tg) {
  f <- hyp[[tg]]$fits$trend; e <- residuals(f$model)
  lb <- Box.test(e, lag = 10, type = "Ljung-Box"); ar <- arch_lm(e)
  D <- prep(tg, conds = REPS); cut99 <- quantile(abs(D$s), 0.99)
  m_inf <- lm(hyp_formula(), D[abs(D$s) <= cut99, ]); ci <- coefs(m_inf)
  vif <- sapply(REPS, function(v) 1 / (1 - summary(lm(reformulate(setdiff(REPS, v), v), D))$r.squared))
  data.frame(target = tg, ljung_box_q10 = unname(lb$statistic), lb_p = lb$p.value, arch_lm5 = ar["stat"], arch_p = ar["p"],
             t(setNames(ci$est[ci$term %in% paste0("s:", REPS)], paste0("drop_top1pct_", names(REPS)))),
             t(setNames(vif, paste0("vif_", names(REPS)))), row.names = NULL) }))
res$diagnostics <- diag; save_tab(diag, "diagnostics")
# Ljung-Box significant -> outline says add a second lag of dy; report that model too
lag2 <- lapply(names(TARGETS), function(tg) {
  D <- prep(tg, conds = REPS); D$dy0b <- c(NA, head(D$dy0, -1))
  m <- lm(update(hyp_formula(), . ~ . + dy0b), D); cf <- coefs(m); cf$target <- tg; cf[cf$term %in% paste0("s:", REPS), ] })
res$lag2 <- do.call(rbind, lag2)

# ---- 4-6. robustness (table 6, appendix D) -----------------------------------------
if (STAGE >= 2) {
  message("4-6 robustness")
  pick <- function(m, label, tg, terms = paste0("s:", REPS)) { cf <- coefs(m); cf <- cf[cf$term %in% terms, ]
    if (nrow(cf)) { cf$check <- label; cf$target <- tg }; cf }
  rob <- list()
  for (tg in names(TARGETS)) {
    # R1: alternative shock maturities
    for (sh in c("us5y", "us10y", "us2y")) if (sh != TARGETS[[tg]]["s"])
      rob[[length(rob) + 1]] <- pick(lm(hyp_formula(), prep(tg, REPS, shock = sh)), paste0("R1 shock ", sh), tg)
    # R3: drop pairs within 3 Korean trading days of an FOMC shock
    fom <- which(A$fomc_d == 1); near <- unique(unlist(lapply(fom, function(i) (i - 3):(i + 3))))
    keep_dates <- A$date[-near[near >= 1 & near <= nrow(A)]]
    rob[[length(rob) + 1]] <- pick(lm(hyp_formula(), prep(tg, REPS, filter = function(D) D$date %in% keep_dates)), "R3 drop FOMC +-3 days", tg)
    # R4: split samples
    rob[[length(rob) + 1]] <- pick(lm(hyp_formula(trend = FALSE), prep(tg, REPS, filter = function(D) D$date < as.Date("2020-01-01"))), "R4 2010-2019", tg)
    rob[[length(rob) + 1]] <- pick(lm(hyp_formula(trend = FALSE), prep(tg, REPS, filter = function(D) D$date >= as.Date("2020-01-01"))), "R4 2020-2026", tg)
    # R5: winsorize dy, dy0, s at 1% / 99%
    D <- prep(tg, REPS); wz <- function(x) { q <- quantile(x, c(.01, .99)); pmin(pmax(x, q[1]), q[2]) }
    D$dy <- wz(D$dy); D$dy0 <- wz(D$dy0); D$s <- wz(D$s)
    rob[[length(rob) + 1]] <- pick(lm(hyp_formula(), D), "R5 winsorized 1%", tg)
    # R6: conditions as 5-day averages (up to t)
    A5 <- A; for (v in REPS) A5[[v]] <- rollapplyr(A[[v]], 5, mean, fill = NA)
    Atmp <- A; A <- A5; D6 <- prep(tg, REPS); A <- Atmp
    rob[[length(rob) + 1]] <- pick(lm(hyp_formula(), D6), "R6 5-day average conditions", tg)
    # R7: signed kr1y_gap for H1; fx_vol for H3
    alt <- c(H1 = "kr1y_gap", H2 = REPS[["H2"]], H3 = "fx_vol")
    rob[[length(rob) + 1]] <- pick(lm(hyp_formula(conds = alt), prep(tg, alt)), "R7 signed gap, fx_vol", tg, paste0("s:", alt))
    # R8: st_spread replaces fut3_cum
    alt8 <- c(H1 = REPS[["H1"]], H2 = "st_spread", H3 = REPS[["H3"]])
    rob[[length(rob) + 1]] <- pick(lm(hyp_formula(conds = alt8), prep(tg, alt8)), "R8 st_spread for H2", tg, paste0("s:", alt8))
    # R9 (outline 2-5): hypothesis model on the 2012-05-31 sample
    rob[[length(rob) + 1]] <- pick(lm(hyp_formula(), prep(tg, REPS, from = START10)), "R9 sample from 2012-05-31", tg)
    # R10: nonlinearity in shock size. Is a condition effect only "large shocks pass through differently"?
    D <- prep(tg, REPS); D$abs_s <- abs(D$s)
    rob[[length(rob) + 1]] <- pick(lm(update(hyp_formula(), . ~ . + abs_s + s:abs_s), D), "R10 add s x |s|", tg,
                                   c(paste0("s:", REPS), "s:abs_s"))
    rob[[length(rob) + 1]] <- pick(lm(update(hyp_formula(asym_f = TRUE), . ~ . + abs_s + s:abs_s), D), "R10 add s x |s|", tg,
                                   paste0(c("sp", "sm"), ":", REPS["H2"]))
    # R11: year-specific spillover (year dummies and s x year replace trend and s x trend).
    # Identifies the condition effects from day-to-day variation within each year only.
    D$yr <- factor(format(D$date, "%Y"))
    rob[[length(rob) + 1]] <- pick(lm(update(hyp_formula(trend = FALSE), . ~ . + yr + s:yr), D), "R11 within-year (s x year)", tg)
    rob[[length(rob) + 1]] <- pick(lm(update(hyp_formula(trend = FALSE, asym_f = TRUE), . ~ . + yr + s:yr), D),
                                   "R11 within-year (s x year)", tg, paste0(c("sp", "sm"), ":", REPS["H2"]))
  }
  # R2: 10y shock split into expectations (s - s_tp) and term premium (s_tp), KW and ACM
  for (tp in c("tp_kw", "tp_acm")) {
    D <- prep("k10", REPS); D$s_tp <- A[[paste0("s_", tp)]][match(D$date, A$date)]; D$s_exp <- D$s - D$s_tp
    m <- lm(hyp_formula(shocks = c("s_exp", "s_tp"), trend = FALSE), D)
    rob[[length(rob) + 1]] <- pick(m, paste0("R2 split by ", tp), "k10", c("s_exp", "s_tp", as.vector(outer(c("s_exp", "s_tp"), REPS, paste, sep = ":"))))
  }
  rob_tab <- do.call(rbind, rob); res$robustness <- rob_tab; save_tab(rob_tab, "table6_robustness")

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
}

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
  main <- do.call(rbind, lapply(names(TARGETS), function(tg) { cf <- hyp[[tg]]$fits$trend$coef
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

# ---- save -------------------------------------------------------------------------
res$pairs <- A
res$session <- sessionInfo()
save(res, file = file.path(OUT, "Team_result.RData"))
message("saved ", file.path(OUT, "Team_result.RData"))
