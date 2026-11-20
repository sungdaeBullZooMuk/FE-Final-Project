# =============================================================================
# 04_explainer.R
# Beginner-friendly explanation of the whole project (Korean), from
# code/explainer_template.html with numbers filled from output/Team_result.RData.
# Run from "Final Project":  Rscript code/04_explainer.R
# Output: output/report/explainer.html and reports/연구_쉬운설명.pdf (headless Chrome).
# Not part of the submission file.
# =============================================================================
load("output/Team_result.RData")
TEMPLATE <- "code/explainer_template.html"
OUT_HTML <- "output/report/explainer.html"
OUT_PDF  <- "reports/연구_쉬운설명.pdf"
dir.create(dirname(OUT_HTML), recursive = TRUE, showWarnings = FALSE)

f  <- function(x, d = 2) formatC(x, format = "f", digits = d)
fp <- function(p) ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))
pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%")
wd <- function(d) paste0(format(d), "(", c("일", "월", "화", "수", "목", "금", "토")[as.POSIXlt(d)$wday + 1], ")")

A <- res$pairs; t2 <- res$table2; t2b <- res$table2b; t3 <- res$table3; t5 <- res$table5
es <- res$econ_size; q <- res$quintiles; rb <- res$robustness; gx <- res$garchx; hf <- res$heat_freq
g3 <- function(tg, term, col) t3[t3$target == tg & t3$spec == "trend" & t3$term == term, col]
sb <- function(tg, col) t2b[t2b$target == tg, col]
b2 <- function(tg, col) t2[t2$target == tg, col]
rq <- function(chk, tg, term, col) rb[rb$check == chk & rb$target == tg & rb$term == term, col][1]
oo <- function(tg, cmp) t5[t5$target == tg & t5$comparison == cmp, ]
esz <- function(qq) es$beta[es$target == "k10" & es$hyp == "H3" & abs(es$quantile - qq) < 1e-9]
qb <- function(k) q$beta[q$target == "k10" & q$hyp == "H3" & q$quintile == k]
hv <- function(tg, v) hf$selected[hf$target == tg & hf$interaction == v]
gc <- function(tg) gx[gx$target == tg & gx$label == "c (s^2/100)", "lr_p"]
case <- res$alignment_case

v <- list(
  today = format(Sys.Date()), n_pairs = format(nrow(A), big.mark = ","),
  per_start = format(min(A$date)), per_end = format(max(A$date1)),
  case_t = wd(case$date), case_t1 = wd(case$date1), case_s = f(case$s_us2y, 0), case_dy = f(case$dy_kr3y, 1),
  b0_3 = f(b2("k3", "beta0")), b0_10 = f(b2("k10", "beta0")), t0_3 = f(b2("k3", "t_beta0"), 1), t0_10 = f(b2("k10", "t_beta0"), 1),
  bup_3 = f(b2("k3", "beta_up")), bdn_3 = f(b2("k3", "beta_down")), bup_10 = f(b2("k10", "beta_up")), bdn_10 = f(b2("k10", "beta_down")),
  brk3 = format(res$supwald$break_date[res$supwald$target == "k3"]), brk10 = format(res$supwald$break_date[res$supwald$target == "k10"]),
  g_3 = f(sb("k3", "beta_growth")), m_3 = f(sb("k3", "beta_monetary")), td_3 = f(sb("k3", "t_diff"), 1), share_3 = pct(sb("k3", "share_mon")),
  g_10 = f(sb("k10", "beta_growth")), m_10 = f(sb("k10", "beta_monetary")), td_10 = f(sb("k10", "t_diff"), 1), share_10 = pct(sb("k10", "share_mon")),
  pct_10 = f(100 * sb("k10", "diff") / sb("k10", "beta_growth"), 0), corr_10 = f(sb("k10", "corr_mon_vix"), 2),
  h1_3 = f(g3("k3", "s:abs_kr1y_gap", "est"), 3), h1_3_p = fp(g3("k3", "s:abs_kr1y_gap", "p")),
  h1_10 = f(g3("k10", "s:abs_kr1y_gap", "est"), 3), h1_10_p = fp(g3("k10", "s:abs_kr1y_gap", "p")), h1_10_holm = fp(g3("k10", "s:abs_kr1y_gap", "p_holm")),
  h2_3 = f(g3("k3", "sp:fut3_cum", "est"), 3), h2_3_p = fp(g3("k3", "sp:fut3_cum", "p")),
  h2_10 = f(g3("k10", "sp:fut3_cum", "est"), 3), h2_10_p = fp(g3("k10", "sp:fut3_cum", "p")), h2_10_holm = fp(g3("k10", "sp:fut3_cum", "p_holm")),
  h3_3 = f(g3("k3", "s:vix", "est"), 3), h3_3_p = fp(g3("k3", "s:vix", "p")),
  h3_10 = f(g3("k10", "s:vix", "est"), 3), h3_10_p = fp(g3("k10", "s:vix", "p")), h3_10_holm = fp(g3("k10", "s:vix", "p_holm")),
  es_vix20 = f(esz(0.2)), es_vix80 = f(esz(0.8)), q1_vix = f(qb(1)), q5_vix = f(qb(5)),
  q_vix_p = fp(q$p_q5_q1[q$target == "k10" & q$hyp == "H3"][1]),
  r11_h2 = f(rq("R11 within-year (s x year)", "k10", "sp:fut3_cum", "est"), 3), r11_h2_p = fp(rq("R11 within-year (s x year)", "k10", "sp:fut3_cum", "p")),
  r11_vix = f(rq("R11 within-year (s x year)", "k10", "s:vix", "est"), 3), r11_vix_p = fp(rq("R11 within-year (s x year)", "k10", "s:vix", "p")),
  r4_h1 = f(rq("R4 2010-2019", "k10", "s:abs_kr1y_gap", "est"), 3), r4_vix = f(rq("R4 2020-2026", "k10", "s:vix", "est"), 3),
  heat_vix = pct(hv("k10", "vix")), heat_fut3 = pct(hv("k3", "fut3_cum")),
  oos_m1_3 = f(oo("k3", "M1 vs M0")$r2_oos), oos_m1_10 = f(oo("k10", "M1 vs M0")$r2_oos),
  oos_m2_3 = f(oo("k3", "M2 vs M1")$r2_oos, 3), oos_m2_10 = f(oo("k10", "M2 vs M1")$r2_oos, 3), cw_10 = fp(oo("k10", "M2 vs M1")$cw_p),
  oos_m3_3 = f(oo("k3", "M3 vs M1")$r2_oos, 3), oos_m3_10 = f(oo("k10", "M3 vs M1")$r2_oos, 3),
  c10p = fp(gc("k10")), c3p = fp(gc("k3"))
)
stopifnot(all(lengths(v) == 1), !any(is.na(unlist(v))))

html <- paste(readLines(TEMPLATE, encoding = "UTF-8"), collapse = "\n")
for (k in names(v)) html <- gsub(paste0("{{", k, "}}"), v[[k]], html, fixed = TRUE)
left <- regmatches(html, gregexpr("\\{\\{[a-z0-9_]+\\}\\}", html))[[1]]
if (length(left)) stop("unfilled placeholders: ", paste(unique(left), collapse = ", "))
writeLines(html, OUT_HTML, useBytes = TRUE)
message("wrote ", OUT_HTML)

chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if (file.exists(chrome)) {
  args <- c("--headless=new", "--disable-gpu", "--no-pdf-header-footer", "--allow-file-access-from-files",
            paste0("--print-to-pdf=", shQuote(normalizePath(OUT_PDF, mustWork = FALSE))),
            shQuote(paste0("file://", normalizePath(OUT_HTML))))
  system2(chrome, args, stdout = FALSE, stderr = FALSE)
  message("wrote ", OUT_PDF)
}
