# =============================================================================
# 04_explainer.R
# Beginner-friendly explanation of the whole project (Korean), from
# code/explainer_template.html with numbers filled from output/Team_result.RData.
# Uses the same result objects as the paper draft (design v8: the rising US share and its
# decomposition as the core; v6 conditions and shock type, v7 volatility as supporting analyses).
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
fp <- function(p) ifelse(p < 0.0001, "<0.0001", ifelse(p < 0.001, formatC(p, format = "f", digits = 4), formatC(p, format = "f", digits = 3)))
pct <- function(x, d = 0) paste0(formatC(100 * x, format = "f", digits = d), "%")
wd <- function(d) paste0(format(d), "(", c("일", "월", "화", "수", "목", "금", "토")[as.POSIXlt(d)$wday + 1], ")")

A <- res$pairs; t2 <- res$paper$table2; t2b <- res$table2b; p1 <- res$paper$table3; jw <- res$paper$table3_joint
t4 <- res$paper$table4; n3 <- res$paper$table4_n3; t5 <- res$table5; e5 <- res$paper$table5
p1t <- res$part1$table; q <- res$quintiles; rb <- res$robustness; gx <- res$garchx; hf <- res$heat_freq
gr <- res$granger; r13 <- res$r13; case <- res$alignment_case

b2  <- function(tg, col) t2[t2$target == tg, col]
sb  <- function(tg, col) t2b[t2b$target == tg, col]
rq  <- function(chk, tg, term, col) rb[rb$check == chk & rb$target == tg & rb$term == term, col][1]
oo  <- function(tg, cmp) t5[t5$target == tg & t5$comparison == cmp, ]
qb  <- function(k) q$beta[q$target == "k10" & q$hyp == "H3" & q$quintile == k]
hv  <- function(tg, v) hf$selected[hf$target == tg & hf$interaction == v]
gc  <- function(tg) gx[gx$target == tg & gx$label == "c (s^2/100)", "lr_p"]
grr <- function(lag, col) gr[gr$target == "k10" & gr$lags == lag, col]
p1s <- function(tg, h, sp, col) p1t[p1t$target == tg & p1t$hyp == h & p1t$spec == sp, col]
e5v <- function(item) e5$k10[e5$item == item]
r13v <- function(cand, col) r13[r13$target == "k10" & r13$cand == cand & r13$spec == "A", col]

# part 1 table rows (design v6 verdict rule: Holm p < 0.05 in both A and B)
HYP_KO <- c(H1 = "P1 정책 기대 (s×G)", H2 = "P2 외국인 수급 (s⁺×F)", H3 = "P3 위험회피 (s×V)")
VERD_KO <- c("significant (opposite sign)" = "<b>유의 (예측과 반대 부호)</b>", "significant (predicted sign)" = "<b>유의 (예측 부호)</b>",
             "spec-dependent" = "설정 의존", "not rejected" = "기각 못 함")
bold <- function(x, p) ifelse(p < 0.05, paste0("<b>", x, "</b>"), x)
p1_rows <- paste(apply(p1, 1, function(r) {
  pa <- as.numeric(r["holm_A"]); pb <- as.numeric(r["holm_B"])
  paste0("<tr><td>", ifelse(r["target"] == "k3", "3년물", "10년물"), "</td><td>", HYP_KO[r["hyp"]], "</td>",
         "<td class='n'>", f(as.numeric(r["est_A"]), 3), "</td><td class='n'>", bold(fp(pa), pa), "</td>",
         "<td class='n'>", f(as.numeric(r["est_B"]), 3), "</td><td class='n'>", bold(fp(pb), pb), "</td>",
         "<td>", VERD_KO[r["verdict"]], "</td></tr>") }), collapse = "\n")

# part 3 N-test rows
N_KO <- c(N2 = "수익률곡선 분류: s×FLAT 계수", N3 = "변동성: c_m − c_g (GARCH-X)", N5 = "표본 밖: M4 vs M1 R²_OOS", N7 = "표본 밖: M4 vs M2 R²_OOS")
n_rows <- paste(sapply(c("N2", "N3", "N5", "N7"), function(n) {
  a <- t4[startsWith(t4$test, n) & t4$target == "k3", ]; b <- t4[startsWith(t4$test, n) & t4$target == "k10", ]
  val <- if (n %in% c("N5", "N7")) pct(b$effect_value, 2) else f(b$effect_value, 3)
  paste0("<tr><td>", n, "</td><td>", N_KO[n], "</td><td class='n'>", fp(a$p_holm), "</td><td class='n'>", val,
         "</td><td class='n'>", bold(fp(b$p_holm), b$p_holm), "</td><td>", ifelse(b$p_holm < 0.05, "<b>통과</b>", "보정 후 유의하지 않음"), "</td></tr>") }),
  collapse = "\n")

# part 4 (design v7): confirmatory V1-V4, VaR, multi-horizon, VAR, cointegration
p4 <- res$part4; p4t <- p4$tests
V_KO <- c(V1 = "V1 분산식에 log VIX (d &gt; 0)", V2 = "V2 VIX 높은 날·낮은 날 c 차이",
          V3 = "V3 미국 충격으로 변동성 예측 개선", V4 = "V4 VIX를 더해 변동성 예측 개선")
V_UNIT <- c(V1 = "d", V2 = "c_hi − c_lo", V3 = "QLIKE 개선", V4 = "QLIKE 개선")
v_rows <- paste(sapply(names(V_KO), function(k) {
  a <- p4t[startsWith(p4t$test, k) & p4t$target == "k3", ]; b <- p4t[startsWith(p4t$test, k) & p4t$target == "k10", ]
  ef <- function(r) if (k %in% c("V3", "V4")) pct(r$effect_value, 2) else f(r$effect_value, 2)
  paste0("<tr><td>", V_KO[k], "</td><td>", V_UNIT[k], "</td><td class='n'>", ef(a), "</td><td class='n'>", fp(a$p),
         "</td><td class='n'>", ef(b), "</td><td class='n'>", fp(b$p), "</td><td>확인 못 함 (Holm ", fp(max(p4t$p_holm)), ")</td></tr>") }),
  collapse = "\n")
vr <- p4$var
var_rows <- paste(sapply(c("RiskMetrics", "G0", "G1", "G2"), function(m) {
  g <- function(tg, lv, col) vr[vr$target == tg & vr$level == lv & vr$model == m, col]
  cell <- function(tg, lv) paste0("<td class='n'>", pct(g(tg, lv, "rate"), 1), " <span class='note'>(p ", fp(g(tg, lv, "kupiec_p")), ")</span></td>")
  lab <- c(RiskMetrics = "RiskMetrics (λ = 0.94)", G0 = "G0 GARCH(1,1)-t", G1 = "G1 + 미국 충격²", G2 = "G2 + 미국 충격² + log VIX")[m]
  paste0("<tr><td>", lab, "</td>", cell("k3", 0.01), cell("k3", 0.05), cell("k10", 0.01), cell("k10", 0.05), "</tr>") }),
  collapse = "\n")
p4c  <- function(tg) p4$garchx[p4$garchx$target == tg & p4$garchx$model == "GARCH-X: c" & p4$garchx$term == "vxreg1", "est"]
mh   <- function(tg, h, col) p4$multi_horizon[p4$multi_horizon$target == tg & p4$multi_horizon$h == h, col]
fv   <- p4$var_model$fevd; fus <- function(r) { x <- fv[fv$response == r & fv$h == 10, ]; x$us2 + x$us10 }
rf   <- p4$var_model$rolling_fevd; ci <- p4$coint
sup  <- function(chk, tg) p4$supp[startsWith(p4$supp$check, chk) & p4$supp$target == tg, "p"]
cot  <- function(pr, col) ci[ci$pair == pr, col]

# design v8 core: US share by period, logit decomposition, block bootstrap tests
p5 <- res$part5; ps5 <- p5$period_stats; t5x <- p5$tests; vf5 <- p5$var_fevd; rb5 <- p5$robustness
psv <- function(tg, k, col) ps5[ps5$target == tg, col][k]
dcv <- function(tg, col) p5$decomposition[p5$decomposition$target == tg, col]
fpb <- function(p) ifelse(p < 0.001, formatC(p, format = "f", digits = 4), formatC(p, format = "f", digits = 3))
per_rows <- paste(sapply(c("k3", "k10"), function(tg) paste(sapply(1:2, function(k) {
  v <- vf5[vf5$response == sub("k", "kr", tg), ]
  paste0("<tr><td>", if (k == 1) ifelse(tg == "k3", "3년물", "10년물") else "", "</td><td>", c("2010–2019", "2020–2026")[k],
         "</td><td class='n'>", f(psv(tg, k, "beta"), 3), "</td><td class='n'>", f(sqrt(psv(tg, k, "var_s")), 1),
         "</td><td class='n'>", f(sqrt(psv(tg, k, "var_e")), 1), "</td><td class='n'><b>", pct(psv(tg, k, "vr"), 1),
         "</b></td><td class='n'>", pct(c(v$share_early, v$share_late)[k], 1), "</td></tr>") }), collapse = "\n")), collapse = "\n")
L_KO <- c(L1 = "L1 미국 몫 증가", L2 = "L2 충격 확대 C_s", L3 = "L3 전이 강화 C_β", L4 = "L4 국내 변동 C_e", L5 = "L5 충격 확대 − 전이 강화")
l_rows <- paste(sapply(names(L_KO), function(k) {
  cell <- function(tg) { r <- t5x[startsWith(t5x$test, k) & t5x$target == tg, ]
    val <- if (k == "L1") paste0("+", f(100 * r$estimate, 1), "%p") else f(r$estimate, 2)
    ci <- if (k == "L1") paste0("[", f(100 * r$lo, 1), ", ", f(100 * r$hi, 1), "]") else paste0("[", f(r$lo, 2), ", ", f(r$hi, 2), "]")
    hp <- if (is.na(r$p_holm)) paste0("(", fpb(r$p), ")") else bold(fpb(r$p_holm), r$p_holm)
    paste0("<td class='n'>", val, "</td><td class='n'>", ci, "</td><td class='n'>", hp, "</td>") }
  paste0("<tr><td>", L_KO[k], "</td>", cell("k3"), cell("k10"), "</tr>") }), collapse = "\n")

jwr <- function(tg) { p <- unlist(jw[jw$target == tg, c("p_A", "p_B")]); r <- unique(fp(range(p))); paste(r, collapse = "–") }

v <- list(
  today = format(Sys.Date()), n_pairs = format(nrow(A), big.mark = ","),
  per_start = format(min(A$date)), per_end = format(max(A$date1)),
  case_t = wd(case$date), case_t1 = wd(case$date1), case_n = case$n_us, case_s = f(case$s_us2y, 1), case_dy = f(case$dy_kr3y, 1),
  b0_3 = f(b2("k3", "beta0"), 3), b0_10 = f(b2("k10", "beta0"), 3), t0_3 = f(b2("k3", "t_beta0"), 1), t0_10 = f(b2("k10", "t_beta0"), 1),
  bp10_3 = f(b2("k3", "bp_per_10bp"), 1), bp10_10 = f(b2("k10", "bp_per_10bp"), 1),
  bup_3 = f(b2("k3", "beta_up"), 3), bdn_3 = f(b2("k3", "beta_down"), 3), bup_10 = f(b2("k10", "beta_up"), 3), bdn_10 = f(b2("k10", "beta_down"), 3),
  pud_3 = f(b2("k3", "p_up_eq_down")), pud_10 = f(b2("k10", "p_up_eq_down")),
  same_3 = f(b2("k3", "same_day_beta")), same_10 = f(b2("k10", "same_day_beta")),
  samet_3 = f(b2("k3", "same_day_t"), 1), samet_10 = f(b2("k10", "same_day_t"), 1), sd_10 = f(b2("k10", "sd_dy_bp"), 1),
  gr10_5 = fp(grr(5, "p")), gr10_10 = fp(grr(10, "p")), gr10_dr2 = f(100 * max(gr$dR2[gr$target == "k10"]), 1),
  gr10_b0 = f(grr(10, "beta0_with_lags"), 3),
  brk3 = format(res$supwald$break_date[res$supwald$target == "k3"], "%Y년 %m월"),
  brk10 = format(res$supwald$break_date[res$supwald$target == "k10"], "%Y년 %m월"),
  p1_rows = p1_rows, jw3 = jwr("k3"), jw10 = jwr("k10"),
  q1_vix = f(qb(1)), q5_vix = f(qb(5)), q_vix_p = fp(q$p_q5_q1[q$target == "k10" & q$hyp == "H3"][1]),
  h2ud_A = fp(res$part1$joint_wald$h2_up_eq_down_p[res$part1$joint_wald$target == "k10" & res$part1$joint_wald$spec == "A"]),
  h2ud_B = fp(res$part1$joint_wald$h2_up_eq_down_p[res$part1$joint_wald$target == "k10" & res$part1$joint_wald$spec == "B"]),
  r11_h2 = f(rq("R11 within-year (s x year)", "k10", "sp:fut3_cum", "est"), 3), r11_h2_p = fp(rq("R11 within-year (s x year)", "k10", "sp:fut3_cum", "p")),
  r11_vix = f(rq("R11 within-year (s x year)", "k10", "s:vix", "est"), 3), r11_vix_p = fp(rq("R11 within-year (s x year)", "k10", "s:vix", "p")),
  r4_vix_e = f(rq("R4 2010-2019", "k10", "s:vix", "est"), 3), r4_vix_e_p = fp(rq("R4 2010-2019", "k10", "s:vix", "p")),
  r4_vix_l = f(rq("R4 2020-2026", "k10", "s:vix", "est"), 3), r4_vix_l_p = fp(rq("R4 2020-2026", "k10", "s:vix", "p")),
  vix_C = f(p1s("k10", "H3", "C", "est"), 3), vix_C_holm = fp(p1s("k10", "H3", "C", "p_holm")),
  baa_vix = f(r13v("baa_spread", "vix_est"), 3), baa_vix_p = fp(r13v("baa_spread", "vix_p")), baa_cor = f(r13v("baa_spread", "cor_vix")),
  krw_est = paste0("+", f(r13v("krw_chg20", "est"), 3)), krw_p = fp(r13v("krw_chg20", "p")), krw_holm = fp(r13v("krw_chg20", "p_holm")),
  g_3 = f(sb("k3", "beta_growth"), 3), m_3 = f(sb("k3", "beta_monetary"), 3), td_3 = f(sb("k3", "t_diff"), 1), share_3 = pct(sb("k3", "share_mon")),
  g_10 = f(sb("k10", "beta_growth"), 3), m_10 = f(sb("k10", "beta_monetary"), 3), td_10 = f(sb("k10", "t_diff"), 1), share_10 = pct(sb("k10", "share_mon")),
  n_rows = n_rows,
  e_g10 = f(e5v("growth-type day (mon = 0)"), 1), e_m10 = f(e5v("monetary-type day (mon = 1)"), 1),
  e_fut = f(e5v("change per +1 s.d. of H2 (spec A)"), 2), e_vix = f(e5v("change per +1 s.d. of H3 (spec A)"), 2),
  n_fc = format(oo("k10", "M2 vs M1")$n, big.mark = ","),
  m1_3 = pct(oo("k3", "M1 vs M0")$r2_oos, 1), m1_10 = pct(oo("k10", "M1 vs M0")$r2_oos, 1),
  m2_3 = pct(oo("k3", "M2 vs M1")$r2_oos, 1), m2_10 = pct(oo("k10", "M2 vs M1")$r2_oos, 1), cw2_10 = fp(oo("k10", "M2 vs M1")$cw_p),
  m4_3 = pct(oo("k3", "M4 vs M1 (N5)")$r2_oos, 1), m4_10 = pct(oo("k10", "M4 vs M1 (N5)")$r2_oos, 1),
  c10p = fp(gc("k10")), c3p = fp(gc("k3")),
  cm10 = f(n3$c_m[n3$target == "k10"]), cg10 = f(n3$c_g[n3$target == "k10"]),
  heat_vix = pct(hv("k10", "vix")), heat_fut3 = pct(hv("k3", "fut3_cum")),
  v_rows = v_rows, var_rows = var_rows, p4_n = format(p4$vol_compare$n[1], big.mark = ","),
  p4c3 = f(p4c("k3")), p4c10 = f(p4c("k10")),
  mh1_3 = f(mh("k3", 1, "beta")), mh10_3 = f(mh("k3", 10, "beta")), mh1_10 = f(mh("k10", 1, "beta")), mh10_10 = f(mh("k10", 10, "beta")),
  mh10_g = f(mh("k10", 10, "beta_growth")), mh10_m = f(mh("k10", 10, "beta_monetary")), mh10_t = f(mh("k10", 10, "t_diff"), 1),
  var_lag = p4$var_model$lag_bic, fevd3 = pct(fus("kr3")), fevd10 = pct(fus("kr10")),
  rf_y0 = format(min(rf$end_date), "%Y"), rf_y1 = format(max(rf$end_date), "%Y"),
  rf3_0 = pct(rf$us_share_kr3[1]), rf10_0 = pct(rf$us_share_kr10[1]),
  rf3_1 = pct(tail(rf$us_share_kr3, 1)), rf10_1 = pct(tail(rf$us_share_kr10, 1)),
  po10 = f(cot("kr10y ~ us10y", "po_stat"), 1), po3 = f(cot("kr3y ~ us2y", "po_stat"), 1), po_cv = f(ci$po_cv5[1], 1),
  jo10 = f(cot("kr10y ~ us10y", "jo_r0"), 1), jo3 = f(cot("kr3y ~ us2y", "jo_r0"), 1), jo_cv = f(ci$jo_r0_cv5[1], 2),
  sp2_3 = fp(sup("P2 split", "k3")), sp2_10 = fp(sup("P2 split", "k10")),
  sud_3 = fp(sup("US up vs down", "k3")), sud_10 = fp(sup("US up vs down", "k10")),
  vr_e3 = pct(psv("k3", 1, "vr"), 1), vr_l3 = pct(psv("k3", 2, "vr"), 1), vr_e10 = pct(psv("k10", 1, "vr"), 1), vr_l10 = pct(psv("k10", 2, "vr"), 1),
  sds_e3 = f(sqrt(psv("k3", 1, "var_s")), 1), sds_l3 = f(sqrt(psv("k3", 2, "var_s")), 1),
  b_e3 = f(psv("k3", 1, "beta")), b_l3 = f(psv("k3", 2, "beta")), b_e10 = f(psv("k10", 1, "beta")), b_l10 = f(psv("k10", 2, "beta")),
  bp_e10 = f(10 * psv("k10", 1, "beta"), 1), bp_l10 = f(10 * psv("k10", 2, "beta"), 1),
  dl10 = f(dcv("k10", "d_logit")), cb10 = f(dcv("k10", "C_beta")), cs10 = f(dcv("k10", "C_s")), ce10 = f(dcv("k10", "C_e")),
  per_rows = per_rows, l_rows = l_rows,
  brk_s3 = format(p5$supwald_s2$break_date[p5$supwald_s2$target == "k3"], "%Y년 %m월"),
  brk_s10 = format(p5$supwald_s2$break_date[p5$supwald_s2$target == "k10"], "%Y년 %m월"),
  h5_3 = fpb(t5x$p_holm[startsWith(t5x$test, "L5") & t5x$target == "k3"]),
  nb_p5_3 = fpb(rb5$p_L5[rb5$target == "k3" & rb5$check == "late period without 2020"]),
  var_rm3 = pct(vr$rate[vr$target == "k3" & vr$level == 0.01 & vr$model == "RiskMetrics"], 1),
  var_rm10 = pct(vr$rate[vr$target == "k10" & vr$level == 0.01 & vr$model == "RiskMetrics"], 1),
  var_g3 = pct(vr$rate[vr$target == "k3" & vr$level == 0.01 & vr$model == "G0"], 1),
  var_g10 = pct(vr$rate[vr$target == "k10" & vr$level == 0.01 & vr$model == "G0"], 1)
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
