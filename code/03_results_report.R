# =============================================================================
# 03_results_report.R
# Writes the Korean results summary (HTML) from output/Team_result.RData.
# Convert to PDF with headless Chrome (see bottom). Run from "Final Project":
#   Rscript code/03_results_report.R
# Not part of the submission file: an internal summary for the team.
# =============================================================================
load("output/Team_result.RData")
OUT_HTML <- "output/report/results_summary.html"
OUT_PDF  <- "분석결과_정리.pdf"
dir.create(dirname(OUT_HTML), recursive = TRUE, showWarnings = FALSE)

f  <- function(x, d = 3) ifelse(is.na(x), "–", formatC(x, format = "f", digits = d))
fp <- function(p) ifelse(is.na(p), "–", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
st <- function(p) ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", ""))))
tg_name <- c(k3 = "3년물 (← 미 2년)", k10 = "10년물 (← 미 10년)")
tg_short <- c(k3 = "3년물", k10 = "10년물")
html_table <- function(df, caption = NULL, note = NULL, align = NULL, cls = "", header = names(df), group = NULL) {
  if (is.null(align)) align <- c("left", rep("right", ncol(df) - 1))
  head <- paste0("<tr>", paste0("<th style='text-align:", align, "'>", header, "</th>", collapse = ""), "</tr>")
  if (!is.null(group))   # group = named vector: label -> number of columns it spans
    head <- paste0("<tr>", paste0("<th class='grp' colspan='", group, "'>", names(group), "</th>", collapse = ""), "</tr>", head)
  body <- apply(df, 1, function(r) paste0("<tr>", paste0("<td style='text-align:", align, "'>", r, "</td>", collapse = ""), "</tr>"))
  paste0("<figure class='tbl ", cls, "'>", if (!is.null(caption)) paste0("<figcaption>", caption, "</figcaption>"),
         "<table>", head, paste(body, collapse = ""), "</table>",
         if (!is.null(note)) paste0("<p class='note'>", note, "</p>"), "</figure>")
}
fig <- function(file, caption, note = NULL, w = "100%")
  paste0("<figure class='fig'><img src='../figures/", file, "' style='width:", w, "'><figcaption>", caption,
         "</figcaption>", if (!is.null(note)) paste0("<p class='note'>", note, "</p>"), "</figure>")
P <- function(...) paste0("<p>", paste0(...), "</p>")

# ---- numbers used in the text -------------------------------------------------------
A  <- res$pairs; t2 <- res$table2; t3 <- res$table3; t5 <- res$table5; es <- res$econ_size
g3 <- function(tg, spec, term, col) t3[t3$target == tg & t3$spec == spec & t3$term == term, col]
b0 <- setNames(t2$beta0, t2$target)
gx <- res$garchx; gc <- function(tg, col) gx[gx$target == tg & gx$label == "c (s^2/100)", col]
esz <- function(tg, h, q) es[es$target == tg & es$hyp == h & abs(es$quantile - q) < 1e-9, "beta"]
oo <- function(tg, cmp, col) t5[t5$target == tg & t5$comparison == cmp, col]
rbx <- res$robustness
rq <- function(chk, tg, term, col) { v <- rbx[rbx$check == chk & rbx$target == tg & rbx$term == term, col]; if (length(v)) v[1] else NA }
r10 <- function(tg, term, col) rq("R10 add s x |s|", tg, term, col)
r11 <- function(tg, term, col) rq("R11 within-year (s x year)", tg, term, col)
n_pairs <- nrow(A); per <- paste(format(min(A$date)), "~", format(max(A$date1)))
t2b <- res$table2b; sb <- function(tg, col) t2b[t2b$target == tg, col]
qb <- function(tg, h, q) res$quintiles$beta[res$quintiles$target == tg & res$quintiles$hyp == h & res$quintiles$quintile == q]
r2v <- function(chk, term, col) { r <- res$robustness; r[r$check == chk & r$term == term, col][1] }
n_cand <- length(unique(res$table_lasso$interaction)); n_win <- length(unique(res$heatmap$end[res$heatmap$target == "k3"]))
n_sel <- sum(res$table_lasso$lasso != 0 | res$table_lasso$enet != 0)

# ---- sections ----------------------------------------------------------------------
key <- paste0("<div class='key'><h3>핵심 결과</h3><ol>",
  "<li><b>평균 전이는 크고 안정적이다.</b> 미국 금리가 1bp 오르면 다음 날 국고채 3년물은 ", f(b0["k3"], 2), "bp, 10년물은 ",
  f(b0["k10"], 2), "bp 오른다(HAC t = ", f(t2$t_beta0[1], 1), ", ", f(t2$t_beta0[2], 1), "). 상승·하락 충격의 크기 차이는 없다.</li>",
  "<li><b>충격의 성격이 전이를 가른다.</b> 미국 금리와 주가가 반대로 움직인 날(통화정책형 충격, Hoek, Kamin & Yoldas 2022)은 같이 움직인 날(성장형)보다 더 많이 넘어온다. ",
  "10년물 전이 계수는 성장형 ", f(sb("k10", "beta_growth"), 2), ", 통화정책형 ", f(sb("k10", "beta_monetary"), 2), "(차이 t = ", f(sb("k10", "t_diff"), 1), "), 3년물은 ",
  f(sb("k3", "beta_growth"), 2), ", ", f(sb("k3", "beta_monetary"), 2), "(t = ", f(sb("k3", "t_diff"), 1), ")이다.</li>",
  "<li><b>10년물에서 조건에 따른 차이가 있지만, 방향은 예측과 반대다.</b> 외국인 3년 선물 순매수가 쌓인 상태에서 미국 금리가 오를수록(H2, ",
  f(g3("k10", "trend", "sp:fut3_cum", "est")), ", Holm p = ", fp(g3("k10", "trend", "sp:fut3_cum", "p_holm")), ") 전이가 <b>약해진다</b>. VIX가 높을 때도 약해지지만(H3, ",
  f(g3("k10", "trend", "s:vix", "est")), ", p = ", fp(g3("k10", "trend", "s:vix", "p")), "), 충격 성격을 통제하면 Holm 보정 후에는 유의하지 않다(Holm p = ",
  fp(g3("k10", "trend", "s:vix", "p_holm")), ").</li>",
  "<li><b>두 결과는 성격이 다르다.</b> 연도별 전이 계수를 따로 두고 같은 해 안의 날별 차이만 보면(R11), H2(s⁺)는 유지되지만(",
  f(r11("k10", "sp:fut3_cum", "est")), ", p ", fp(r11("k10", "sp:fut3_cum", "p")), ") VIX 효과는 약해진다(", f(r11("k10", "s:vix", "est")), ", p ",
  fp(r11("k10", "s:vix", "p")), "). <b>H2는 '어떤 날'의 효과, H3는 '어떤 시기'의 효과</b>다. 충격 크기의 비선형(R10)을 통제하면 VIX 효과는 오히려 강해진다(", f(r10("k10", "s:vix", "est")), ", t ", f(r10("k10", "s:vix", "t"), 1), ").</li>",
  "<li><b>3년물은 세 조건 모두에서 전이가 거의 달라지지 않는다.</b> Holm 보정 후 유의한 항이 없다.</li>",
  "<li><b>조건부 모형의 예측 개선은 10년물에서만, 작게 나타난다.</b> Clark–West p = ", fp(oo("k10", "M2 vs M1", "cw_p")),
  "이지만 MSFE는 거의 같다. LASSO 모형(M3)은 기준 모형보다 못하다. 전체 표본 LASSO는 상호작용을 하나도 고르지 않는다.</li>",
  "<li><b>미국 충격은 다음 날 국고채 변동성도 키운다.</b> GARCH-X의 c는 10년물 ", f(gc("k10", "est"), 2), "(우도비 p = ", fp(gc("k10", "lr_p")),
  "), 3년물 ", f(gc("k3", "est"), 2), "(p = ", fp(gc("k3", "lr_p")), ")이다.</li></ol></div>")

s1 <- paste0("<h2>1. 연구 설계 요약</h2>",
  P("<b>연구 질문</b>: 간밤 미국 금리 충격이 다음 날 국고채로 더 세게 넘어오는 날은 어떤 조건일 때인가?"),
  P("<b>모형</b>: Δy(t+1) = α + (β<sub>0</sub> + Σ<sub>j</sub> β<sub>j</sub> z<sub>j</sub>(t) + β<sub>m</sub> mon(t))·s(t) + Σ<sub>j</sub> γ<sub>j</sub> z<sub>j</sub>(t) + γ<sub>m</sub> mon(t) + ρΔy(t) + δ′D(t) + [trend, s×trend] + ε. ",
    "Δy는 한국 t일 종가 → t+1일 종가 변화(bp), s(t)는 한국 t일 마감 뒤 t+1일 개장 전에 끝난 미국 거래일 변화의 합(bp), z는 t일 마감 때 알려진 조건(표준화)이다. ",
    "mon(t)는 충격의 성격이다. s(t)와 같은 미국 거래일들의 NASDAQ 수익률이 s(t)와 부호가 반대면 1(통화정책형), 같으면 0(성장형)이다. 가설이 아니라 통제로 넣었다. ",
    "표준오차는 Newey–West(시차 9)이다."),
  html_table(data.frame(가설 = c("H1 기대 경로", "H2 수급 경로", "H3 위험 경로"),
                        `대표 조건` = c("|국고채 1년 − 기준금리|", "외국인 3년 국채선물 누적 순매수 (250일 z)", "log VIX"),
                        `원래 예측` = c("전이 확대 (+)", "미국 금리 상승일 전이 확대 (+)", "전이 확대 (+)"), check.names = FALSE),
             "표 A. 가설과 대표 조건 변수", align = c("left", "left", "left")),
  P("<b>변수 확정</b>: 대표 변수 3개는 결과를 보기 전에 정했고 바꾸지 않았다. 선행연구 검토(「조건별전이_변수확정_선행연구.md」)에 따라 LASSO 후보를 13개에서 ", n_cand,
    "개(|kr1y_gap|, policy_gap, fut3_cum, fut10_cum, fgn_share, log VIX, baa_spread, krw_chg20)로 줄였고, 충격 성격(mon)을 통제로 추가했다. mon의 효과는 넣기 전에 한 번 확인했다."),
  P("<b>미리 정한 설정</b>: 모든 검정은 양측이다. 전이 계수가 기간에 따라 커지므로 가설 모형과 LASSO에는 trend와 s×trend를 통제로 넣었다(추세 없는 결과도 함께 보고). ",
    "단위근 검정에서 I(1)로 나온 조건 ", paste(res$transformed, collapse = ", "), "은 250일 이동 z점수로 바꿨다. 다중검정은 가설 3개 × 만기 2개 = 6개에 Holm 보정을 한다."))

d1 <- res$desc; d1 <- d1[d1$variable %in% c("dy_kr3y", "dy_kr10y", "s_us2y", "s_us10y", "abs_kr1y_gap", "fut3_cum", "vix"), ]
d1$variable <- c(dy_kr3y = "Δ국고채 3년 (bp)", dy_kr10y = "Δ국고채 10년 (bp)", s_us2y = "충격 s: 미 2년 (bp)", s_us10y = "충격 s: 미 10년 (bp)",
                 abs_kr1y_gap = "|kr1y_gap| (%p)", fut3_cum = "fut3_cum (z)", vix = "log VIX")[d1$variable]
s2 <- paste0("<h2>2. 데이터</h2>",
  P("한국 거래일 쌍 ", format(n_pairs, big.mark = ","), "개(", per, "). 출처는 ECOS, FRED, 뉴욕 연은, KRX, 금융감독원이고 전처리 규칙은 「조건별전이_데이터전처리.md」에 있다."),
  fig("figure4_levels.png", "그림 4. 한·미 국채 금리 수준"),
  html_table(data.frame(변수 = d1$variable, n = format(d1$n, big.mark = ","), 평균 = f(d1$mean, 2), 표준편차 = f(d1$sd, 2),
                        최소 = f(d1$min, 2), 최대 = f(d1$max, 2), 첨도 = f(d1$kurt, 1), check.names = FALSE),
             "표 1. 주요 변수 기술통계", "금리 변화의 첨도가 높아(두꺼운 꼬리) 견고성 R5에서 1% 윈저화를 확인한다."))

s3 <- paste0("<h2>3. 평균 전이 (기준 모형)</h2>",
  html_table(data.frame(만기 = tg_name[t2$target], `β₀` = paste0(f(t2$beta0), st(2 * pnorm(-abs(t2$t_beta0)))), `HAC t` = f(t2$t_beta0, 1),
                        `ρ (전날 Δy)` = f(t2$rho), R2 = f(t2$r2), n = format(t2$n, big.mark = ","),
                        `β⁺ (미 상승)` = f(t2$beta_up), `β⁻ (미 하락)` = f(t2$beta_down), `β⁺=β⁻ p` = fp(t2$p_up_eq_down), check.names = FALSE),
             "표 2. 기준 모형: Δy(t+1) = α + β₀ s(t) + ρΔy(t) + δ′D(t)", "*** p<0.01. D = 연휴 뒤 첫날, 미국 휴장(s = 0), FOMC, 금통위 더미."),
  P("전이 계수는 두 만기 모두 약 0.3이고, 상승·하락 충격에 대칭이다. 다만 아래 그림처럼 시기에 따라 크게 움직인다. 3년물은 2013년 테이퍼 텐트럼 무렵 0.6까지 올랐고, ",
    "10년물은 2010–11년 0에 가까웠다가 2023년 이후 0.4–0.5로 높아졌다. 구조변화 sup-Wald 검정은 3년물 ", format(res$supwald$break_date[1]),
    "(p ", fp(res$supwald$p[1]), "), 10년물 ", format(res$supwald$break_date[2]), "(p ", fp(res$supwald$p[2]),
    ")을 변화 시점으로 잡는다. 10년물 시점은 탐색 구간의 시작점이라 2010–11년의 낮은 전이를 반영한 것으로 본다."),
  fig("figure5_rolling_beta.png", "그림 5. 250일 롤링 전이 계수 (점선 = 전체 표본)"),
  "<h3>3-1. 충격의 성격</h3>",
  html_table(data.frame(만기 = tg_name[t2b$target], `통화정책형 비율` = paste0(f(100 * t2b$share_mon, 1), "%"), `성장형 β` = f(t2b$beta_growth),
                        `통화정책형 β` = paste0(f(t2b$beta_monetary), " (", f(t2b$se_monetary), ")"), 차이 = paste0(f(t2b$diff), st(t2b$p_diff)),
                        `차이 t` = f(t2b$t_diff, 1), `mon–VIX 상관` = f(t2b$corr_mon_vix, 3), check.names = FALSE),
             "표 2-1. 성장형(mon = 0) vs 통화정책형(mon = 1) 충격의 전이",
             "기준 모형에 mon과 s × mon을 추가. 통화정책형 = 미국 금리와 NASDAQ이 같은 기간에 반대로 움직임. 괄호는 HAC 표준오차(델타법). *** p<0.01, ** p<0.05.", cls = "small"),
  P("미국 금리 상승이 주가 하락과 함께 오면(통화정책형) 한국으로 더 많이 넘어온다. 10년물은 차이가 ", f(sb("k10", "diff"), 2), "로 성장형 대비 약 ",
    f(100 * sb("k10", "diff") / sb("k10", "beta_growth"), 0), "% 크다. Hoek, Kamin & Yoldas(2022)가 신흥국에서 보인 결과와 같은 방향이다. ",
    "mon은 VIX와 거의 상관이 없어서, 아래 조건 효과와 따로 성립한다. 가설 모형에도 mon과 s × mon을 통제로 넣었다."))

# table 3
rows <- data.frame(term = c("s:abs_kr1y_gap", "s:fut3_cum", "sp:fut3_cum", "sm:fut3_cum", "s:vix", "s:mon"),
                   name = c("H1  s × |kr1y_gap|", "H2  s × fut3 (대칭)", "H2  s⁺ × fut3 (미 상승일)", "H2  s⁻ × fut3 (미 하락일)", "H3  s × log VIX", "통제  s × mon (충격 성격)"),
                   pred = c("+", "", "+", "", "+", ""))
mk3 <- function(spec) do.call(cbind, lapply(c("k3", "k10"), function(tg) {
  x <- sapply(rows$term, function(tm) { r <- t3[t3$target == tg & t3$spec == spec & t3$term == tm, ]
    c(paste0(f(r$est), st(r$p)), paste0("(", f(r$se), ")"), fp(r$p), ifelse(is.na(r$p_holm), "", fp(r$p_holm))) })
  out <- data.frame(t(x)); names(out) <- paste(tg_name[tg], c("계수", "SE", "p", "Holm p")); out }))
t3a <- cbind(data.frame(`상호작용` = rows$name, `예측` = rows$pred, check.names = FALSE), mk3("trend"))
t3b <- cbind(data.frame(`상호작용` = rows$name, `예측` = rows$pred, check.names = FALSE), mk3("notrend"))
H3 <- c("상호작용", "예측", rep(c("계수", "SE", "p", "Holm p"), 2))
G3 <- c(" " = 2, "3년물 (← 미 2년)" = 4, "10년물 (← 미 10년)" = 4)
beta_main <- sapply(c("k3", "k10"), function(tg) g3(tg, "trend", "s", "est"))
esd <- data.frame(만기 = tg_short[es$target], 가설 = es$hyp, 조건 = es$condition, 분위 = paste0(es$quantile * 100, "%"),
                  `전이 계수` = f(es$beta), `95% 구간` = paste0("[", f(es$lo), ", ", f(es$hi), "]"), check.names = FALSE)
qp <- unique(res$quintiles[, c("target", "hyp", "condition", "p_q5_q1")])
s4 <- paste0("<h2>4. 가설 검정</h2>",
  P("세 대표 조건을 한 식에 함께 넣어 경쟁시켰다. 계수는 조건이 1 표준편차 높을 때 전이 계수가 얼마나 바뀌는지다(기준 전이 계수: 3년물 ",
    f(beta_main["k3"]), ", 10년물 ", f(beta_main["k10"]), "). Holm 보정은 H1, H2(s⁺), H3 × 2개 만기의 6개 검정에 적용했다."),
  html_table(t3a, "표 3. 가설 모형: 상호작용 계수 (본 설정: trend, s×trend 통제)",
             "괄호는 Newey–West 표준오차. 양측 p값. *** p<0.01, ** p<0.05, * p<0.1. H2의 공식 검정은 s⁺ × fut3이다(연구개요 4-2). mon은 0/1 더미라 s × mon은 통화정책형 충격일 때 전이 계수의 차이다.", cls = "small",
             header = H3, group = G3),
  html_table(t3b, "표 3-1. 같은 모형, 추세 통제 없음 (비교용)", cls = "small", header = H3, group = G3),
  P("<b>읽는 법</b>"),
  "<ul><li><b>10년물 H2</b>: 외국인 선물 순매수가 많이 쌓인 상태(z 높음)에서 미국 금리가 오르면 전이가 ", f(g3("k10", "trend", "sp:fut3_cum", "est")),
  " 작아진다(Holm p ", fp(g3("k10", "trend", "sp:fut3_cum", "p_holm")), "). 청산 매도로 전이가 커진다는 예측과 반대다. 미국 금리 하락일(s⁻)에는 차이가 없다. Holm 보정 후 유의한 유일한 결과다.</li>",
  "<li><b>10년물 H3</b>: VIX가 1 표준편차 높으면 전이 계수가 ", f(g3("k10", "trend", "s:vix", "est")), " 줄어든다(p ", fp(g3("k10", "trend", "s:vix", "p")),
  ", Holm p ", fp(g3("k10", "trend", "s:vix", "p_holm")), "). 예측(+)과 반대 방향이다. 충격 성격을 통제하기 전에는 Holm 보정 후에도 5%에서 유의했으나(−0.034, Holm p 0.044), 통제 후 약해졌다. ",
  "VIX 하위 20%에서 전이 계수는 ", f(esz("k10", "H3", 0.2)), ", 상위 20%에서 ", f(esz("k10", "H3", 0.8)), "이다(충격 성격은 표본 평균).</li>",
  "<li><b>H1</b>: 10년물에서 음(−)의 방향이다(p ", fp(g3("k10", "trend", "s:abs_kr1y_gap", "p")), ", Holm p ", fp(g3("k10", "trend", "s:abs_kr1y_gap", "p_holm")), "). 3년물은 0에 가깝다.</li>",
  "<li><b>3년물</b>: s⁺ × fut3이 보정 전 p ", fp(g3("k3", "trend", "sp:fut3_cum", "p")), "이고, Holm 보정 후에는 유의하지 않다.</li>",
  "<li><b>충격 성격(통제)</b>: 가설 모형 안에서도 s × mon은 10년물 ", f(g3("k10", "trend", "s:mon", "est")), "(p ", fp(g3("k10", "trend", "s:mon", "p")), "), 3년물 ",
  f(g3("k3", "trend", "s:mon", "est")), "(p ", fp(g3("k3", "trend", "s:mon", "p")), ")이다.</li>",
  "<li>추세 통제를 빼면(표 3-1) 10년물 H2(s⁺)는 Holm p가 ", fp(g3("k10", "notrend", "sp:fut3_cum", "p_holm")), ", H3는 ", fp(g3("k10", "notrend", "s:vix", "p_holm")),
  "로 둘 다 10% 근처다. 방향은 같다.</li></ul>",
  fig("figure6_coefficients.png", "그림 6. 상호작용 계수와 95% HAC 구간 (채운 점 = 본 설정, 빈 점 = 추세 없음)"),
  html_table(esd[esd$만기 == tg_short["k10"], ], "표 3-2. 경제적 크기: 조건 하위·상위 20%에서의 10년물 전이 계수 (델타법 95% 구간)", cls = "small"),
  fig("figure1_quintiles.png", "그림 1. 조건 5분위별 전이 계수 (선형 가정 없이)"),
  html_table(data.frame(만기 = tg_short[qp$target], 가설 = qp$hyp, 조건 = qp$condition, `b5 − b1 = 0 p` = fp(qp$p_q5_q1), check.names = FALSE),
             "표 3-3. 5분위 검정: 최상위 분위와 최하위 분위 전이 계수가 같은가", cls = "small"),
  P("5분위 결과는 선형 상호작용보다 뚜렷하다. 10년물은 VIX가 높을수록 전이 계수가 1분위 ", f(qb("k10", "H3", 1), 2), "에서 5분위 ", f(qb("k10", "H3", 5), 2), "(으)로 줄고(p ",
    fp(qp$p_q5_q1[qp$target == "k10" & qp$hyp == "H3"]), "), |kr1y_gap|에서도 ", f(qb("k10", "H1", 1), 2), "에서 ", f(qb("k10", "H1", 5), 2), "(으)로 줄어든다(p ",
    fp(qp$p_q5_q1[qp$target == "k10" & qp$hyp == "H1"]), "). 3년물은 분위별로 들쭉날쭉하고 체계적인 차이가 없다."))

tl <- res$table_lasso; tl <- tl[tl$lasso != 0 | tl$enet != 0, ]
hf <- res$heat_freq; hf <- hf[order(-hf$selected), ]; hf <- hf[hf$selected > 0.05, ]
s5 <- paste0("<h2>5. 데이터 기반 확인 (LASSO·Elastic Net)</h2>",
  P("2012-05-31 이후 표본에서 조건 ", n_cand, "개의 상호작용에만 벌점을 주고 BIC로 λ를 골랐다. s, Δy(t), 더미, mon, s × mon, trend, s × trend와 조건 주효과는 벌점 없이 넣었다(설명변수 ",
    res$lasso$k3$p, "개). 사후 추론은 하지 않고 어떤 경로가 선택되는지만 본다."),
  if (n_sel > 0) html_table(data.frame(만기 = tg_short[tl$target], `선택된 상호작용` = tl$interaction, LASSO = f(tl$lasso, 4), `Elastic Net (α = 0.5)` = f(tl$enet, 4), check.names = FALSE),
             "표 4. 전체 표본에서 선택된 상호작용 (나머지는 모두 0)") else "",
  P(if (n_sel == 0) "<b>전체 표본에서는 두 만기 모두, LASSO와 Elastic Net 모두 상호작용을 하나도 고르지 않았다</b>(표 4 생략). BIC 기준으로는 추세와 충격 성격을 통제한 뒤 조건 상호작용이 추가하는 설명력이 벌점보다 작다. 가설 모형에서 유의한 효과(10년물 H2 s⁺)도 크기가 작아서 데이터만으로 고를 정도는 아니라는 뜻이다. " else "",
    "변수 확정 전(후보 13개, trend·mon 없음)에는 baa_spread가 두 만기 모두 선택됐다. 확인해 보면 10년물의 baa_spread는 trend와 mon 중 하나만 넣으면 남고 둘 다 넣어야 사라진다. ",
    "3년물은 후보를 8개로 줄이는 것만으로 사라진다. 즉 이 선택은 설정에 민감하다."),
  fig("figure2_heatmap.png", paste0("그림 2. 1,000일 롤링 창별 LASSO 선택 (20일 간격, ", n_win, "개 창)")),
  html_table(data.frame(만기 = tg_short[hf$target], 상호작용 = hf$interaction, `선택 빈도` = paste0(f(100 * hf$selected, 1), "%"), check.names = FALSE),
             "표 4-1. 롤링 선택 빈도 5% 초과 항목", "50% 이상이면 \"안정적으로 선택\"으로 보기로 했으나, 그런 항목은 없다."),
  P("롤링 창에서 안정적으로 선택되는 상호작용은 없다. 다만 선택될 때는 대표 변수가 선택된다. 10년물은 VIX(", f(100 * hf$selected[hf$target == "k10" & hf$interaction == "vix"], 0),
    "%), 3년물은 fut3_cum(", f(100 * hf$selected[hf$target == "k3" & hf$interaction == "fut3_cum"], 0), "%)이 가장 자주 선택된다. 그림 2에서 어느 시기에 선택됐는지 볼 수 있다."))

t5s <- t5; t5s$`만기` <- tg_name[t5s$target]
s6 <- paste0("<h2>6. 표본외 평가</h2>",
  P("1,000거래일 롤링 창으로 한국 t+1일 개장 직전(s(t)는 관측된 상태)의 Δy를 예측했다. M1·M2는 매일, M3(LASSO)는 20일마다 다시 추정했다. 첫 예측은 M1·M2가 ",
    format(res$oos$k3$first_forecast), ", M3가 ", format(min(res$oos$k3$forecasts$date[!is.na(res$oos$k3$forecasts$M3)])), "이다. 충격 성격(mon, s × mon)은 M1·M2·M3 모두에 넣었다."),
  html_table(data.frame(만기 = tg_short[t5s$target], 비교 = sub(" (non-nested)", "", t5s$comparison, fixed = TRUE), n = format(t5s$n, big.mark = ","), `MSFE 1` = f(t5s$msfe1, 2), `MSFE 2` = f(t5s$msfe2, 2),
                        `표본외 R²` = f(t5s$r2_oos, 3), `방향 적중 2` = paste0(f(100 * t5s$hit2, 1), "%"), `CW p` = fp(t5s$cw_p),
                        `GW 무조건 p` = fp(t5s$p_uncond), `GW 조건 p` = fp(t5s$p_cond), `DM p` = fp(t5s$dm_p), check.names = FALSE),
             "표 5. 표본외 예측 비교 (비교 \"A vs B\"에서 MSFE 1 = A, MSFE 2 = B)",
             "M0 = 0(랜덤워크), M1 = 기준 모형, M2 = 가설 모형, M3 = LASSO. 표본외 R² = 1 − MSFE 2 / MSFE 1(양수면 B가 나음). CW = Clark–West(단측, 중첩 비교), GW = Giacomini–White(제곱 손실), DM = 비중첩 M2 vs M3.", cls = "small"),
  "<ul><li>미국 충격을 쓰는 기준 모형(M1)은 랜덤워크보다 확실히 낫다(표본외 R² 3년물 ", f(oo("k3", "M1 vs M0", "r2_oos"), 2), ", 10년물 ", f(oo("k10", "M1 vs M0", "r2_oos"), 2), ").</li>",
  "<li>조건부 모형(M2)은 10년물에서만 Clark–West 검정으로 유의하게 낫다(p ", fp(oo("k10", "M2 vs M1", "cw_p")), "). 다만 MSFE 차이는 거의 없어서, 경제적으로 의미 있는 예측 개선이라고 하기는 어렵다.</li>",
  "<li>3년물 M2의 누적 손실은 2022년 6월 며칠의 큰 예측 오차가 대부분을 만들었다(그림 3).</li>",
  "<li>LASSO 모형(M3)은 두 만기 모두 M1보다 못하다. 조건을 데이터로 고르는 방식은 표본외에서 과적합된다.</li></ul>",
  fig("figure3_cumulative_loss.png", "그림 3. M2의 M1 대비 누적 제곱오차 이득 (올라가면 M2가 나음)"))

rb <- res$robustness; r2 <- rb[grepl("^R2", rb$check), ]
cell <- function(est, t) ifelse(is.na(est), "–", paste0(f(est), st(2 * pnorm(-abs(t))), " (", f(t, 1), ")"))
terms1011 <- c("s:abs_kr1y_gap" = "H1  s × |kr1y_gap|", "sp:fut3_cum" = "H2  s⁺ × fut3 (미 상승일)", "s:vix" = "H3  s × log VIX", "s:abs_s" = "s × |s| (비선형)")
r1011 <- do.call(rbind, lapply(names(terms1011), function(tm) {
  vals <- unlist(lapply(c("k3", "k10"), function(tg) {
    m <- t3[t3$target == tg & t3$spec == "trend" & t3$term == tm, ]
    c(if (nrow(m)) cell(m$est, m$t) else "–", cell(r10(tg, tm, "est"), r10(tg, tm, "t")), cell(r11(tg, tm, "est"), r11(tg, tm, "t"))) }))
  as.data.frame(t(c(terms1011[[tm]], vals))) }))
names(r1011) <- c("항", "k3_main", "k3_r10", "k3_r11", "k10_main", "k10_r10", "k10_r11")
s7 <- paste0("<h2>7. 견고성</h2>",
  P("세 대표 상호작용이 설정을 바꿔도 유지되는지 확인했다(그림 7). <b>10년물 H3(VIX)</b>는 거의 모든 설정에서 음(−)이고, FOMC 전후 제외(R3), 윈저화(R5), 5일 평균(R6), 2012년 이후 표본(R9), 충격 크기 비선형 통제(R10)에서도 유의하다. ",
    "<b>시기 분할(R4)</b>에서 경로가 바뀐다. 2010–2019년에는 H1(10년물)과 H2(3년물)이 강하게 음(−)이고, 2020–2026년에는 10년물 H3가 강하게 음(−)이다."),
  fig("figure7_robustness.png", "그림 7. 상호작용 계수의 견고성 (95% HAC 구간)", "H2 칸은 대칭 항(s × fut3)이다. H2의 공식 검정인 미국 금리 상승일 항(s⁺ × fut3)은 표 6-1에 있다."),
  "<h3>7-1. '어떤 날'인가 '어떤 시기'인가 (R10, R11)</h3>",
  P("본 설정은 선형 추세(s×trend)만 통제한다. 그런데 그림 5처럼 전이 계수는 해마다 출렁인다. 그래서 조건 효과가 하루 단위 차이에서 나오는지, 연도 단위의 국면 차이에서 나오는지 두 가지로 확인했다. ",
    "<b>R10</b>은 큰 충격이 더 많이(또는 덜) 넘어오는 비선형성(s × |s|)을 통제한다. <b>R11</b>은 연도 더미와 s × 연도를 넣어 해마다 전이 계수가 달라도 되게 한다. 이러면 조건 효과는 같은 해 안의 날별 차이로만 식별된다."),
  html_table(r1011, "표 6-1. 핵심 상호작용: 본 설정 vs 비선형 통제(R10) vs 연도 내 식별(R11)",
             "괄호는 HAC t. *** p<0.01, ** p<0.05, * p<0.1. s × |s|는 R10에서 추가한 비선형 항이다.", cls = "small",
             header = c("항", rep(c("본 설정", "R10", "R11"), 2)), group = c(" " = 1, "3년물" = 3, "10년물" = 3)),
  "<ul><li><b>R10</b>: 10년물에서 큰 충격일수록 더 많이 넘어온다(s × |s| = ", f(r10("k10", "s:abs_s", "est"), 4), ", t ", f(r10("k10", "s:abs_s", "t"), 1),
  "). 이를 통제하면 VIX 효과는 ", f(r10("k10", "s:vix", "est")), "(t ", f(r10("k10", "s:vix", "t"), 1), ")로 <b>더 강해진다</b>. VIX 효과는 충격 크기의 비선형성이 만든 것이 아니다. 반면 10년물 H2(s⁺)는 ", f(r10("k10", "sp:fut3_cum", "est")), "(t ", f(r10("k10", "sp:fut3_cum", "t"), 1),
  ")로 약해진다. 미국 금리가 크게 오른 날의 비선형성과 일부 겹친다.</li>",
  "<li><b>R11</b>: 같은 해 안의 날별 차이만 보면 10년물 VIX 효과는 ", f(r11("k10", "s:vix", "est")), "(t ", f(r11("k10", "s:vix", "t"), 1), ")로 약해지고 H1은 0이 된다. ",
  "VIX 효과의 상당 부분은 'VIX가 높은 해에 전이가 낮았다'는 국면 차이에서 나온다.</li>",
  "<li><b>R11</b>: 반면 10년물 H2(s⁺ × fut3)는 연도 안에서도 ", f(r11("k10", "sp:fut3_cum", "est")), "(t ", f(r11("k10", "sp:fut3_cum", "t"), 1),
  ")로 유지된다. 외국인 포지션 효과는 날 단위로 작동한다. 다만 R10에서 약해지므로 '큰 상승 충격'과 구분이 완전하지는 않다.</li></ul>",
  html_table(data.frame(`분해 기준` = r2$check, 항 = r2$term, 계수 = paste0(f(r2$est), st(r2$p)), t = f(r2$t, 2), p = fp(r2$p), check.names = FALSE),
             "표 6. R2: 10년물 충격을 기대 부분(s_exp)과 기간프리미엄(s_tp)으로 분해", "KW = Kim–Wright, ACM = Adrian–Crump–Moench. 기간프리미엄은 사후 추정치라 해석에 한계가 있다.", cls = "small"),
  P("ACM 분해에서는 기대 부분과 기간프리미엄 부분이 비슷한 크기(", f(r2v("R2 split by tp_acm", "s_exp", "est"), 2), ", ", f(r2v("R2 split by tp_acm", "s_tp", "est"), 2),
    ")로 넘어오고, VIX에 따라 약해지는 것은 <b>기간프리미엄 부분</b>이다(s_tp × VIX, p ",
    fp(r2$p[r2$check == "R2 split by tp_acm" & r2$term == "s_tp:vix"]), "). 위험회피 때 미 국채 기간프리미엄의 움직임(안전자산 수요)은 한국으로 덜 넘어온다는 해석과 맞는다. ",
    "KW 분해는 기간프리미엄이 금리 변화 대부분을 흡수해 결과가 다르다."))

gxs <- gx[gx$label %in% c("s", "alpha1", "beta1", "c (s^2/100)", "shape"), ]
s8 <- paste0("<h2>8. 변동성 전이 (GARCH-X)</h2>",
  P("평균식은 기준 모형, 분산식은 σ²(t+1) = ω + aε(t)² + bσ²(t) + c·s(t)²/100, Student-t 분포. c ≥ 0 제약 아래 추정하고 c = 0 대비 우도비 검정(50:50 카이제곱 혼합)을 했다."),
  html_table(data.frame(만기 = tg_short[gxs$target], 모수 = gxs$label, 추정치 = f(gxs$est, 3), `강건 SE` = f(gxs$se, 3), t = f(gxs$t, 2),
                        `우도비(c)` = ifelse(gxs$label == "c (s^2/100)", f(gxs$lr_c, 2), ""), `p(c)` = ifelse(gxs$label == "c (s^2/100)", fp(gxs$lr_p), ""), check.names = FALSE),
             "표 7. GARCH-X 추정 결과", cls = "small"),
  P("미국 충격의 제곱은 다음 날 국고채 변동성을 키운다. 10년물은 강하게(p ", fp(gc("k10", "lr_p")), "), 3년물은 약하게(p ", fp(gc("k3", "lr_p")), ") 나타난다. 평균 전이(s 계수)는 GARCH 오차 아래에서도 ", f(gx$est[gx$target == "k3" & gx$label == "s"], 2), ", ", f(gx$est[gx$target == "k10" & gx$label == "s"], 2), "로 유지된다."))

dg <- res$diagnostics
s9 <- paste0("<h2>9. 진단</h2>",
  html_table(data.frame(만기 = tg_short[dg$target], `Ljung–Box Q(10)` = f(dg$ljung_box_q10, 1), `LB p` = fp(dg$lb_p), `ARCH LM(5)` = f(dg$arch_lm5, 0), `ARCH p` = fp(dg$arch_p),
                        `VIF H1/H2/H3` = paste(f(dg$vif_H1, 2), f(dg$vif_H2, 2), f(dg$vif_H3, 2), sep = " / "),
                        `|s| 상위 1% 제외: H1/H2/H3` = paste(f(dg$drop_top1pct_H1), f(dg$drop_top1pct_H2), f(dg$drop_top1pct_H3), sep = " / "), check.names = FALSE),
             "표 8. 가설 모형 진단", cls = "small"),
  "<ul><li>잔차 자기상관은 없다(Ljung–Box). 그래서 Δy 시차를 늘릴 필요가 없다.</li>",
  "<li>ARCH 효과가 매우 강하다. HAC 표준오차가 추론을 보호하고, 8절 GARCH-X의 근거가 된다.</li>",
  "<li>대표 조건 간 VIF는 1.1 수준으로 공선성 문제가 없다.</li>",
  "<li>|s| 상위 1% 거래일을 빼면 10년물 VIX 계수(", f(dg$drop_top1pct_H3[dg$target == "k10"]), ")는 유지되지만, 10년물 H2(대칭)는 크게 줄어 일부 큰 충격일이 이끈다.</li></ul>")

s10 <- paste0("<h2>10. 해석과 남은 과제</h2>",
  "<ul><li><b>무엇이 넘어오느냐가 중요하다.</b> 통화정책형 충격(금리↑ 주가↓)은 성장형 충격보다 더 많이 넘어온다(3-1절). 한국 금리는 미국의 정책 기대 변화에 더 민감하게 반응한다. Hoek, Kamin & Yoldas(2022)의 신흥국 결과를 한국 일별 자료로 확인한 것이다.</li>",
  "<li><b>위험이 큰 '국면'에 전이가 약하다.</b> 원래 예측(위험이 전이를 키움)과 반대다. 다만 충격 성격을 통제하면 Holm 보정 후 유의하지 않고, 주로 연도 단위 국면 차이에서 나온다(R11). 그래서 '위험한 날'보다 '위험한 시기'로, 보조 결과로 쓰는 것이 정확하다. 위험회피 국면에서 미 국채 금리 움직임은 안전자산 수요와 기간프리미엄 변화가 크고(R2 ACM), 이런 움직임은 한국 금리로 덜 넘어온다.</li>",
  "<li><b>외국인 포지션은 날 단위로 전이를 줄인다.</b> 외국인 선물 순매수가 쌓인 날 미국 금리가 오르면 10년물 전이는 오히려 작다. 연도 안의 날별 차이에서도 유지되는 유일한 결과다(R11). 포지션이 청산 압력보다 완충 역할을 했을 가능성, 또는 순매수 누적이 외국인 수요가 강한 국면을 나타낼 가능성을 논의한다. 강규호·서영경(2022)의 '선물 매도가 금리를 올린다'는 결과와는 질문이 다르다는 점을 함께 적는다. ",
  "다만 누적 순매수의 250일 z점수는 미결제약정이 아닌 포지션 대용치라는 한계를 명시한다.</li>",
  "<li><b>연구 질문에 대한 답</b>: 전이 강도는 날 단위 조건보다 국면 단위로 크게 변한다(그림 5, R11). 날 단위로 의미 있는 조건은 외국인 포지션 하나다. 조건부 모형의 표본외 개선이 작은 것(6절)과 같은 그림이다.</li>",
  "<li><b>경로는 시기마다 바뀐다.</b> 2010년대에는 정책 기대(H1), 2020년 이후에는 위험(H3)이 전이 강도를 가른다(R4).</li>",
  "<li><b>남은 과제</b>: (1) 반대 부호 결과의 경제적 해석 보강(기간프리미엄 분해 결과 중심), (2) 리포트 표·그림 번호 정리, (3) 팀 번호가 정해지면 제출 파일 이름 변경, (4) BOK WP 2026-15 원문 확인 후 선행연구 비교 문장 확정.</li></ul>",
  P("<b>해석 시 유의할 점</b>: 상호작용은 인과 경로가 아니라 조건부 상관이다. 원래 예측은 한쪽 방향이었으나 이론적 이유로 양측 검정으로 바꿨고, 이 사실을 본문에 밝힌다. 대표 변수는 결과를 보기 전에 정했고 바꾸지 않았다. ",
    "충격 성격(mon)은 선행연구에 따라 추가했지만 추가 전에 효과를 확인했으므로, 이 사실도 본문에 밝힌다."))

ur <- res$unit_root
sA <- paste0("<h2>부록 A. 단위근 검정</h2>",
  html_table(data.frame(계열 = ur$series, `ADF τ` = f(ur$adf_tau, 2), `5% 임계값` = f(ur$adf_cv5, 2), KPSS = f(ur$kpss, 3), `KPSS p` = fp(ur$kpss_p),
                        `I(1) 판정` = ifelse(ur$I1, "예", "아니오"), `분석에 쓴 형태` = ur$used_as, check.names = FALSE),
             "표 A1. ADF(상수항, AIC 시차)와 KPSS(수준 정상성)", "I(1) 판정 = ADF가 단위근을 기각하지 못하고 KPSS가 정상성을 기각. KPSS p는 표 보간값(0.01–0.10에서 잘림).", cls = "small"),
  "<h2>부록 B. 재현 방법</h2>",
  "<ul><li><code>Rscript code/02_analysis.R</code>: 분석 전체(몇 분). 결과는 <code>output/</code>(RData, 표 csv, 그림 png).</li>",
  "<li><code>Rscript code/03_results_report.R</code>: 이 문서(HTML, PDF).</li>",
  "<li><code>TEAM=Team3 Rscript code/00_build_submission.R</code>: 제출용 단일 R 파일. 수집 코드는 <code>if (FALSE)</code> 블록 안에 들어간다.</li></ul>")

css <- "
@page { size: A4; margin: 16mm 15mm 16mm 15mm; }
body { font-family: 'Apple SD Gothic Neo', 'Noto Sans KR', sans-serif; font-size: 9.6pt; line-height: 1.55; color: #0b0b0b; background: #fff; }
h1 { font-size: 19pt; margin: 0 0 2pt; } .sub { color: #52514e; margin: 0 0 12pt; }
h2 { font-size: 13pt; margin: 18pt 0 6pt; padding-bottom: 3pt; border-bottom: 1.5px solid #0b0b0b; break-after: avoid; }
h3 { font-size: 11pt; margin: 0 0 4pt; }
p { margin: 4pt 0 6pt; } ul, ol { margin: 4pt 0 8pt; padding-left: 18pt; } li { margin: 2pt 0; }
.key { border: 1px solid #b9b8b0; border-left: 4px solid #2a78d6; padding: 8pt 12pt; background: #f6f8fb; margin: 8pt 0 10pt; break-inside: avoid; }
figure { margin: 8pt 0 10pt; break-inside: avoid; } figcaption { font-weight: 700; font-size: 9.2pt; margin: 0 0 4pt; }
.fig img { display: block; border: 1px solid #e4e3dd; } .fig figcaption { margin: 4pt 0 0; }
table { border-collapse: collapse; width: 100%; font-size: 8.6pt; font-variant-numeric: tabular-nums; }
th.grp { border-top: 1.2px solid #0b0b0b; border-bottom: 0.5px solid #b9b8b0; text-align: center; background: #fff; }
th { border-top: 1.2px solid #0b0b0b; border-bottom: 0.8px solid #0b0b0b; padding: 3pt 5pt; font-weight: 700; background: #f3f3f0; }
td { border-bottom: 0.5px solid #e1e0da; padding: 2.5pt 5pt; } tr:last-child td { border-bottom: 1.2px solid #0b0b0b; }
.small table { font-size: 7.9pt; } .note { font-size: 8pt; color: #52514e; margin: 3pt 0 0; }
code { font-size: 8.6pt; background: #f3f3f0; padding: 0 2pt; }
"
html <- paste0("<!doctype html><html lang='ko'><head><meta charset='utf-8'><title>분석 결과 정리</title><style>", css, "</style></head><body>",
  "<h1>조건별 금리 전이: 분석 결과 정리</h1>",
  "<p class='sub'>When Do US Yields Cross the Pacific? · 금융계량경제학 기말 프로젝트 · ", format(Sys.Date()), " · 표본 ", per, ", 거래일 쌍 ",
  format(n_pairs, big.mark = ","), "개</p>",
  key, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, sA, "</body></html>")
writeLines(html, OUT_HTML, useBytes = TRUE)
message("wrote ", OUT_HTML)

# PDF via headless Chrome
chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if (file.exists(chrome)) {
  args <- c("--headless=new", "--disable-gpu", "--no-pdf-header-footer", "--allow-file-access-from-files",
            paste0("--print-to-pdf=", shQuote(normalizePath(OUT_PDF, mustWork = FALSE))),
            shQuote(paste0("file://", normalizePath(OUT_HTML))))
  system2(chrome, args, stdout = FALSE, stderr = FALSE)
  message("wrote ", OUT_PDF)
}
