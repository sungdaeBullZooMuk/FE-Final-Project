# =============================================================================
# 05_paper_tables.R
# Renders the paper's main-text tables 1-6 and figure 2 (Korean captions) from
# res$paper in output/Team_result.RData, as a preview for writing the paper.
# Layout follows docs/조건별전이_보고서_구성안.md (11pt, line spacing 1.3).
# Run from "Final Project":  Rscript code/05_paper_tables.R
# Not part of the submission file.
# =============================================================================
load("output/Team_result.RData")
stopifnot(!is.null(res$paper))
pt <- res$paper
OUT_HTML <- "output/report/paper_tables.html"
OUT_PDF  <- "reports/본문_표_초안.pdf"
dir.create(dirname(OUT_HTML), recursive = TRUE, showWarnings = FALSE)

f  <- function(x, d = 3) ifelse(is.na(x), "–", formatC(x, format = "f", digits = d))
fp <- function(p) ifelse(is.na(p), "–", ifelse(p < 0.001, formatC(p, format = "e", digits = 1), formatC(p, format = "f", digits = 3)))
bold_if <- function(txt, cond) ifelse(!is.na(cond) & cond, paste0("<b>", txt, "</b>"), txt)
tg_ko <- c(k3 = "3년물 (← 미 2년)", k10 = "10년물 (← 미 10년)")
hyp_ko <- c(H1 = "P1 기대: s × |kr1y_gap|", H2 = "P2 수급: s⁺ × fut3", H3 = "P3 위험: s × log VIX")
verdict_ko <- c("not rejected" = "기각 못 함", "spec-dependent" = "설정 의존",
                "significant (opposite sign)" = "유의 (예측과 반대 부호)", "significant (predicted sign)" = "유의 (예측 부호)")

tbl <- function(head_rows, body_rows, caption, note = NULL, align = NULL) {
  paste0("<figure class='tbl'><figcaption>", caption, "</figcaption><table>",
         paste(head_rows, collapse = ""), paste(body_rows, collapse = ""), "</table>",
         if (!is.null(note)) paste0("<p class='note'>", note, "</p>"), "</figure>")
}
tr <- function(cells, tag = "td", cls = NULL) {
  cl <- if (is.null(cls)) "" else paste0(" class='", cls, "'")
  paste0("<tr", cl, ">", paste0("<", tag, ">", cells, "</", tag, ">", collapse = ""), "</tr>")
}
th_span <- function(labels, spans) paste0("<tr>", paste0("<th class='grp' colspan='", spans, "'>", labels, "</th>", collapse = ""), "</tr>")
src <- function(...) paste0("<br><span class='src'>출처: ", paste(..., sep = ", "), "</span>")

# ---- 표 1 --------------------------------------------------------------------------
t1 <- pt$table1
lab1 <- c(dy_kr3y = "국고채 3년 일별 변화 Δy (bp)", dy_kr10y = "국고채 10년 일별 변화 Δy (bp)",
          s_us2y = "미 2년 충격 s (bp)", s_us10y = "미 10년 충격 s (bp)",
          abs_kr1y_gap = "|국고채 1년 − 기준금리| (%p)", fut3_cum = "외국인 3년 선물 누적 순매수 (250일 z)", vix = "log VIX",
          "mon share (k3)" = "통화정책형 날 비율 mon = 1 (3년물 짝)", "mon share (k10)" = "통화정책형 날 비율 mon = 1 (10년물 짝)",
          "FLAT share (k3)" = "단기 주도 날 비율 FLAT = 1 (3년물 짝)", "FLAT share (k10)" = "단기 주도 날 비율 FLAT = 1 (10년물 짝)")
is_sh <- grepl("share", t1$variable)
body1 <- sapply(seq_len(nrow(t1)), function(i) with(t1[i, ], tr(c(lab1[variable],
  if (is_sh[i]) c("", paste0(f(100 * mean, 1), "%"), "", "", "", "") else
    c(format(n, big.mark = ","), f(mean, 3), f(sd, 3), f(min, 2), f(median, 2), f(max, 2))))))
T1 <- tbl(tr(c("변수", "N", "평균", "표준편차", "최소", "중앙값", "최대"), "th"), body1,
  "표 1. 기술통계 (2010-01-04 ~ 2026-10-30, 한국 거래일 쌍)",
  paste0("Δy는 한국 t일 종가 → t+1일 종가 변화, s는 그사이 끝난 미국 거래일 변화의 합이다. 조건은 한국 t일 마감 때 알려진 값이다. ",
         "비율은 s ≠ 0인 날 기준이다.", src("paper_table1.csv")))

# ---- 표 2 --------------------------------------------------------------------------
t2 <- pt$table2
body2 <- sapply(seq_len(nrow(t2)), function(i) with(t2[i, ], tr(c(tg_ko[target],
  paste0(f(beta0), " (", f(t_beta0, 1), ")"), paste0(f(rho), " (", f(t_rho, 1), ")"), f(r2, 3), format(n, big.mark = ","),
  f(beta_up), f(beta_down), fp(p_up_eq_down), f(bp_per_10bp, 1), f(sd_dy_bp, 1)))))
T2 <- tbl(c(th_span(c("", "기준 모형 (1)", "상승·하락 분리", "경제적 크기"), c(1, 4, 3, 2)),
            tr(c("목표 (← 충격)", "β₀ (t)", "ρ (t)", "R²", "N", "β 상승일", "β 하락일", "같음 p", "미 10bp → bp", "Δy 표준편차 (bp)"), "th")),
  body2, "표 2. 기준 전이: 미국 금리 충격의 다음 날 국고채 전이",
  paste0("식 (1): Δy(t+1) = α + β₀ s(t) + ρ Δy(t) + δ′D(t) + ε. D는 휴장·FOMC·금통위 더미. 괄호 안은 Newey–West(시차 9) t값. ",
         "'같음 p'는 β 상승일 = β 하락일의 HAC Wald 검정.", src("paper_table2.csv")))

# ---- 표 3 --------------------------------------------------------------------------
t3 <- pt$table3; j3 <- pt$table3_joint
body3 <- unlist(lapply(names(tg_ko), function(tg) {
  d <- t3[t3$target == tg, ]
  c(tr(c(paste0("<b>", tg_ko[tg], "</b>"), rep("", 7)), cls = "sec"),
    sapply(seq_len(nrow(d)), function(i) with(d[i, ], tr(c(hyp_ko[hyp],
      bold_if(f(est_A), holm_A < 0.05), fp(p_A), bold_if(fp(holm_A), holm_A < 0.05),
      bold_if(f(est_B), holm_B < 0.05), fp(p_B), bold_if(fp(holm_B), holm_B < 0.05), verdict_ko[verdict])))),
    with(j3[j3$target == tg, ], tr(c("결합 Wald χ²(3): 세 계수 = 0",
      f(W_A, 2), fp(p_A), fp(holm_A), f(W_B, 2), fp(p_B), fp(holm_B), ""), cls = "joint")))
}))
T3 <- tbl(c(th_span(c("", "설정 A: trend 있음", "설정 B: trend 없음", ""), c(1, 3, 3, 1)),
            tr(c("가설 (예측 부호 +)", "계수", "p", "Holm p", "계수", "p", "Holm p", "판정"), "th")),
  body3, "표 3. 사전 가설 (1부): 시장 조건에 따른 전이 차이",
  paste0("식 (2)의 상호작용 계수. 조건은 표준화했으므로 계수는 조건 1 표준편차당 전이 계수의 변화다. P2는 s를 상승(s⁺)·하락(s⁻)으로 나눈 식에서 s⁺ 항이다. ",
         "양측 검정, Newey–West(시차 9). Holm은 설정마다 6개 검정(가설 3 × 만기 2)에 적용한다. 굵게 = Holm p < 0.05. ",
         "판정 규칙: A·B 모두 통과하면 '유의', 하나만 통과하면 '설정 의존'. ",
         "결합 Wald 행의 계수 칸은 χ² 통계량이고, Holm은 만기 2개에 적용한다.", src("paper_table3.csv", "paper_table3_joint.csv")))

# ---- 표 4 --------------------------------------------------------------------------
t4 <- pt$table4; n3 <- pt$table4_n3
test_ko <- c(N2 = "N2 s × FLAT > 0 (단측 t)", N3 = "N3 c_m > c_g (부호 붙인 LR)",
             N5 = "N5 M4 vs M1 (Clark–West)", N7 = "N7 M4 vs M2 (GW 무조건부, 단측)")
eff_ko <- c(N2 = "s × FLAT 계수", N3 = "c_m − c_g", N5 = "R²_OOS", N7 = "R²_OOS")
efmt <- function(code, x) if (code %in% c("N5", "N7")) paste0(f(100 * x, 2), "%") else f(x, 3)
codes <- unique(substr(t4$test, 1, 2))
body4 <- sapply(codes, function(cd) {
  cells <- c(test_ko[cd], eff_ko[cd])
  for (tg in names(tg_ko)) { r <- t4[substr(t4$test, 1, 2) == cd & t4$target == tg, ]
    cells <- c(cells, efmt(cd, r$effect_value), f(r$stat, 2), fp(r$p), bold_if(fp(r$p_holm), r$p_holm < 0.05)) }
  tr(cells) })
n3txt <- if (!is.null(n3)) paste0("N3의 성격별 c: 10년물 c_m ", f(n3$c_m[n3$target == "k10"], 2), " (", f(n3$se_c_m[n3$target == "k10"], 2), "), c_g ",
  f(n3$c_g[n3$target == "k10"], 2), " (", f(n3$se_c_g[n3$target == "k10"], 2), "); 3년물 c_m ", f(n3$c_m[n3$target == "k3"], 2),
  ", c_g ", f(n3$c_g[n3$target == "k3"], 2), ". ") else ""
T4 <- tbl(c(th_span(c("", "", "3년물", "10년물"), c(1, 1, 4, 4)),
            tr(c("검정 (미리 정한 예측)", "효과 지표", "효과", "통계량", "p", "Holm p", "효과", "통계량", "p", "Holm p"), "th")),
  body4, "표 4. 충격 성격 (3부): 미리 고정한 검정 8개",
  paste0("mon = 1은 미국 금리 변화와 같은 날 NASDAQ 수익률의 부호가 반대인 날(통화정책형, Hoek, Kamin & Yoldas 2022의 일별 근사), ",
         "FLAT = 1은 |Δ미 2년| > |Δ미 10년|인 날이다. N3는 GARCH(1,1)-X 분산식에 s²·mon과 s²·(1 − mon)을 넣은 모형(Student-t). ",
         "N5·N7의 R²_OOS = 1 − MSFE(M4)/MSFE(비교 모형), 1,000일 롤링. M1 = 기준, M2 = M1 + 조건 3개, M4 = M1 + mon. ",
         "Holm은 8개 검정(4 × 만기 2)에 함께 적용한다. 굵게 = Holm p < 0.05. ", n3txt,
         "1부(설정 A) 6개와 합친 14개 Holm에서도 10년물 N2(", fp(t4$p_holm_14[substr(t4$test, 1, 2) == "N2" & t4$target == "k10"]),
         ")와 N5(", fp(t4$p_holm_14[substr(t4$test, 1, 2) == "N5" & t4$target == "k10"]), ")는 통과한다.",
         src("paper_table4.csv", "paper_table4_n3.csv")))

# ---- 표 5 --------------------------------------------------------------------------
t5 <- pt$table5
item_ko <- c("baseline: 10 bp US shock" = "기준 전이 (미 10bp 충격)",
             "change per +1 s.d. of H1 (spec A)" = "P1 |kr1y_gap| +1 표준편차일 때 변화",
             "change per +1 s.d. of H2 (spec A)" = "P2 외국인 선물 +1 표준편차일 때 변화 (미 금리 상승일)",
             "change per +1 s.d. of H3 (spec A)" = "P3 log VIX +1 표준편차일 때 변화",
             "growth-type day (mon = 0)" = "성장형 날 (mon = 0)", "monetary-type day (mon = 1)" = "통화정책형 날 (mon = 1)",
             "s.d. of daily KTB change" = "참고: 국고채 일별 변화의 표준편차")
ord5 <- c(1, 5, 6, 2, 4, 3, 7)
body5 <- sapply(ord5, function(i) with(t5[i, ], tr(c(item_ko[item], f(k3, 2), f(k10, 2)), cls = if (i == 7) "ref" else NULL)))
T5 <- tbl(tr(c("미국 금리 10bp 충격일 때 다음 날 국고채 변화 (bp)", "3년물", "10년물"), "th"), body5,
  "표 5. 경제적 크기",
  paste0("기준 전이 = 10 × β₀(표 2). 성장형·통화정책형 = 10 × 각 날의 전이 계수(s × mon을 넣은 식 N1. 효과를 본 뒤 정한 식이라 확증 검정에는 쓰지 않는다).",
         "조건 행 = 10 × 상호작용 계수(설정 A, 표 3). 조건 효과와 성격 효과의 크기 비교는 검정이 아니다(직접 비교 N7은 표 4).",
         src("paper_table5.csv")))

# ---- 표 6 --------------------------------------------------------------------------
t6 <- pt$table6
chk_ko <- function(x) {
  m <- c("Part 1 spec A" = "본 설정 A (trend)", "Part 1 spec B" = "설정 B (trend 없음)",
         "Part 1 spec C" = "설정 C (A + mon 통제)", "Part 1 spec D" = "설정 D (B + mon 통제)",
         "R1 shock us2y" = "R1 충격을 미 2년으로", "R1 shock us3y" = "R1 충격을 미 3년으로", "R1 shock us5y" = "R1 충격을 미 5년으로",
         "R3 drop FOMC +-3 days" = "R3 FOMC ±3일 제외", "R4 2010-2019" = "R4 2010–2019 (trend 없음)",
         "R4 2020-2026" = "R4 2020–2026 (trend 없음)", "R5 winsorized 1%" = "R5 1% 윈저화",
         "R6 5-day average conditions" = "R6 조건 5일 평균", "R7 signed gap, fx_vol" = "R7 부호 있는 gap, VIX 대신 환율 변동성",
         "R8 st_spread for H2" = "R8 H2 조건을 st_spread로", "R9 sample from 2012-05-31" = "R9 2012-05-31 이후",
         "R10 add s x |s|" = "R10 s × |s| 추가", "R11 within-year (s x year)" = "R11 연도 내 (s × 연도, trend 없음)")
  out <- m[x]; z <- grepl("^R12", x)
  out[z] <- sub("R12 fut3 z-window (\\d+) days, spec (.)", "R12 fut3 z 창 \\1일, 설정 \\2", x[z]); unname(out) }
pc <- function(p, h) ifelse(is.na(p), "–", ifelse(is.na(h), fp(p), paste0(fp(p), " (", bold_if(fp(h), h < 0.05), ")")))
body6 <- sapply(seq_len(nrow(t6)), function(i) with(t6[i, ], tr(c(chk_ko(check),
  f(vix_est), pc(vix_p, vix_holm), f(fut3p_est), pc(fut3p_p, fut3p_holm), f(fut3sym_est), pc(fut3sym_p, NA)),
  cls = if (i == 1) "main" else NULL)))
T6 <- tbl(c(th_span(c("", "P3: s × log VIX", "P2: s⁺ × fut3", "참고: s × fut3 (대칭)"), c(1, 2, 2, 2)),
            tr(c("설정 (10년물)", "계수", "p (Holm)", "계수", "p (Holm)", "계수", "p"), "th")),
  body6, "표 6. 견고성: 10년물 두 결과",
  paste0("괄호 안 Holm p는 해당 설정의 1부 6개 검정 묶음 기준이며, 1부 설정과 R12에만 있다. R1–R9는 fut3를 대칭(s × fut3)으로 넣은 식이라 s⁺ 항이 없다. ",
         "R1의 미 2년 충격은 10년물에 짧은 만기 충격을 넣은 것이다. 3년물은 어떤 설정에서도 조건 효과가 Holm을 통과하지 않는다.",
         src("paper_table6.csv")))

# ---- 그림 1: 시점 정렬 도식 (SVG) ---------------------------------------------------
# Panel (a) a normal day, panel (b) the real holiday case stored by 02_analysis.R
# (res$alignment_case, checked there with stopifnot).
ac <- res$alignment_case
us_days <- seq(ac$date, ac$date1 - 1, by = "day")
us_days <- us_days[!format(us_days, "%u") %in% c("6", "7")]          # US weekdays t <= d < t+1
stopifnot(length(us_days) == ac$n_us)
kr_gap <- seq(ac$date + 1, ac$date1 - 1, by = "day"); kr_gap <- kr_gap[!format(kr_gap, "%u") %in% c("6", "7")]
wd <- function(d) c("월", "화", "수", "목", "금", "토", "일")[as.integer(format(d, "%u"))]
md <- function(d) paste0(as.integer(format(d, "%m")), "/", as.integer(format(d, "%d")))
sg <- function(x) sprintf("%+.1f", x)
KR <- "#2a78d6"; US <- "#eb6834"; INK <- "#141413"; MUTE <- "#5b5a55"
tx <- function(x, y, s, size = 13, weight = 400, fill = INK, anchor = "middle")
  sprintf("<text x='%g' y='%g' text-anchor='%s' font-size='%g' font-weight='%d' fill='%s'>%s</text>", x, y, anchor, size, weight, fill, s)
bx <- function(x, y, w, h, col, fillc) sprintf("<rect x='%g' y='%g' width='%g' height='%g' rx='5' fill='%s' stroke='%s' stroke-width='1.2'/>", x, y, w, h, fillc, col)
ln <- function(x1, y1, x2, y2, col = INK, w = 1.2, dash = NULL, arrow = FALSE)
  sprintf("<line x1='%g' y1='%g' x2='%g' y2='%g' stroke='%s' stroke-width='%g'%s%s/>", x1, y1, x2, y2, col, w,
          if (is.null(dash)) "" else paste0(" stroke-dasharray='", dash, "'"), if (arrow) " marker-end='url(#ar)'" else "")
brk <- function(x1, x2, y, up = FALSE) { d <- if (up) -12 else 12
  sprintf("<path d='M%g,%g L%g,%g L%g,%g L%g,%g' fill='none' stroke='%s' stroke-width='1.4'/>", x1, y, x1, y + d, x2, y + d, x2, y, INK) }
box2 <- function(x, y, w, col, fillc, l1, l2)    # two-line box, 60 high
  paste0(bx(x, y, w, 60, col, fillc), tx(x + w / 2, y + 26, l1, 19, 700, col), tx(x + w / 2, y + 49, l2, 16.5, 400, col))
# sizes in viewBox units: the figure is ~170 mm wide in the paper, so 19 ~ 9 pt and 16.5 ~ 8 pt
nb <- length(us_days); ux0 <- 330; uw <- (420 - 16 * (nb - 1)) / nb
svg1 <- paste0(
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 1000 505' font-family=\"'Apple SD Gothic Neo','Noto Sans KR',sans-serif\">",
  "<defs><marker id='ar' markerWidth='8' markerHeight='8' refX='7' refY='4' orient='auto'><path d='M0,0 L8,4 L0,8 z' fill='", MUTE, "'/></marker></defs>",
  "<rect width='1000' height='505' fill='#ffffff'/>",
  # panel (a)
  tx(20, 26, "(a) 보통의 하루", 20, 700, INK, "start"),
  tx(40, 62, "조건 z(t) 확정 (한국 t일 마감)", 19, 700, INK, "start"),
  tx(40, 85, "|kr1y_gap|, 외국인 선물 포지션, log VIX", 16.5, 400, MUTE, "start"),
  ln(250, 94, 250, 122, MUTE, 1.2, "4 3"),
  tx(620, 62, "충격 s(t) = 미국 금리 변화 (bp)", 19, 700),
  tx(620, 85, "같은 밤 주가 방향 → mon(t), 곡선 모양 → FLAT(t)", 16.5, 400, MUTE),
  ln(30, 184, 975, 184, INK, 1.5, arrow = TRUE),
  box2(40, 124, 210, KR, "#eef4fc", "한국 t일 마감", "종가 y(t)"),
  box2(400, 124, 280, US, "#fdf1eb", "미국 t일 장", "한국 시간으로 밤"),
  box2(760, 124, 200, KR, "#eef4fc", "한국 t+1일 마감", "종가 y(t+1)"),
  brk(145, 860, 192), tx(500, 234, "목표 Δy(t+1) = y(t+1) − y(t) (bp)", 19, 700),
  # divider
  ln(20, 258, 980, 258, "#d9d8d2", 1),
  # panel (b)
  tx(20, 292, paste0("(b) 한국 휴장이 낀 실제 사례: ", format(ac$date, "%Y"), "년 ", md(ac$date), " → ", md(ac$date1)), 20, 700, INK, "start"),
  tx(540, 326, paste0("s(t) = 미 2년 금리 ", md(max(us_days)), " 종가 − ", md(min(us_days) - 1), " 종가 = ", sg(ac$s_us2y), "bp"), 19, 700),
  tx(540, 350, paste0("미국 거래일 ", nb, "개 변화의 합. 한국은 ",
                      if (length(kr_gap)) paste0(md(min(kr_gap)), "–", md(max(kr_gap)), " 휴장") else "휴장 없음", " (더미 hol = 1)"), 16.5, 400, MUTE),
  brk(ux0, ux0 + 420, 372, up = TRUE),
  ln(30, 436, 975, 436, INK, 1.5, arrow = TRUE),
  box2(40, 376, 210, KR, "#eef4fc", paste0("한국 ", md(ac$date), "(", wd(ac$date), ")"), "종가"),
  paste0(sapply(seq_len(nb), function(i) { x <- ux0 + (i - 1) * (uw + 16)
    box2(x, 376, uw, US, "#fdf1eb", paste0(md(us_days[i]), "(", wd(us_days[i]), ")"), "미국 장") }), collapse = ""),
  box2(790, 376, 170, KR, "#eef4fc", paste0("한국 ", md(ac$date1), "(", wd(ac$date1), ")"), "종가"),
  brk(145, 875, 444), tx(510, 488, paste0("Δy(t+1) 국고채 3년 = ", sg(ac$dy_kr3y), "bp (", md(ac$date), " 종가 → ", md(ac$date1), " 종가)"), 19, 700),
  "</svg>")
FIG1_SVG <- "output/figures/paper_figure1_timing.svg"
writeLines(svg1, FIG1_SVG, useBytes = TRUE)
F1 <- paste0("<figure class='fig'><figcaption>그림 1. 자료의 시점 정렬</figcaption>",
             "<img src='../figures/paper_figure1_timing.svg'><p class='note'>",
             "조건은 충격보다 먼저 알려진 값(한국 t일 마감)만 쓴다. 따라서 회귀의 어떤 설명변수도 목표 Δy(t+1)보다 뒤의 정보를 담지 않는다. ",
             "미국 휴장일은 그날 변화가 0으로 들어가고, 결측은 보간하지 않는다. (b)는 02_analysis.R가 stopifnot으로 확인하는 정렬 사례다.", src("paper_figure1_timing.svg (05_paper_tables.R가 생성)"), "</p></figure>")

# ---- 그림 2 ------------------------------------------------------------------------
F2 <- paste0("<figure class='fig'><figcaption>그림 2. 250일 롤링 전이 계수 (점선 = 전체 표본)</figcaption>",
             "<img src='../figures/figure5_rolling_beta.png'><p class='note'>그림 안의 영문 제목은 본문 작성 때 지운다.",
             src("figure5_rolling_beta.png"), "</p></figure>")

css <- "
@page { size: A4; margin: 22mm 20mm; }
body { font-family: 'AppleMyungjo', 'Nanum Myeongjo', serif; font-size: 11pt; line-height: 1.3; color: #0b0b0b; background: #fff; }
h1 { font-family: 'Apple SD Gothic Neo', sans-serif; font-size: 15pt; margin: 0 0 4pt; }
.sub { font-family: 'Apple SD Gothic Neo', sans-serif; font-size: 9pt; color: #52514e; margin: 0 0 14pt; }
figure { margin: 0 0 18pt; break-inside: avoid; }
figcaption { font-family: 'Apple SD Gothic Neo', sans-serif; font-weight: 700; font-size: 10pt; margin: 0 0 5pt; }
table { border-collapse: collapse; width: 100%; font-family: 'Apple SD Gothic Neo', sans-serif; font-size: 8.6pt; font-variant-numeric: tabular-nums; }
th { border-bottom: 0.8px solid #0b0b0b; padding: 3pt 4pt; font-weight: 600; text-align: right; }
tr:first-child th { border-top: 1.4px solid #0b0b0b; }
th.grp { text-align: center; border-bottom: 0.5px solid #8a8984; }
th:first-child, td:first-child { text-align: left; }
td { padding: 2.4pt 4pt; text-align: right; }
tr:last-child td { border-bottom: 1.4px solid #0b0b0b; }
tr.sec td { padding-top: 6pt; } tr.joint td { border-top: 0.5px solid #c9c8c2; color: #3a3a38; }
tr.main td { background: #f3f3f0; } tr.ref td { color: #52514e; border-top: 0.5px solid #c9c8c2; }
.note { font-family: 'Apple SD Gothic Neo', sans-serif; font-size: 8pt; line-height: 1.4; color: #3a3a38; margin: 4pt 0 0; }
.src { color: #8a8984; }
.fig img { display: block; width: 100%; border: 0.5px solid #e4e3dd; }
"
html <- paste0("<!doctype html><html lang='ko'><head><meta charset='utf-8'><title>본문 표 초안</title><style>", css, "</style></head><body>",
  "<h1>본문 표·그림 초안 (그림 1–2, 표 1–6)</h1>",
  "<p class='sub'>보고서 구성안 기준 · 수치는 output/Team_result.RData의 res$paper에서 자동 생성 · ", format(Sys.Date()),
  "</p>",
  F1, T1, T2, F2, T3, T4, T5, T6, "</body></html>")
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
