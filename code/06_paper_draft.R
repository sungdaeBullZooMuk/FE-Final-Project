# =============================================================================
# 06_paper_draft.R
# Assembles the paper draft: the section texts in paper/*.md (in file-name order)
# with tables and figures from 05_paper_tables.R put where the text has a line
# "[[표 n]]" or "[[그림 n]]". Output: reports/본문_초안.pdf.
# Run from "Final Project":  Rscript code/06_paper_draft.R
# Not part of the submission file.
# =============================================================================
library(commonmark)
E <- new.env()
sys.source("code/05_paper_tables.R", envir = E)       # builds T1-T6, F1-F2 (and refreshes the table preview)
OUT_HTML <- "output/report/paper_draft.html"
OUT_PDF  <- "reports/본문_초안.pdf"

blocks <- c("표 1" = E$T1, "표 2" = E$T2, "표 3" = E$T3, "표 4" = E$T4, "표 5" = E$T5, "표 6" = E$T6,
            "그림 1" = E$F1, "그림 2" = E$F2,
            "표 A1" = E$TA1, "표 A2" = E$TA2, "표 A3" = E$TA3, "표 A4" = E$TA4, "표 A5" = E$TA5)
files <- sort(list.files("paper", pattern = "\\.md$", full.names = TRUE))
stopifnot(length(files) > 0)
md_text <- function(f) gsub("(?s)<!--.*?-->", "", paste(readLines(f, encoding = "UTF-8"), collapse = "\n"), perl = TRUE)  # drop editor notes
body <- paste(sapply(files, function(f) markdown_html(md_text(f), extensions = TRUE, smart = FALSE)), collapse = "\n")
used <- character(0)
for (k in names(blocks)) {
  tag <- paste0("<p>[[", k, "]]</p>")
  if (grepl(tag, body, fixed = TRUE)) { body <- gsub(tag, blocks[[k]], body, fixed = TRUE); used <- c(used, k) }
}
left <- regmatches(body, gregexpr("\\[\\[[^]]+\\]\\]", body))[[1]]
if (length(left)) stop("unknown placeholders: ", paste(left, collapse = ", "))

css <- paste0(E$css, "
h1 { font-size: 16pt; line-height: 1.35; margin: 0 0 4pt; }
h2 { font-family: 'Apple SD Gothic Neo', sans-serif; font-size: 13pt; margin: 18pt 0 8pt; break-after: avoid; }
h3 { font-family: 'Apple SD Gothic Neo', sans-serif; font-size: 11pt; margin: 14pt 0 5pt; break-after: avoid; }
body { font-family: 'Apple SD Gothic Neo', 'Noto Sans KR', sans-serif; font-size: 11pt; line-height: 1.3; }  /* team decision 11-24; the serif face mangles χ², R² */
p { margin: 0 0 7pt; } ul { margin: 0 0 8pt; padding-left: 16pt; } li { margin: 0 0 4pt; }
figure { margin: 10pt 0 14pt; }
pre { font-family: 'Apple SD Gothic Neo', sans-serif; font-size: 10pt; line-height: 1.45; margin: 6pt 0 9pt 12pt; white-space: pre-wrap; break-inside: avoid; }  /* equations */
code { font-family: Menlo, monospace; font-size: 9.5pt; }
pre code { font-family: inherit; font-size: inherit; }
")
html <- paste0("<!doctype html><html lang='ko'><head><meta charset='utf-8'><title>본문 초안</title><style>", css, "</style></head><body>",
               "<p class='sub'>본문 초안 · ", paste(basename(files), collapse = ", "), " · 표·그림: ", paste(used, collapse = ", "),
               " · ", format(Sys.Date()), "</p>", body, "</body></html>")
writeLines(html, OUT_HTML, useBytes = TRUE)
message("wrote ", OUT_HTML, " (placeholders filled: ", paste(used, collapse = ", "), ")")

chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if (file.exists(chrome)) {
  args <- c("--headless=new", "--disable-gpu", "--no-pdf-header-footer", "--allow-file-access-from-files",
            paste0("--print-to-pdf=", shQuote(normalizePath(OUT_PDF, mustWork = FALSE))),
            shQuote(paste0("file://", normalizePath(OUT_HTML))))
  system2(chrome, args, stdout = FALSE, stderr = FALSE)
  message("wrote ", OUT_PDF)
}
