# Builds the single submission file required by the course guide:
#   submission/<TEAM>_rcode.R = collection code inside if (FALSE) { } + analysis code
# Run from the "Final Project" folder:  TEAM=Team3 Rscript code/00_build_submission.R
TEAM <- Sys.getenv("TEAM", "Team")
dir.create("submission", showWarnings = FALSE)
collect  <- readLines("code/01_collect_data.R", encoding = "UTF-8")
analysis <- readLines("code/02_analysis.R", encoding = "UTF-8")
analysis <- gsub("Team_result.RData", paste0(TEAM, "_result.RData"), analysis, fixed = TRUE)
analysis <- gsub("data/Data_team.csv", paste0("Data_", sub("^Team", "team", TEAM), ".csv"), analysis, fixed = TRUE)
out <- c(
  "# =============================================================================",
  sprintf("# %s_rcode.R  (FE Final Project, single submission file)", TEAM),
  "# Part 1 (inside if (FALSE)): data collection. Needs an ECOS API key and the manual",
  "#   KRX/FSS files, so it is not run by default. It produced the submitted csv.",
  "# Part 2: all analyses, starting from the csv. Put the csv in the working directory.",
  "# =============================================================================",
  "", "if (FALSE) {", collect, "}  # end of data collection", "", analysis)
writeLines(out, file.path("submission", paste0(TEAM, "_rcode.R")), useBytes = TRUE)
message("wrote submission/", TEAM, "_rcode.R (", length(out), " lines)")
