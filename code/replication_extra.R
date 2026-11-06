## ALL FILE PATHS IN SCRIPT ARE RELATIVE TO THIS SCRIPT'S FOLDER LOCATION (code/)

rm(list = ls())

library(data.table, warn.conflicts = FALSE)

# Local Overleaf path, to copy output directly.
overleaf <- "/Users/sgdodd/Library/CloudStorage/Dropbox-Penn/Stephanie Dodd/Apps/Overleaf/Stat 5200/replication exhibits"

# Different path on my personal computer
if (Sys.info()[["user"]] == "stephaniedodd") {
	overleaf <- "/Users/stephaniedodd/Penn Dropbox/Stephanie Dodd/Apps/Overleaf/Stat 5200/replication exhibits"
}

#####################################################
# Pull in publications data
#####################################################

# Data output from dta_to_csv.R
pubs <- fread("../data/raw/Surnames_pubs_data.csv")

# Number of authors: count non-blank author fields.
# The "authors" var in the data doesn't match the paper's counts (5 papers coded 0, 8 undercounted); the count does.
pubs[, nauth := 0]
for (i in 1:5) {
	pubs[get(paste0("author", i)) != "", nauth := nauth + 1]
}

# Surname = last word of the author name, after dropping suffixes (jr., ii, iii) and trailing initials ("bobenrieth h.")
# Then alphabetical index of the surname initial: A = 1 ... Z = 26
for (i in 1:5) {
	pubs[, name := trimws(get(paste0("author", i)))]
	pubs[, name := sub(" (jr\\.?|sr\\.?|ii|iii|iv)$", "", name)]
	pubs[, name := sub(" [a-z]\\.$", "", name)]
	pubs[, surname := sub(".* ", "", name)]
	pubs[name == "", surname := NA_character_]
	pubs[, paste0("ini", i) := match(toupper(substr(surname, 1, 1)), LETTERS)]
}
pubs[, c("name", "surname") := NULL]

# Mean initial of all authors on the paper
pubs[, mean_ini := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0("ini", 1:5)]

# Alphabetical = author initials in (weakly) increasing order; same initial counts as alphabetical.
# Single-authored papers count as alphabetical, but they're only shown as their own row.
pubs[, alph := 1]
for (i in 1:4) {
	pubs[!is.na(get(paste0("ini", i + 1))) & get(paste0("ini", i + 1)) < get(paste0("ini", i)), alph := 0]
}
pubs[, nonalph := 1 - alph]

pubs[, multi := 0]
pubs[nauth > 1, multi := 1]


##############################################################
# Table 5: Publication statistics by number of authors
##############################################################

## Significance tests, following Einav and Yariv (2004) Table 7: regressions of mean initial on number-of-authors dummies, interacted with alphabetical order
# Paper's tests are directional ("significantly lower/higher"), so one-sided at 5 percent: right sign and two-sided p < 0.10

# ** Mean initial significantly lower than single-authored papers (5 percent)
reg_nauth <- lm(mean_ini ~ factor(nauth), data = pubs)
reg_multi <- lm(mean_ini ~ multi, data = pubs)

# ^^ Nonalphabetical mean initial significantly higher than alphabetical papers with the same number of authors (5 percent)
pubs_multi <- pubs[multi == 1]
reg_nonalph     <- lm(mean_ini ~ factor(nauth) + factor(nauth):nonalph, data = pubs_multi)
reg_nonalph_all <- lm(mean_ini ~ nonalph, data = pubs_multi)

coef_nauth       <- summary(reg_nauth)$coefficients
coef_multi       <- summary(reg_multi)$coefficients
coef_nonalph     <- summary(reg_nonalph)$coefficients
coef_nonalph_all <- summary(reg_nonalph_all)$coefficients


## Summary stats for each row of the table

# Rows by number of authors (share of all papers)
tab_nauth <- pubs[, .(n = .N, mean = mean(mean_ini), sd = sd(mean_ini)), by = nauth]
tab_nauth[, pct := 100 * n / nrow(pubs)]
setorder(tab_nauth, nauth)

# Alphabetical/nonalphabetical rows within number of authors (share of papers with that number of authors)
tab_alph <- pubs_multi[, .(n = .N, mean = mean(mean_ini), sd = sd(mean_ini)), by = .(nauth, alph)]
tab_alph[, pct := 100 * n / sum(n), by = nauth]

# All multiauthored rows (share of all papers, then share of multiauthored papers)
tab_multi <- pubs_multi[, .(n = .N, mean = mean(mean_ini), sd = sd(mean_ini))]
tab_multi[, pct := 100 * n / nrow(pubs)]

tab_multi_alph <- pubs_multi[, .(n = .N, mean = mean(mean_ini), sd = sd(mean_ini)), by = alph]
tab_multi_alph[, pct := 100 * n / sum(n)]


## Stars

tab_nauth[, stars := ""]
for (k in 2:5) {
	term <- paste0("factor(nauth)", k)
	tab_nauth[nauth == k & coef_nauth[term, "Estimate"] < 0 & coef_nauth[term, "Pr(>|t|)"] < 0.10, stars := "**"]
}

tab_alph[, stars := ""]
for (k in 2:5) {
	term <- paste0("factor(nauth)", k, ":nonalph")
	tab_alph[nauth == k & alph == 0 & coef_nonalph[term, "Estimate"] > 0 & coef_nonalph[term, "Pr(>|t|)"] < 0.10, stars := "$^{\\wedge\\wedge}$"]
}

tab_multi[, stars := ""]
tab_multi[coef_multi["multi", "Estimate"] < 0 & coef_multi["multi", "Pr(>|t|)"] < 0.10, stars := "**"]

tab_multi_alph[, stars := ""]
tab_multi_alph[alph == 0 & coef_nonalph_all["nonalph", "Estimate"] > 0 & coef_nonalph_all["nonalph", "Pr(>|t|)"] < 0.10, stars := "$^{\\wedge\\wedge}$"]


## Cell text: "N (pct%)", mean to 2 decimals with stars, sd to 2 decimals

tab_nauth[, row_txt := paste0(nauth, " & ", format(n, big.mark = ",", trim = TRUE), " (", sprintf("%.1f", pct), "\\%) & ",
	sprintf("%.2f", mean), stars, " & ", sprintf("%.2f", sd), " \\\\")]

tab_alph[, label := "\\quad Alphabetically"]
tab_alph[alph == 0, label := "\\quad Nonalphabetically"]
tab_alph[, row_txt := paste0(label, " & ", format(n, big.mark = ",", trim = TRUE), " (", sprintf("%.1f", pct), "\\%) & ",
	sprintf("%.2f", mean), stars, " & ", sprintf("%.2f", sd), " \\\\")]

tab_multi[, row_txt := paste0("All multiauthored & ", format(n, big.mark = ",", trim = TRUE), " (", sprintf("%.1f", pct), "\\%) & ",
	sprintf("%.2f", mean), stars, " & ", sprintf("%.2f", sd), " \\\\")]

tab_multi_alph[, label := "\\quad Alphabetically"]
tab_multi_alph[alph == 0, label := "\\quad Nonalphabetically"]
tab_multi_alph[, row_txt := paste0(label, " & ", format(n, big.mark = ",", trim = TRUE), " (", sprintf("%.1f", pct), "\\%) & ",
	sprintf("%.2f", mean), stars, " & ", sprintf("%.2f", sd), " \\\\")]


# Output tex file with formatted table
table5_tex <- c(
	"\\begin{tabular}{l *{3}{c}}",
	"\\toprule",
	"Number of authors & Obs. (\\%) & Mean initial & Std. dev. \\\\",
	"\\midrule",
	tab_nauth[nauth == 1]$row_txt,
	tab_nauth[nauth == 2]$row_txt,
	tab_alph[nauth == 2 & alph == 1]$row_txt,
	tab_alph[nauth == 2 & alph == 0]$row_txt,
	tab_nauth[nauth == 3]$row_txt,
	tab_alph[nauth == 3 & alph == 1]$row_txt,
	tab_alph[nauth == 3 & alph == 0]$row_txt,
	tab_nauth[nauth == 4]$row_txt,
	tab_alph[nauth == 4 & alph == 1]$row_txt,
	tab_alph[nauth == 4 & alph == 0]$row_txt,
	tab_nauth[nauth == 5]$row_txt,
	tab_alph[nauth == 5 & alph == 1]$row_txt,
	tab_alph[nauth == 5 & alph == 0]$row_txt,
	tab_multi$row_txt,
	tab_multi_alph[alph == 1]$row_txt,
	tab_multi_alph[alph == 0]$row_txt,
	"\\bottomrule",
	"\\end{tabular}"
	)

writeLines(table5_tex, "../output/table5_pubs_stats.tex")
writeLines(table5_tex, file.path(overleaf, "table5_pubs_stats.tex"))
