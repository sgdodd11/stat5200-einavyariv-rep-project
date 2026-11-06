## ALL FILE PATHS IN SCRIPT ARE RELATIVE TO THIS SCRIPT'S FOLDER LOCATION (code/)

rm(list = ls())

library(data.table, warn.conflicts = FALSE)
library(ggplot2)

# Overleaf LMRoman font
latex_font <- "LM Roman 10"

# Locan Overleaf path, to copy output directly. 
overleaf <- "/Users/sgdodd/Library/CloudStorage/Dropbox-Penn/Stephanie Dodd/Apps/Overleaf/Stat 5200/replication exhibits"

# Different path on my personal computer
if (Sys.info()[["user"]] == "stephaniedodd") {
	overleaf <- "/Users/stephaniedodd/Penn Dropbox/Stephanie Dodd/Apps/Overleaf/Stat 5200/replication exhibits"
}

#####################################################
# Pull in faculty data
#####################################################

# Data output from dta_to_csv.R (names already cleaned data_to_csv.R)
faculty <- fread("../data/raw/Surnames_faculty_data.csv")

# Sort alphabetically by last name
setorder(faculty, lname)

# Alphabetical order of surname initial: A = 1 ... Z = 26
faculty[, initial := toupper(substr(lname, 1, 1))]
faculty[, alph_order := match(initial, LETTERS)]


#####################################################
# Figure 1: CDF tenure by initial, for economists
#####################################################

# Restrict to econ data
econdata <- faculty[psych == 0]

# Rm people with missing tenure status. 
econdata <- econdata[!is.na(tenure)]

# Create untenured var for cdf 
econdata[, untenured := NA_integer_]
econdata[tenure == 0, untenured := 1]
econdata[tenure == 1, untenured := 0]

# CDFs of tenure and untenured, for economists, by department rank group
for (top in c(5, 10, 20, 35)) {

	if (top == 35) {
		fname <- "alldept"
	} else {
		fname <- paste0("top", top)
	}

	# Restrict to departments ranked 1 to top
	econ_top <- econdata[rank >= 1 & rank <= top]

	tenureCDF_econ <- ggplot() +
		stat_ecdf(data = econ_top[tenure == 1],
			aes(x = alph_order, color = "Tenured", linetype = "Tenured"),
			geom = "line", pad = FALSE, linewidth = 0.8) +
		stat_ecdf(data = econ_top[untenured == 1],
			aes(x = alph_order, color = "Untenured", linetype = "Untenured"),
			geom = "line", pad = FALSE, linewidth = 0.8) +
		# CDF hits 1 at the last observed initial; extend each line flat at 1 out to Z
		geom_segment(aes(x = max(econ_top[tenure == 1]$alph_order), xend = 26, y = 1, yend = 1,
			color = "Tenured", linetype = "Tenured"), linewidth = 0.8) +
		geom_segment(aes(x = max(econ_top[untenured == 1]$alph_order), xend = 26, y = 1, yend = 1,
			color = "Untenured", linetype = "Untenured"), linewidth = 0.8) +
		scale_color_manual(values = c("Tenured" = "violetred4", "Untenured" = "palevioletred3")) +
		scale_linetype_manual(values = c("Tenured" = "solid", "Untenured" = "dashed")) +
		scale_x_continuous(breaks = 1:26, labels = LETTERS) +
		scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
		labs(x = "First Letter of Surname", y = "Cumulative Share",
			color = NULL, linetype = NULL) +
		theme_classic(base_size = 14, base_family = latex_font) +
		theme(legend.position = c(0.2, 0.85))

	ggsave(paste0("../output/tenureCDF_econ_", fname, ".pdf"), tenureCDF_econ,
		width = 5, height = 4, device = quartz, type = "pdf")
	ggsave(file.path(overleaf, paste0("tenureCDF_econ_", fname, ".pdf")), tenureCDF_econ,
		width = 5, height = 4, device = quartz, type = "pdf")
}


##############################################################
# Table 1: LPM of tenure on surname initial, for economists
##############################################################
# Description of regression specification in README. 
econ_reg <- econdata

# American = 1 if nationality is USA; lots of blanks, counted as not American
econ_reg[, american := 0]
econ_reg[nationality == "USA", american := 1]

# Origin controls: the RAs' two name-origin classifications didn't fully agree, so the paper includes both sets (jewish, indian, otherasian and the "2" versions)
# For each department cutoff: one regression without controls, one with

# Ok, looped regs, w and w/o controls, for different rank groups, to recreate table 1
table1 <- data.table()
for (top in c(5, 10, 20, 35)) {

	# Restrict to departments ranked 1 to top for each group
	# Non-emeritus faculty only
	econ_top <- econ_reg[rank >= 1 & rank <= top & emeritus == 0]

	# Run and store regs
	reg_nocontrols <- lm(tenure ~ alph_order, data = econ_top)
	reg_controls   <- lm(tenure ~ alph_order + american +
		jewish + indian + otherasian + jewish2 + indian2 + otherasian2,
		data = econ_top)

	# Store relevant coeffs, and export regression table outputs as data to input in latex table
	for (controls in c(0, 1)) {
		if (controls == 0) reg <- reg_nocontrols
		if (controls == 1) reg <- reg_controls

		coefs <- summary(reg)$coefficients
		table1 <- rbind(table1, data.table(
			top        = top,
			controls   = controls,
			coef       = coefs["alph_order", "Estimate"],
			se         = coefs["alph_order", "Std. Error"],
			pval       = coefs["alph_order", "Pr(>|t|)"],
			amer_coef  = ifelse(controls == 1, coefs["american", "Estimate"], NA),
			amer_se    = ifelse(controls == 1, coefs["american", "Std. Error"], NA),
			amer_pval  = ifelse(controls == 1, coefs["american", "Pr(>|t|)"], NA),
			rsq        = summary(reg)$r.squared,
			n          = nobs(reg),
			n_tenured  = sum(econ_top$tenure)))
	}
}


## Format Table 1 like the paper

# Significance stars: *** 1 percent, ** 5 percent, * 10 percent
table1[, stars := ""]
table1[pval < 0.10, stars := "*"]
table1[pval < 0.05, stars := "**"]
table1[pval < 0.01, stars := "***"]

table1[, amer_stars := ""]
table1[amer_pval < 0.10, amer_stars := "*"]
table1[amer_pval < 0.05, amer_stars := "**"]
table1[amer_pval < 0.01, amer_stars := "***"]

# Cell text: 4 decimals for coefficients and SEs, SEs in parentheses, LaTeX minus signs
table1[, coef_txt := gsub("-", "$-$", paste0(sprintf("%.4f", coef), stars))]
table1[, se_txt := sprintf("(%.4f)", se)]

# American: emdash in the no-controls columns
table1[, amer_txt := "---"]
table1[controls == 1, amer_txt := gsub("-", "$-$", paste0(sprintf("%.4f", amer_coef), amer_stars))]
table1[, amer_se_txt := ""]
table1[controls == 1, amer_se_txt := sprintf("(%.4f)", amer_se)]

table1[, origin_txt := "No"]
table1[controls == 1, origin_txt := "Yes"]

# N and number tenured span both columns of a sample, so take them from the no-controls rows
table1_n <- table1[controls == 0]
table1_n[, n_txt := paste0("\\multicolumn{2}{c}{", format(n, big.mark = ",", trim = TRUE), "}")]
table1_n[, tenured_txt := paste0("\\multicolumn{2}{c}{", n_tenured,
	" (", sprintf("%.1f", 100 * n_tenured / n), "\\%)}")]

# Aaaaaaand output tex file with formatted table
table1_tex <- c(
	"\\begin{tabular}{l *{8}{c}}",
	"\\toprule",
	"& \\multicolumn{8}{c}{\\textit{DV: Tenure}} \\\\",
	" & \\multicolumn{2}{c}{Top 5 econ} & \\multicolumn{2}{c}{Top 10 econ} & \\multicolumn{2}{c}{Top 20 econ} & \\multicolumn{2}{c}{Top 35 econ} \\\\",
	"\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}",
	" & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\",
	"\\midrule",
	paste0("Last name initial & ", paste(table1$coef_txt, collapse = " & "), " \\\\"),
	paste0(" & ", paste(table1$se_txt, collapse = " & "), " \\\\"),
	paste0("American nationality & ", paste(table1$amer_txt, collapse = " & "), " \\\\"),
	paste0(" & ", paste(table1$amer_se_txt, collapse = " & "), " \\\\"),
	"\\midrule[0.25pt]",
	paste0("Six origin controls & ", paste(table1$origin_txt, collapse = " & "), " \\\\"),
	paste0("Number of tenured (\\%) & ", paste(table1_n$tenured_txt, collapse = " & "), " \\\\"),
	paste0("R$^2$ & ", paste(sprintf("%.4f", table1$rsq), collapse = " & "), " \\\\"),
	paste0("Number of obs. & ", paste(table1_n$n_txt, collapse = " & "), " \\\\"),
	"\\bottomrule",
	"\\end{tabular}"
	)

writeLines(table1_tex, "../output/table1_tenure_econ.tex")
writeLines(table1_tex, file.path(overleaf, "table1_tenure_econ.tex"))


#####################################################
# Figure 4: CDF tenure by initial, for psychologists
#####################################################

# Restrict to psych data
psychdata <- faculty[psych == 1]

# Rm people with missing tenure status. 
psychdata <- psychdata[!is.na(tenure)]

# Create untenured var for cdf 
psychdata[, untenured := NA_integer_]
psychdata[tenure == 0, untenured := 1]
psychdata[tenure == 1, untenured := 0]

# CDFs of tenure and untenured, for psychologists, by department rank group
for (top in c(5, 10, 20, 35)) {

	if (top == 35) {
		fname <- "alldept"
	} else {
		fname <- paste0("top", top)
	}

	# Restrict to departments ranked 1 to top
	psych_top <- psychdata[rank >= 1 & rank <= top]

	tenureCDF_psych <- ggplot() +
		stat_ecdf(data = psych_top[tenure == 1],
			aes(x = alph_order, color = "Tenured", linetype = "Tenured"),
			geom = "line", pad = FALSE, linewidth = 0.8) +
		stat_ecdf(data = psych_top[untenured == 1],
			aes(x = alph_order, color = "Untenured", linetype = "Untenured"),
			geom = "line", pad = FALSE, linewidth = 0.8) +
		# CDF hits 1 at the last observed initial; extend each line flat at 1 out to Z
		geom_segment(aes(x = max(psych_top[tenure == 1]$alph_order), xend = 26, y = 1, yend = 1,
			color = "Tenured", linetype = "Tenured"), linewidth = 0.8) +
		geom_segment(aes(x = max(psych_top[untenured == 1]$alph_order), xend = 26, y = 1, yend = 1,
			color = "Untenured", linetype = "Untenured"), linewidth = 0.8) +
		scale_color_manual(values = c("Tenured" = "violetred4", "Untenured" = "palevioletred3")) +
		scale_linetype_manual(values = c("Tenured" = "solid", "Untenured" = "dashed")) +
		scale_x_continuous(breaks = 1:26, labels = LETTERS) +
		scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
		labs(x = "First Letter of Surname", y = "Cumulative Share",
			color = NULL, linetype = NULL) +
		theme_classic(base_size = 14, base_family = latex_font) +
		theme(legend.position = c(0.2, 0.85))

	ggsave(paste0("../output/tenureCDF_psych_", fname, ".pdf"), tenureCDF_psych,
		width = 5, height = 4, device = quartz, type = "pdf")
	ggsave(file.path(overleaf, paste0("tenureCDF_psych_", fname, ".pdf")), tenureCDF_psych,
		width = 5, height = 4, device = quartz, type = "pdf")
}


#####################################################
# Table 4: LPM of tenure on surname initial, for psychologists
#####################################################

# Description of regression specification in README.

# No American control: nationality is blank for all psychologists

# Origin controls: the RAs' two name-origin classifications didn't fully agree, so the paper includes both sets (jewish, indian, otherasian and the "2" versions)
# For each department cutoff: one regression without controls, one with

# Ok, looped regs, w and w/o controls, for different rank groups, to recreate table 4
table4 <- data.table()
for (top in c(5, 10, 20, 35)) {

	# Restrict to departments ranked 1 to top for each group
	# Non-emeritus faculty only
	psych_top <- psychdata[rank >= 1 & rank <= top & emeritus == 0]

	# Run and store regs
	reg_nocontrols <- lm(tenure ~ alph_order, data = psych_top)
	reg_controls   <- lm(tenure ~ alph_order +
		jewish + indian + otherasian + jewish2 + indian2 + otherasian2,
		data = psych_top)

	# Store relevant coeffs, and export regression table outputs as data to input in latex table
	for (controls in c(0, 1)) {
		if (controls == 0) reg <- reg_nocontrols
		if (controls == 1) reg <- reg_controls

		coefs <- summary(reg)$coefficients
		table4 <- rbind(table4, data.table(
			top       = top,
			controls  = controls,
			coef      = coefs["alph_order", "Estimate"],
			se        = coefs["alph_order", "Std. Error"],
			pval      = coefs["alph_order", "Pr(>|t|)"],
			rsq       = summary(reg)$r.squared,
			n         = nobs(reg),
			n_tenured = sum(psych_top$tenure)))
	}
}


## Format Table 4 like the paper

# Significance stars: *** 1 percent, ** 5 percent, * 10 percent
table4[, stars := ""]
table4[pval < 0.10, stars := "*"]
table4[pval < 0.05, stars := "**"]
table4[pval < 0.01, stars := "***"]

# Cell text: 4 decimals for coefficients and SEs, SEs in parentheses, LaTeX minus signs
table4[, coef_txt := gsub("-", "$-$", paste0(sprintf("%.4f", coef), stars))]
table4[, se_txt := sprintf("(%.4f)", se)]

table4[, origin_txt := "No"]
table4[controls == 1, origin_txt := "Yes"]

# N and number tenured span both columns of a sample, so take them from the no-controls rows
table4_n <- table4[controls == 0]
table4_n[, n_txt := paste0("\\multicolumn{2}{c}{", format(n, big.mark = ",", trim = TRUE), "}")]
table4_n[, tenured_txt := paste0("\\multicolumn{2}{c}{", format(n_tenured, big.mark = ",", trim = TRUE),
	" (", sprintf("%.1f", 100 * n_tenured / n), "\\%)}")]

# Aaaaaaand output tex file with formatted table
table4_tex <- c(
	"\\begin{tabular}{l *{8}{c}}",
	"\\toprule",
	"& \\multicolumn{8}{c}{\\textit{DV: Tenure}} \\\\",
	" & \\multicolumn{2}{c}{Top 5 psych} & \\multicolumn{2}{c}{Top 10 psych} & \\multicolumn{2}{c}{Top 20 psych} & \\multicolumn{2}{c}{Top 35 psych} \\\\",
	"\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}",
	" & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\",
	"\\midrule",
	paste0("Last name initial & ", paste(table4$coef_txt, collapse = " & "), " \\\\"),
	paste0(" & ", paste(table4$se_txt, collapse = " & "), " \\\\"),
	"\\midrule[0.25pt]",
	paste0("Six origin controls & ", paste(table4$origin_txt, collapse = " & "), " \\\\"),
	paste0("Number of tenured (\\%) & ", paste(table4_n$tenured_txt, collapse = " & "), " \\\\"),
	paste0("R$^2$ & ", paste(sprintf("%.4f", table4$rsq), collapse = " & "), " \\\\"),
	paste0("Number of obs. & ", paste(table4_n$n_txt, collapse = " & "), " \\\\"),
	"\\bottomrule",
	"\\end{tabular}"
	)

writeLines(table4_tex, "../output/table4_tenure_psych.tex")
writeLines(table4_tex, file.path(overleaf, "table4_tenure_psych.tex"))

