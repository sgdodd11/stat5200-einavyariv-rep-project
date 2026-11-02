# Convert every .dta file in "replication download" to a CSV alongside it.
# Usage: Rscript dta_to_csv.R   (run from the project root)

library(foreign)  # ships with R; reads older Stata formats (these files are Stata 7)

in_dir <- "replication download"
files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

for (f in files) {
  df <- read.dta(f, convert.factors = TRUE)  # value labels -> text
  out <- sub("\\.dta$", ".csv", f)
  write.csv(df, out, row.names = FALSE, na = "")
  cat(sprintf("%s -> %s (%d rows, %d cols)\n", basename(f), basename(out), nrow(df), ncol(df)))
}
