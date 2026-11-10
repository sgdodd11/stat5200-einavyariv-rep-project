# Einav Yariv data download has .dta files; this script convers them to CSVs
# It also adds some re-encoding for problematic characters
# and cleans text to plain ASCII (for Scopus queries)

## ALL FILE PATHS IN SCRIPT ARE RELATIVE TO THIS SCRIPT'S FOLDER LOCATION (code/)

library(foreign)

in_dir <- "../data/125621-V1"
out_dir <- "../data/raw"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

# Columns holding person names: keep only letters, spaces, hyphens, apostrophes, periods
name_cols <- c("lname", "fname", "key",
               paste0("author", 1:5), paste0("keyauthor", 1:5))

clean_text <- function(x, name_col = FALSE) {
  # Non-breaking space (shows up as "¬†" or "Â " in some programs) -> regular space
  x <- gsub(" ", " ", x)
  # Smart quotes and dashes -> plain ASCII
  x <- gsub("[‘’`]", "'", x)
  x <- gsub("[“”]", "\"", x)
  x <- gsub("[–—]", "-", x)
  # Accented letters -> unaccented
  x <- chartr("áàâäãåéèêëíìîïóòôöõøúùûüñçýÿÁÀÂÄÃÅÉÈÊËÍÌÎÏÓÒÔÖÕØÚÙÛÜÑÇÝ",
              "aaaaaaeeeeiiiiooooooouuuuncyyAAAAAAEEEEIIIIOOOOOOUUUUNCY", x)
  x <- gsub("ß", "ss", x); x <- gsub("æ", "ae", x); x <- gsub("Æ", "AE", x)
  # Drop anything still outside printable ASCII
  x <- gsub("[^ -~]", "", x)
  if (name_col) x <- tolower(gsub("[^A-Za-z .'-]", "", x))
  # Collapse repeated spaces and trim ends
  trimws(gsub(" +", " ", x))
}

for (f in files) {
  df <- read.dta(f, convert.factors = TRUE)  # value labels -> text
  for (col in names(df)) {
    if (is.factor(df[[col]])) df[[col]] <- as.character(df[[col]])
    if (is.character(df[[col]])) {
      # Old Stata files store text as Windows-1252 (Latin-1 superset); re-encode to UTF-8
      df[[col]] <- iconv(df[[col]], from = "CP1252", to = "UTF-8")
      df[[col]] <- clean_text(df[[col]], name_col = col %in% name_cols)
    }
  }
  out <- file.path(out_dir, sub("\\.dta$", ".csv", basename(f)))
  write.csv(df, out, row.names = FALSE, na = "")
  cat(sprintf("%s -> %s (%d rows, %d cols)\n", basename(f), basename(out), nrow(df), ncol(df)))
}