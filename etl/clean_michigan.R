library(pdftools)
library(dplyr)
library(stringr)
library(readr)

# Set the path to the PDF file and the output CSV file
out_path <- "data/processed/cleaned_michigan.csv"


col_names <- c(
    "title",
    "author",
    "publication_type",
    "edition_isbn",
    "date",
    "rejection_codes",
    "rejection_reason"
)

df <- read_csv(
    "data/raw/michigan/extracted_tables.csv",
    col_names = col_names,
    skip = 1
) |>
    filter(!title %in% c("Title", "Page Number:"))


write_csv(cleaned_df, out_path)
