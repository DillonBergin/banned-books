library(pdftools)
library(dplyr)
library(stringr)
library(readr)
library(tesseract)
library(here)
library(stringr)
library(rJava)
library(tabulapdf)


# Set the path to the PDF file and the output CSV file
pdf_path <- "data/raw/michigan/Restricted-Publication-List_1-12-2026.pdf"
out_path <- "data/processed/cleaned_michigan.csv"


tables <- extract_tables(pdf_path, pages = 1)


write_csv(cleaned_df, out_path)
