library(pdftools)
library(dplyr)
library(stringr)
library(readr)
library(tesseract)
library(here)
library(stringr)

# Set the path to the PDF file and the output CSV file
pdf_path <- "data/raw/kansas/KORA-_Bergin.pdf"
out_path <- "data/processed/cleaned_kansas.csv"

# OCR the PDF file to extract text
pdf_convert(
    pdf_path,
    format = "png",
    dpi = 300,
    filenames = file.path(
        "data/raw/kansas/ocr_images",
        paste0("page_", 1:pdf_info(pdf_path)$pages, ".png")
    )
)
# Get text from OCR-d files
page_files <- list.files(
    "data/raw/kansas/ocr_images",
    pattern = "\\.png$",
    full.names = TRUE
)
# Pages 31-60 look to be reasons they books were banned, but we only want the first 30 pages for now
# Maybe I will request a better formatted dataset later, pages 31-60 not worth it for now
page_nums <- as.numeric(gsub(".*page_(\\d+)\\.png$", "\\1", page_files))
page_files <- page_files[page_nums >= 1 & page_nums <= 30]

text <- sapply(page_files, ocr)

# Turn text into dataframe
titles <- unlist(strsplit(text, "\n"))

df <- data.frame(
    title = unname(titles),
    issues = NA,
    author = NA,
    date = NA,
    publication_type = NA,
    rejection_reason = NA,
    stringsAsFactors = FALSE
)


cleaned_df <- df %>%
    mutate(
        title = str_remove(title, "^[\\|:\\s]+"),
        title = str_trim(title)
    ) |>
    filter(title != "") |>
    filter(title != "Title")


write_csv(cleaned_df, out_path)
