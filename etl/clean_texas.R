library(xml2)
library(purrr)
library(dplyr)

# Set the path to the PDF file and the output CSV file
pdf_path <- "data/raw/texas/DENIED_BOOKS_20250702.pdf"
out_path <- "data/processed/cleaned_texas.csv"

raw_xml <- read_xml("data/raw/texas/DENIED_BOOKS_20250702.xml")

# Extract root namespace
ns <- xml_ns_rename(xml_ns(raw_xml), d1 = "ss")

# Get rows from XML structure
rows <- xml_find_all(raw_xml, ".//ss:Row", ns)

# Set column names for df
col_names <- c("title", "author", "date", "unit_deny_reason", "drc_deny_reason")

df <- rows %>%
  map(~ xml_find_all(.x, ".//ss:Cell/ss:Data", ns) |> xml_text()) |>
  map(~ set_names(.x[1:5], col_names)) |>
  map_dfr(bind_rows) |>
  slice(-(1:2))

write_csv(df, out_path)
