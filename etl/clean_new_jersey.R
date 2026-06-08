# scrape PDF ----
books <- pdftools::pdf_text(here::here("data", "raw", "new_jersey", "NJDOC UNAUTHORIZED PUBLICATIONS OCTOBER 2025.pdf"))

# basic cleaning ----

# split character string of books by line break "\n"
books_split <- stringr::str_split(string = books, pattern = "\n")

# create dataframe from list of titles
books_df <- books_split |> 
  unlist() |> 
  dplyr::as_tibble()

# remove empty and header rows
books_df <- books_df |> 
  # remove empty rows
  filter(value != "") |>
  # remove first 5 rows
  tail(-5)

# clean titles and volumes/issues ----
books_df <-
  books_df |> 
  # separate volume/issue from title
  tidyr::separate_wider_delim(
    cols = value, 
    delim = "●", 
    names = c("title", "issues"),
    too_few = "debug", 
    too_many = "merge",
    cols_remove = TRUE) |>
  # remove debugging info from prior step
  dplyr::select(-c(value_ok, value_pieces, value_remainder)) |>
  # remove leading and trailing white spaces
  mutate(title = str_squish(title),
         issues = str_squish(issues)) |> 
  # replace string pattern " ● " with a comma
  dplyr::mutate(issues = str_replace_all(issues, pattern = " ● ", ", ")) |> 
  # remove messy title column
  select(-value)

# deal with variations of volume
books_df <-
  books_df |> 
  # separate everything before "Vol" from the rest
  tidyr::separate_wider_regex(
    cols = title,
    patterns = c(title_alt = ".+?(?=Vol)"),
    too_few = "debug"
  ) |>
  # define alternative column for issues
  rename(issues_alt = title_remainder) |>
  # replace with separated titles and volumes/issues
  mutate(title = if_else(!is.na(title_alt), title_alt, title),
         issues = if_else(!is.na(title_alt), issues_alt, issues)) |>
  # remove leading and trailing white spaces
  mutate(title = str_squish(title),
         issues = str_squish(issues)) |> 
  select(title, issues) |> 
  mutate(issues = str_replace_all(string = issues, 
                                  pattern = "Vol ", 
                                  replacement = "Vol. "))

# clean specific rows ----
manual_changes <-
  tibble::tribble(
    ~title, ~clean_title, ~date, ~author, ~rejection_reason,
    "Advanced Persistent Threat Hacking (re-added 9/21/2021)", "Advanced Persistent Threat Hacking", "9/21/2021", NA, NA,
    "The Satanic Bible (Author: Anton Szandor LaVey)", "The Satanic Bible", NA, "Anton Szandor LaVey", NA,
    "Witches Craft", "Witches Craft", NA, NA, "This version of Wiccan Bible is due to content"
  )

books_df <-
  books_df |> 
  # create placeholders for more information
  mutate(clean_title = title,
         author = NA_character_,
         date = NA_character_, 
         publication_type = NA_character_,
         rejection_reason = NA_character_) |> 
  # update specific rows with defined manual changes
  dplyr::rows_update(manual_changes, by = "title") |> 
  # clean issue column of specific row
  mutate(issues = if_else(title == "Witches Craft", NA, issues)) |> 
  # keep relevant columns
  select(title = clean_title, issues, author, date, publication_type, rejection_reason)


# export as CSV file
write_csv(x = books_df,
          file = here::here("data", "processed", "cleaned_new_jersey.csv"))
