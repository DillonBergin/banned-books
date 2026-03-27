library(pdftools)
library(dplyr)
library(stringr)
library(readr)

pdf_path <- "data/raw/georgia/PRC_List_2025.pdf"
out_path <- "data/processed/cleaned_georgia.csv"

year_pat <- "^(20[0-9]{2}|201[0-9])$"

# ---------------------------------------------------------------------------
# 1. Read and normalize lines
# ---------------------------------------------------------------------------
raw_lines <- pdf_text(pdf_path) |>
  paste(collapse = "\n") |>
  str_split("\n") |>
  unlist()

# Classify each line by its leading marker (before trimming)
line_type <- case_when(
  str_detect(raw_lines, "^\\s{0,4}•")  ~ "bullet",
  str_detect(raw_lines, "^\\s+o\\s")   ~ "sub_bullet",
  TRUE                                  ~ "other"
)

lines <- str_trim(raw_lines)

# ---------------------------------------------------------------------------
# 2. Join wrapped continuation lines to their parent
#    A continuation line is non-empty "other" that follows a bullet/sub_bullet
# ---------------------------------------------------------------------------
joined      <- character(length(lines))
joined_type <- character(length(lines))
skip        <- logical(length(lines))

for (i in seq_along(lines)) {
  if (skip[i]) next
  joined[i]      <- lines[i]
  joined_type[i] <- line_type[i]

  if (line_type[i] %in% c("bullet", "sub_bullet")) {
    j <- i + 1
    while (j <= length(lines) &&
           line_type[j] == "other" &&
           nchar(lines[j]) > 0) {
      # Don't absorb a bare year header unless the current line ends mid-sentence
      # (trailing comma = wrapped continuation; no comma = new section header)
      is_year_line      <- str_detect(lines[j], year_pat)
      prev_incomplete   <- str_detect(str_trim(joined[i]), ",$")
      if (is_year_line && !prev_incomplete) break
      joined[i] <- paste(joined[i], lines[j])
      skip[j]   <- TRUE
      j         <- j + 1
    }
  }
}

# Drop skipped and empty lines
keep <- !skip & nchar(lines) > 0
joined      <- joined[keep]
joined_type <- joined_type[keep]

# ---------------------------------------------------------------------------
# 3. Walk through lines, tracking ban year and series context
# ---------------------------------------------------------------------------
records       <- list()
ban_year      <- NA_character_
series_author <- NA_character_   # author inherited by sub-bullets
series_title  <- NA_character_   # series name prepended to sub-bullet titles

# Patterns
# "Series" header if entry contains "Series" AND no publication date follows
series_pat  <- "(?i)\\bseries\\b"

# Author extraction: "by X" or "author X" (at end of string, possibly with date after)
MONTHS <- "January|February|March|April|May|June|July|August|September|October|November|December"
# Date suffix at end of entry: "Month [Day,] Year" or "MM.DD.YYYY"
date_suffix_pat <- paste0(
  ",?\\s*(?:",
  "(?:", MONTHS, ")(?:/(?:", MONTHS, "))?(?:\\s+\\d{1,2}(?:-\\d{1,2})?)?[,\\s]+\\d{4}",
  "|\\d{1,2}\\.\\d{1,2}\\.\\d{4}",
  "|[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}",
  "|\\d{4}\\s*$",
  ").*$"
)

# Known magazine/periodical keywords
magazine_names <- paste(
  c("Allure", "Cosmopolitan", "Elle", "Esquire", "GQ", "Glamour", "Maxim",
    "Vogue", "Rolling Stone", "Sports Illustrated", "National Geographic",
    "Men's Health", "Men's Fitness", "Marie Clair", "Harper", "Inked",
    "Tattoo", "Revolver", "Wired", "Time", "Newsweek", "People", "Us Weekly",
    "Star", "OK!", "OK Magazine", "New Yorker", "New York", "Ebony",
    "Popular Mechanics", "Popular Science", "Smithsonian", "Scientific American",
    "Psychology Today", "Economist", "Teen Vogue", "Latina", "Nylon",
    "FLEX", "Flex", "Hot Bike", "Outdoor Life", "Boat", "Surfer", "Surfing",
    "ESPN", "W,", "ELLE", "BAZAAR", "Black and Pink", "Earth First",
    "Fifth Estate", "Under Lock & Key", "2600",
    "GON,", "Georgia Outdoor News", "Men's Journal", "Predator Extreme",
    "Drawing,", "Survival", "Release,", "Bay View", "Hip Hop Weekly",
    "Skin and Ink", "Skin And Ink", "Super Street",
    "TVyNovelas", "CODE ", "Fangora", "W Magazine",
    "Black Men", "Tools for Freedom", "Artist Magazine", "Artists Magazine"),
  collapse = "|"
)
magazine_pat <- regex(magazine_names, ignore_case = TRUE)

is_periodical <- function(text) {
  str_detect(text, magazine_pat) |
  str_detect(text, "(?i)\\b(issue|vol\\.|no\\.|newsletter|flyer|magazine|quarterly)\\b") |
  str_detect(text, "(?i)\\bvolume\\s+\\d+.*(?:issue|no\\.|#)") |  # "Volume X, Issue Y" = periodical
  (str_detect(text, "(?i)(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{4}$") &
   !str_detect(text, "(?i)published\\s+(January|February|March|April|May|June|July|August|September|October|November|December)")) |
  str_detect(text, "#\\d+")
}

strip_date_suffix <- function(text) {
  str_remove(text, regex(date_suffix_pat, ignore_case = TRUE))
}

# Returns list(title, author) parsed together to avoid ambiguity
parse_entry <- function(text, keep_date_in_title = FALSE) {
  t <- str_remove(text, "^[•o]\\s*")

  # --- Author extraction helpers ---

  clean_author <- function(a) {
    a <- str_trim(a)
    a <- str_remove(a, regex("[,.]?\\s*published.*$", ignore_case = TRUE))
    a <- str_remove(a, paste0(",?\\s*(?:", MONTHS, ").*$"))
    a <- str_remove(a, '"$|^"')
    # keep (pen name) and similar parenthetical qualifiers
    a <- str_remove(a, "[,\\.]+$")
    str_trim(a)
  }

  author      <- NA_character_
  author_via_by <- FALSE  # TRUE when author found via explicit "by" attribution

  # 0. Possessive: "Author's Title" — any apostrophe variant
  m_poss <- str_match(t, "^([A-Z][a-z]+(?: [A-Z][a-z]+)*)['\u2019\u02bc]s\\b")
  if (!is.na(m_poss[1, 2])) author <- clean_author(m_poss[1, 2])

  # 1a. Explicit ", by Author" — reliable: "by" follows a comma
  if (is.na(author)) {
    m_comby <- str_match(t, regex(
      paste0(",\\s*by\\s+(.+?)(?:\\s*,?\\s*(?:published\\b|", MONTHS, "\\s+\\d).*)?$"),
      ignore_case = TRUE
    ))
    if (!is.na(m_comby[1, 2])) { author <- clean_author(m_comby[1, 2]); author_via_by <- TRUE }
  }

  # 1b. Bare "by" in text with lookahead to date — only when date is present
  #     AND what's between "by" and the date has no intermediate comma (avoids
  #     "by Trouble, Eric Jerome Dickey" grabbing too much)
  if (is.na(author)) {
    m_by <- str_match(t, regex(
      paste0("\\bby\\s+([^,]+?)(?=\\s*,?\\s*(?:published\\b|", MONTHS, ")\\s+\\d)"),
      ignore_case = TRUE
    ))
    if (!is.na(m_by[1, 2])) {
      candidate <- str_trim(m_by[1, 2])
      # Reject single lowercase-able words (prepositions inside titles)
      if (str_detect(candidate, "\\s") || str_detect(candidate, "^[A-Z][a-z]+\\s+[A-Z]")) {
        author <- clean_author(candidate); author_via_by <- TRUE
      }
    }
  }

  # 1c. "by Name1, Name2" followed by date — comma-separated co-authors
  #     e.g. "by Mandii B, WeezyWTF, June 24, 2025"
  #     First segment must be 2+ words to avoid "by Trouble, Eric..." misparse
  if (is.na(author)) {
    m_coauth <- str_match(t, regex(
      paste0("\\bby\\s+(.+?)(?=\\s*,\\s*(?:", MONTHS, ")\\s+\\d)"),
      ignore_case = TRUE
    ))
    if (!is.na(m_coauth[1, 2])) {
      candidate <- str_trim(m_coauth[1, 2])
      segs      <- str_split(candidate, ",\\s*")[[1]]
      first_seg_words <- length(str_split(str_trim(segs[1]), "\\s+")[[1]])
      # Accept only if: first segment is 2+ words AND all segments start with capital
      if (first_seg_words >= 2 && all(str_detect(str_trim(segs), "^[A-Z]"))) {
        author <- clean_author(candidate); author_via_by <- TRUE
      }
    }
  }

  # 1d. Explicit ", author Name" keyword (e.g. "Title, author Ronald Dalton, Jr. Published …")
  if (is.na(author)) {
    m_auth_kw <- str_match(t, regex(",\\s*author\\s+(.+)$", ignore_case = TRUE))
    if (!is.na(m_auth_kw[1, 2])) { author <- clean_author(m_auth_kw[1, 2]); author_via_by <- TRUE }
  }

  # 2. ": Author Published Date" — if author portion contains " by ", use what follows "by"
  if (is.na(author)) {
    m_colon <- str_match(t, regex(":\\s*([A-Z][^:]+?)\\s+[Pp]ublished\\b"))
    if (!is.na(m_colon[1, 2])) {
      a <- str_trim(m_colon[1, 2])
      # If "by X" is inside the matched text, take just X
      inner_by <- str_match(a, regex("\\bby\\s+(.+)$", ignore_case = TRUE))
      if (!is.na(inner_by[1, 2])) a <- str_trim(inner_by[1, 2])
      author <- clean_author(a); author_via_by <- TRUE
    }
  }

  # 3. "Title, Author, Month Day, Year" — last two segments are a split date
  if (is.na(author)) {
    parts <- str_split(t, ",\\s*")[[1]]
    n <- length(parts)
    if (n >= 4) {
      last      <- str_trim(parts[n])
      penult    <- str_trim(parts[n - 1])
      antepenult <- str_trim(parts[n - 2])
      # last = year, penult = "Month Day" → combined date
      if (str_detect(last, "^\\d{4}$") &&
          str_detect(penult, regex(paste0("^(?:", MONTHS, ")"), ignore_case = TRUE)) &&
          str_detect(antepenult, "^[A-Z]") && !str_detect(antepenult, "\\d")) {
        author <- clean_author(antepenult)
      }
    }
  }

  # 4. "Title, Author [Month Day], Year" — strip inline date from mid segment first
  if (is.na(author)) {
    parts <- str_split(t, ",\\s*")[[1]]
    n <- length(parts)
    if (n >= 3) {
      last <- str_trim(parts[n])
      mid  <- str_trim(parts[n - 1])
      # Strip trailing "Month [Day]" from mid (e.g. "R.I. Richardsun (pen name) October 8")
      mid_clean <- str_trim(str_remove(mid, regex(paste0("\\s+(?:", MONTHS, ").*$"), ignore_case = TRUE)))
      if (str_detect(last, regex(paste0("^(?:", MONTHS, "|\\d{4})"), ignore_case = TRUE)) &&
          str_detect(mid_clean, "^[A-Z]") &&
          !str_detect(mid_clean, "\\d") &&
          length(str_split(mid_clean, "\\s+")[[1]]) <= 5) {
        author <- clean_author(mid_clean)
      }
    }
  }

  # 5. "Title, SingleWord" — single proper-name word at end, no date
  if (is.na(author)) {
    m_single <- str_match(t, ",\\s*([A-Z][a-z]+\\.?)\\s*$")
    if (!is.na(m_single[1, 2])) author <- clean_author(m_single[1, 2])
  }

  # 6. Last resort: "by Author" at very end of string (no date) — covers series headers
  if (is.na(author)) {
    m_end <- str_match(t, regex("\\bby\\s+(.{3,})\\s*$", ignore_case = TRUE))
    if (!is.na(m_end[1, 2])) author <- clean_author(m_end[1, 2])
  }

  # --- Title extraction ---
  title <- t

  # Possessive format: strip leading "Author's " to leave just the title
  title <- str_remove(title, regex("^[A-Z][a-z]+(?: [A-Z][a-z]+)*['\u2019\u02bc]s\\s+"))

  # Strip quotes only when they form a balanced pair wrapping the ENTIRE title
  # (avoids stripping subtitle quotes like Sports Illustrated, "In the Paint")
  starts_quote <- str_detect(title, '^["\u201c]')
  ends_quote   <- str_detect(title, '["\u201d]$')
  if (starts_quote && ends_quote) {
    title <- str_sub(title, 2, -2)
  } else if (starts_quote) {
    # Unbalanced leading quote: strip it + any matching close quote before a dash
    title <- str_remove(title, '^["\u201c]')
    title <- str_remove(title, '["\u201d](?=-)')
  }

  # Remove ": [by] Author published Date" — but ONLY when the colon introduces an author,
  # not a subtitle. Subtitle signals: starts with article, has prep words ("of","the"), or has comma.
  colon_m <- str_match(title, regex(":\\s*(.*?)\\s+[Pp]ublished\\b", ignore_case = TRUE))
  if (!is.na(colon_m[1, 2])) {
    cc <- str_trim(colon_m[1, 2])
    cc_no_by <- str_trim(str_remove(cc, regex("^by\\s+", ignore_case = TRUE)))
    is_subtitle <- str_detect(cc_no_by, regex("^(the|a|an)\\s", ignore_case = TRUE)) ||
                   str_detect(cc_no_by, regex("\\b(of|the|for)\\b", ignore_case = TRUE)) ||
                   str_detect(cc_no_by, ",")
    explicit_by <- str_detect(cc, regex("^by\\s+", ignore_case = TRUE))
    if (explicit_by || !is_subtitle) {
      title <- str_remove(title, regex(":\\s*(?:by\\s+)?[A-Za-z].+?\\s+[Pp]ublished\\b.*$",
                                       ignore_case = TRUE))
    }
  }

  # Remove trailing ", by Author [Date]" or "– by Author [Date]"
  title <- str_remove(title, regex("[,–-]\\s*(?:by|author)\\s+.+$", ignore_case = TRUE))

  # Remove "by Author [published|,rest]" — handles & connectors ("Ashley & JaQuavis"),
  # multi-word names, and initials. Requires 2+ name tokens to avoid stripping "by Trouble".
  title <- str_remove(title, regex(paste0(
    "\\s+by\\s+",
    # Option A: multi-token name (2+ words, may include "& Name" or "and Name")
    "(?:[A-Z][A-Za-z.]+(?:\\s+(?:&\\s*[A-Za-z.]*|[Aa]nd\\s+[A-Z][A-Za-z.]+|[A-Z][A-Za-z.]*))+",
    "|",
    # Option B: any name immediately adjacent to "published" (single word OK)
    "[A-Z][A-Za-z.&]+(?:\\s+[A-Z&][A-Za-z.]*)*(?=\\s*,?\\s*[Pp]ublished\\b))",
    "(?:\\s*(?:[Pp]ublished\\b|,).*)?$"
  )))

  # Remove trailing date suffix for books; periodicals keep it as part of the title
  if (!keep_date_in_title) title <- strip_date_suffix(title)
  title <- str_trim(title)
  title <- str_remove(title, "[\\s;,/-]+$")

  # Remove trailing ", Author" only when author wasn't already found via "by" attribution
  # (avoids stripping subtitles like "Revelations" after "by Author" was already stripped)
  if (!author_via_by) {
    title <- str_remove(title, regex(
      ",\\s*[A-Z][A-Za-z.]+(?:\\s+[A-Za-z.()]+){0,4}(?:\\s*\\([^)]+\\))?$"
    ))
  }

  # Remove stray trailing comma or punctuation
  title <- str_remove(title, "[,\"]+$")

  list(title = title, author = author)
}

extract_author <- function(text) parse_entry(text)$author

is_series_header <- function(text) {
  str_detect(text, regex(series_pat)) &&
    !str_detect(text, regex(paste0("(?:", MONTHS, ")\\s+\\d{1,2},?\\s*\\d{4}"), ignore_case = TRUE))
}

for (i in seq_along(joined)) {
  txt  <- joined[i]
  typ  <- joined_type[i]
  clean <- str_remove(txt, "^[•o]\\s*")

  # Year header
  if (str_detect(clean, year_pat)) {
    ban_year      <- clean
    series_author <- NA_character_
    series_title  <- NA_character_
    next
  }

  # Skip header / blank lines
  if (typ == "other" && !str_detect(clean, "^[•o]")) next

  # Series header bullet — remember author + title for sub-items, don't emit a row
  if (typ == "bullet" && is_series_header(clean)) {
    series_author <- extract_author(clean)
    if (is.na(series_author)) {
      m_colon <- str_match(clean, ":\\s*(.+?)\\s*$")
      if (!is.na(m_colon[1, 2])) series_author <- str_trim(m_colon[1, 2])
    }
    if (is.na(series_author) || nchar(series_author) == 0) series_author <- NA_character_
    # Series title: everything before ":" or before "by"
    series_title <- str_trim(str_remove(clean, regex("(?::\\s*.*|\\s+by\\s+.*)$", ignore_case = TRUE)))
    if (is.na(series_title) || nchar(series_title) == 0) series_title <- NA_character_
    next
  }

  # Reset series context when we hit a top-level bullet
  if (typ == "bullet") {
    series_author <- NA_character_
    series_title  <- NA_character_
  }

  # Parse title + author together
  is_per  <- is_periodical(clean)
  parsed  <- parse_entry(clean, keep_date_in_title = is_per)
  # Sub-bullets belong to a series — always use series_author when available
  author <- if (typ == "sub_bullet" && !is.na(series_author)) series_author else parsed$author
  # Prepend series name to sub-bullet titles
  title <- if (typ == "sub_bullet" && !is.na(series_title)) {
    paste0(series_title, ": ", parsed$title)
  } else {
    parsed$title
  }

  records[[length(records) + 1]] <- tibble(
    title            = title,
    author           = author,
    date             = if (!is.na(ban_year)) as.integer(ban_year) else NA_integer_,
    publication_type = if (is_per) "periodical" else "book",
    rejection_reason = NA_character_
  )
}

# ---------------------------------------------------------------------------
# 4. Assemble and write
# ---------------------------------------------------------------------------
result <- bind_rows(records) |>
  mutate(
    title  = str_squish(title),
    author = str_squish(author),
    date   = as.character(date)
  ) |>
  filter(nchar(title) > 1)

write_csv(result, out_path, na = "")
message("Wrote ", nrow(result), " rows to ", out_path)
