# ==============================================================================
# 04 - DATA CLEANING & MERGING
# Combines all 3 sources, cleans for NLP readiness
# Filters: English, relevant to brand, min length, no duplicates
# Outputs: master_clean.rds + master_clean.xlsx
# ==============================================================================

library(tidyverse)
library(writexl)
library(lexicon)  # install.packages("lexicon") if needed

# --- Paths ---
data_path <- "Dataset/"

# ==============================================================================
# STEP 1: Load all scraped data
# ==============================================================================
cat("Loading datasets...\n")

app_store   <- readRDS(paste0(data_path, "app_store_reviews.rds"))
reddit      <- readRDS(paste0(data_path, "reddit_reviews.rds"))
trustpilot  <- readRDS(paste0(data_path, "trustpilot_reviews.rds"))

cat("  App Store: ", nrow(app_store), "\n")
cat("  Reddit:    ", nrow(reddit), "\n")
cat("  Trustpilot:", nrow(trustpilot), "\n")

# ==============================================================================
# STEP 2: Standardize columns (text, source, brand, rating)
# ==============================================================================
cat("\nStandardizing columns...\n")

# App Store already has: text, rating, source, brand
app_clean <- app_store %>%
  select(text, source, brand, rating) %>%
  mutate(rating = as.numeric(rating))

# Reddit has: text, source, brand (no rating — that's fine)
reddit_clean <- reddit %>%
  select(text, source, brand) %>%
  mutate(rating = NA_real_)

# Trustpilot has: text, source, brand, rating
tp_clean <- trustpilot %>%
  select(text, source, brand, rating) %>%
  mutate(rating = as.numeric(rating))

# Combine
master_raw <- bind_rows(app_clean, reddit_clean, tp_clean)
cat("Combined raw total:", nrow(master_raw), "\n")

# ==============================================================================
# STEP 3: Cleaning filters
# ==============================================================================
cat("\nCleaning...\n")

master_clean <- master_raw %>%
  # Fix invalid UTF-8 characters FIRST (prevents downstream errors)
  mutate(text = iconv(text, from = "UTF-8", to = "UTF-8", sub = "")) %>%

  # Remove NAs and empty text
  filter(!is.na(text), text != "") %>%

  # Trim whitespace
  mutate(text = trimws(text)) %>%

  # Strip URLs from within text (keep the rest of the sentence)
  mutate(text = str_replace_all(text, "https?://\\S+", "")) %>%
  mutate(text = trimws(text)) %>%

  # Remove texts that ARE just a URL or link
  filter(!str_detect(text, "^https?://")) %>%
  filter(!str_detect(text, "^www\\.")) %>%

  # Remove gibberish: hex codes, random alphanumeric strings
  filter(!str_detect(text, "^[0-9a-f]{8,}$")) %>%           # hex strings like "4253423e32"
  filter(!str_detect(text, "^[0-9a-zA-Z._-]{1,10}$")) %>%   # short codes like "1s", "abc123"

  # Remove texts that are mostly numbers/symbols (not real language)
  filter(str_count(text, "[a-zA-Z]") / nchar(text) > 0.5) %>%

  # Remove very short texts (< 20 chars = not useful for NLP)
  filter(nchar(text) >= 20) %>%

  # Remove bot-like, deleted, or automated text
  filter(!str_detect(text, "(?i)^(deleted|removed|\\[removed\\]|\\[deleted\\]|\\[score hidden\\])$")) %>%
  filter(!str_detect(text, "(?i)^(i am a bot|this is an automated|beep boop)")) %>%

  # Remove markdown image/link-only posts
  filter(!str_detect(text, "^!?\\[.*\\]\\(.*\\)$")) %>%

  # Remove Reddit/website junk
  filter(!str_detect(text, "(?i)(imgur\\.com|reddit\\.com/gallery|gfycat|streamable|v\\.redd\\.it)")) %>%
  filter(!str_detect(text, "(?i)^(http|ftp|mailto)")) %>%
  filter(!str_detect(text, "(?i)(wiki|wikipedia\\.org)")) %>%

  # Remove duplicates (same text + brand)
  distinct(text, brand, .keep_all = TRUE) %>%

  # Only keep our 3 brands
  filter(brand %in% c("Nike", "Adidas", "Under Armour"))

cat("After junk removal:", nrow(master_clean), "\n")

# ==============================================================================
# STEP 3B: Remove profanity and slurs
# ==============================================================================
cat("Removing profanity and slurs...\n")

# Get profanity list and escape any regex special characters
profane_words <- profanity_alvarez
profane_words <- str_replace_all(profane_words, "([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1")

# Add common leetspeak / creative spelling variants the list might miss
extra_slurs <- c(
  "n[i1!][g9][g9][ae@][r]?[sz]?",   # n-word + variants of the N Word
  "n[i1!][g9]{2,}[ae@]?[r]?[sz]?",
  "f[a@][g9]{1,2}[o0]?[t]?[sz]?",   # f-slur variants
  "r[e3]t[a@]rd[s]?"                 # r-slur variants
)

# Combine
profane_words <- c(profane_words, extra_slurs)

# Process in small batches (avoids regex too-long errors)
batch_size <- 50
n_batches <- ceiling(length(profane_words) / batch_size)

for (i in 1:n_batches) {
  start <- (i - 1) * batch_size + 1
  end <- min(i * batch_size, length(profane_words))
  batch_pattern <- paste0("\\b(", paste(profane_words[start:end], collapse = "|"), ")\\b")

  master_clean <- master_clean %>%
    mutate(text = str_replace_all(text, regex(batch_pattern, ignore_case = TRUE), ""))
}

cat("  Processed", length(profane_words), "profane words in", n_batches, "batches\n")

# Clean up leftover whitespace and re-check length
master_clean <- master_clean %>%
  mutate(text = str_squish(text)) %>%
  filter(nchar(text) >= 20) %>%
  mutate(doc_id = paste0("DOC_", row_number()))

cat("After cleaning:", nrow(master_clean), "\n")

# ==============================================================================
# STEP 4: Report source balance
# ==============================================================================
cat("\n=== Source Balance ===\n")
balance <- master_clean %>%
  count(brand, source) %>%
  pivot_wider(names_from = source, values_from = n, values_fill = 0)
print(balance)

cat("\n=== Per Brand Totals ===\n")
print(count(master_clean, brand))

# ==============================================================================
# STEP 5: Save clean dataset
# ==============================================================================
saveRDS(master_clean, paste0(data_path, "master_clean.rds"))
write_xlsx(master_clean, paste0(data_path, "master_clean.xlsx"))
cat("\nSaved master_clean.rds and master_clean.xlsx to Dataset/\n")
cat("This is your NLP-ready dataset!\n")
