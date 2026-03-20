# ==============================================================================
# 00 - DATA SUMMARY: Overview of master_clean dataset
# ==============================================================================

library(tidyverse)

data_path <- "Dataset/"
master <- readRDS(paste0(data_path, "master_clean.rds"))

cat("========================================\n")
cat("MASTER CLEAN DATASET OVERVIEW\n")
cat("========================================\n")
cat("Total rows:", nrow(master), "\n\n")

# --- By brand ---
cat("--- By Brand ---\n")
print(count(master, brand))

# --- By source ---
cat("\n--- By Source ---\n")
print(count(master, source))

# --- Brand x Source ---
cat("\n--- Brand x Source ---\n")
brand_source <- master %>%
  count(brand, source) %>%
  pivot_wider(names_from = source, values_from = n, values_fill = 0)
brand_source$TOTAL <- rowSums(select(brand_source, -brand))
print(brand_source)

# --- Source % per brand ---
cat("\n--- Source % per Brand ---\n")
pct <- master %>%
  count(brand, source) %>%
  group_by(brand) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = source, values_from = pct, values_fill = 0)
print(pct)

# --- Rating distribution (where available) ---
cat("\n--- Rating Distribution (App Store + Trustpilot only) ---\n")
print(master %>% filter(!is.na(rating)) %>% count(brand, rating) %>%
        pivot_wider(names_from = rating, values_from = n, values_fill = 0))

# --- Avg text length per brand ---
cat("\n--- Avg Text Length (characters) ---\n")
print(master %>% group_by(brand) %>% summarise(avg_chars = round(mean(nchar(text))),
                                                 min_chars = min(nchar(text)),
                                                 max_chars = max(nchar(text))))
