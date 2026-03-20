# ==============================================================================
# 05 - NLP READY: Load Clean Data + All Libraries
# Run this at the start of any NLP analysis script
# ==============================================================================

# --- Libraries ---
library(tidyverse)      # data wrangling
library(tidytext)       # tokenization, sentiment, tf-idf
library(textdata)       # NRC emotion lexicon, AFINN
library(topicmodels)    # LDA topic modeling
library(SnowballC)      # word stemming
library(igraph)         # network graphs
library(ggraph)         # visualize networks
library(writexl)        # save to Excel

# --- Load the clean master dataset ---
data_path <- "Dataset/"
master <- readRDS(paste0(data_path, "master_clean.rds"))

# --- Quick summary ---
cat("=== NLP Dataset Loaded ===\n")
cat("Total documents:", nrow(master), "\n\n")

cat("Per brand:\n")
print(count(master, brand))

cat("\nPer source:\n")
print(count(master, source))

cat("\nBrand x Source:\n")
print(master %>% count(brand, source) %>% pivot_wider(names_from = source, values_from = n, values_fill = 0))

# --- Columns available ---
# master$doc_id  : unique ID
# master$text    : the review/comment text (cleaned)
# master$source  : "App Store (US)", "Reddit", "Trustpilot", etc.
# master$brand   : "Nike", "Adidas", or "Puma"
# master$rating  : 1-5 for App Store/Trustpilot, NA for Reddit

cat("\n=== Ready for NLP analysis! ===\n")
cat("Your data is in the 'master' dataframe.\n")
