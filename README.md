# Nike NLP Simulation

**Course:** Business Analysis for Unstructured Data 
**Assignment:** A2: Team Work — Nike NLP Simulation
**Due:** March 24, 2026

## Overview

NLP analysis of Nike, Adidas, and Under Armour brand perception using consumer reviews scraped from three platforms. We play the role of a Data & AI Consulting Team delivering findings to Nike's CMO and VP of Consumer Insights.

The guiding question: *"What are customers really saying about Nike — and how should we respond strategically?"*

## Business Questions

| # | Question | Method |
|---|----------|--------|
| Q1 | **Brand Perception** — How is Nike emotionally positioned online? | NRC emotion lexicon, LDA topic modeling, bigrams/trigrams |
| Q2 | **Pricing Sensitivity** — Are customers describing Nike as overpriced? | Aspect-based sentiment on price-related sentences |
| Q3 | **Competitive Positioning** — What differentiates Nike from Adidas in consumer language? | TF-IDF, pairwise word correlations, co-occurrence networks |
| Q4 | **Sustainability Narrative** — Is Nike's sustainability messaging resonating? | Keyword filtering + sentiment scoring on sustainability sentences |

## Data Sources

| Source | Method | Brands |
|--------|--------|--------|
| Apple App Store | RSS/JSON feed, 8 countries × 10 pages | Nike (301521403), Adidas (1266591536), Under Armour (291890420) |
| Reddit | Native JSON API (`reddit.com/search.json`), 10 search terms per brand | Nike, Adidas, Under Armour |
| Trustpilot | `__NEXT_DATA__` JSON parsing, 40 pages per brand | nike.com, adidas.com, underarmour.com |

Final cleaned dataset: **~16K documents** across all three brands and sources.

## Project Structure

```
├── 00_data_summary.R          # Quick summary of master_clean.rds
├── 01_scrape_app_store.R      # App Store scraper (multi-country)
├── 02_scrape_reddit.R         # Reddit JSON API scraper
├── 03_scrape_trustpilot.R     # Trustpilot scraper
├── 04_clean_data.R            # Merge, clean, deduplicate, profanity filter
├── 05_nlp_analysis.R          # Full NLP pipeline (Sections 1–7)
├── 05_nlp_analysis.Rmd        # R Markdown version of the NLP analysis
├── Dataset/
│   ├── app_store_reviews.rds
│   ├── reddit_reviews.rds
│   ├── trustpilot_reviews.rds
│   ├── master_clean.rds       # Final clean dataset
│   └── master_clean.xlsx
└── README.md
```

## Pipeline

**Scraping (Scripts 01–03)** → Raw reviews with text, brand, source, rating
**Cleaning (Script 04)** → UTF-8 fix, URL removal, gibberish filter, 20-char minimum, English filter, bot removal, deduplication, profanity removal (lexicon::profanity_alvarez + leetspeak variants)
**Analysis (Script 05)** → Tokenization, stopwords, stemming, n-grams, DTM, sentiment (NRC/AFINN/Bing), LDA, TF-IDF, pairwise correlations, networks, aspect-based sentiment, word frequency correlations

## NLP Techniques Used

**Mandatory:** Tokenization, stopword removal, stemming, n-grams (bigrams + trigrams), DTM, sentiment analysis (3 lexicons), LDA topic modeling

**Additional:** TF-IDF for brand differentiation, pairwise word correlations (widyr), co-occurrence network graphs (igraph + ggraph), aspect-based sentiment (price, quality, sustainability), Zipf's Law validation, word frequency correlograms

## Dependencies

```r
install.packages(c("tidyverse", "tidytext", "topicmodels", "SnowballC",
                    "igraph", "ggraph", "widyr", "scales", "jsonlite",
                    "writexl", "lexicon"))
```

## How to Run

1. Run scripts `01` through `03` to scrape data (saves to `Dataset/`)
2. Run `04_clean_data.R` to merge and clean everything into `master_clean.rds`
3. Run `05_nlp_analysis.R` or knit `05_nlp_analysis.Rmd` for the full analysis
