# ==============================================================================
# 01 - APP STORE SCRAPER (Multi-Country)
# Brands: Nike, Adidas, Under Armour
# Uses Apple's free RSS/JSON API — no API key needed
# Loops through 8 English-speaking countries × 10 pages = up to 4,000 per brand
# ==============================================================================

library(jsonlite)
library(tidyverse)
library(writexl)

# --- Save path ---
save_path <- "Dataset/"

# --- App Store IDs ---
# Nike Run Club: 301521403 | Adidas: 1266591536 | Under Armour MapMyRun: 291890420
brands <- list(
  Nike            = "301521403",
  Adidas          = "1266591536",
  `Under Armour`  = "291890420"
)

# --- Multi-country scraper ---
scrape_app_store <- function(app_id, brand_name) {

  # 8 English-speaking regions for more coverage
  countries <- c("us", "gb", "ca", "au", "nz", "ie", "sg", "za")
  all_reviews <- list()

  for (country in countries) {
    for (page in 1:10) {

      url <- paste0("https://itunes.apple.com/", country,
                     "/rss/customerreviews/page=", page,
                     "/id=", app_id, "/sortby=mostrecent/json")

      cat("  ", brand_name, "-", toupper(country), "page", page, "\n")

      raw <- tryCatch(fromJSON(url, simplifyDataFrame = TRUE), error = function(e) NULL)

      if (!is.null(raw) && !is.null(raw$feed$entry)) {
        entries <- raw$feed$entry

        page_df <- tibble(
          text   = entries$content$label,
          rating = as.numeric(entries$`im:rating`$label),
          source = paste0("App Store (", toupper(country), ")"),
          brand  = brand_name
        ) %>% filter(!is.na(rating))

        all_reviews[[paste(country, page)]] <- page_df
      }

      Sys.sleep(0.5)
    }
  }

  bind_rows(all_reviews)
}

# --- Run for all brands ---
cat("=== App Store Extraction Starting ===\n")

app_data <- map2_dfr(brands, names(brands), function(id, name) {
  cat("\nScraping", name, "...\n")
  scrape_app_store(id, name)
})

cat("\n=== Results ===\n")
print(table(app_data$brand))
cat("Total:", nrow(app_data), "\n")

# --- Save ---
write_xlsx(app_data, paste0(save_path, "app_store_reviews.xlsx"))
saveRDS(app_data, paste0(save_path, "app_store_reviews.rds"))
cat("Saved to Dataset/\n")
