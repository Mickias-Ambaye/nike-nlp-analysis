# ==============================================================================
# 03 - TRUSTPILOT SCRAPER
# Brands: Nike, Adidas, Under Armour
# Scrapes up to 40 pages per brand (~800 reviews each)
# Rich reviews: pricing complaints, quality issues, sustainability mentions
# ==============================================================================

library(httr)
library(jsonlite)
library(tidyverse)
library(writexl)

# --- Save path ---
save_path <- "Dataset/"

# --- Trustpilot brand URLs ---
brands <- list(
  Nike             = "www.nike.com",
  Adidas           = "www.adidas.com",
  `Under Armour`   = "www.underarmour.com"
)

# --- Helper ---
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- Scraper function ---
scrape_trustpilot <- function(brand_slug, brand_name, max_pages = 40) {

  all_reviews <- list()

  for (page in 1:max_pages) {

    url <- paste0("https://www.trustpilot.com/review/", brand_slug, "?page=", page)
    cat("  ", brand_name, "- page", page, "\n")

    resp <- GET(url, add_headers(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
    ))

    if (status_code(resp) != 200) {
      cat("    Status", status_code(resp), "- stopping.\n")
      break
    }

    html <- content(resp, "text", encoding = "UTF-8")

    # Trustpilot embeds review data in __NEXT_DATA__ JSON
    json_match <- regmatches(html,
      regexpr('id="__NEXT_DATA__"[^>]*>([^<]+)', html, perl = TRUE))

    if (length(json_match) == 0) break

    json_str <- sub('id="__NEXT_DATA__"[^>]*>', '', json_match)
    parsed <- tryCatch(fromJSON(json_str, simplifyDataFrame = FALSE),
                       error = function(e) NULL)

    if (is.null(parsed)) break

    reviews_list <- tryCatch(parsed$props$pageProps$reviews, error = function(e) NULL)

    if (is.null(reviews_list) || length(reviews_list) == 0) break

    page_df <- map_dfr(reviews_list, function(r) {
      tibble(
        text   = paste(r$title %||% "", r$text %||% "", sep = ". "),
        rating = r$rating %||% NA_real_,
        date   = r$dates$publishedDate %||% NA_character_,
        source = "Trustpilot",
        brand  = brand_name
      )
    })

    all_reviews[[page]] <- page_df
    Sys.sleep(1.5)
  }

  bind_rows(all_reviews)
}

# --- Run for all brands ---
cat("=== Trustpilot Extraction Starting ===\n\n")

trustpilot_data <- map2_dfr(brands, names(brands), function(slug, name) {
  cat("\nScraping", name, "...\n")
  scrape_trustpilot(slug, name)
})

cat("\n=== Results ===\n")
print(table(trustpilot_data$brand))
cat("Total:", nrow(trustpilot_data), "\n")

# --- Save ---
write_xlsx(trustpilot_data, paste0(save_path, "trustpilot_reviews.xlsx"))
saveRDS(trustpilot_data, paste0(save_path, "trustpilot_reviews.rds"))
cat("Saved to Dataset/\n")
