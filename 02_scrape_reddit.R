# ==============================================================================
# 02 - REDDIT SCRAPER
# Brands: Nike, Adidas, Under Armour
# Uses Reddit's native JSON API (no Google dependency, no rate limit issues)
# ==============================================================================

library(jsonlite)
library(httr)
library(tidyverse)
library(writexl)

# --- Save path ---
save_path <- "Dataset/"

# --- Search queries per brand (brand-specific only) ---
search_queries <- list(
  Nike           = c("Nike", "Nike quality", "Nike price", "Nike overpriced",
                      "Nike sustainable", "Nike shoes review", "Nike running shoes",
                      "Nike sneakers", "Nike customer service", "Nike durability"),
  Adidas         = c("Adidas", "Adidas quality", "Adidas price", "Adidas overpriced",
                      "Adidas sustainable", "Adidas shoes review", "Adidas running shoes",
                      "Adidas sneakers", "Adidas customer service", "Adidas durability"),
  `Under Armour` = c("Under Armour", "UnderArmour", "UA shoes", "Under Armour quality",
                      "Under Armour price", "Under Armour review", "Under Armour running",
                      "Under Armour sneakers", "Under Armour customer service",
                      "Under Armour durability")
)

# --- Function: search Reddit's JSON API directly ---
reddit_search <- function(query, limit = 100, after = NULL) {

  url <- paste0("https://www.reddit.com/search.json?q=", URLencode(query),
                "&sort=relevance&limit=", limit, "&type=link")
  if (!is.null(after)) url <- paste0(url, "&after=", after)

  resp <- GET(url, user_agent("R scraper for academic project"))

  if (status_code(resp) == 429) {
    cat("    Rate limited, waiting 10s...\n")
    Sys.sleep(10)
    resp <- GET(url, user_agent("R scraper for academic project"))
  }

  if (status_code(resp) != 200) return(NULL)

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
  return(parsed$data)
}

# --- Function: get comments from a single post ---
get_post_comments <- function(permalink) {

  url <- paste0("https://www.reddit.com", permalink, ".json?limit=100")
  resp <- GET(url, user_agent("R scraper for academic project"))

  if (status_code(resp) != 200) return(character(0))

  parsed <- tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || length(parsed) < 2) return(character(0))

  # Comments are in the 2nd element
  comments_data <- parsed[[2]]$data$children
  comments <- sapply(comments_data, function(c) {
    if (c$kind == "t1") c$data$body else NA
  })

  comments[!is.na(comments)]
}

# --- Main scraper for one brand ---
scrape_brand <- function(brand_name, queries) {

  all_data <- list()
  counter <- 0

  for (q in queries) {
    cat("  Searching '", q, "'", sep = "")

    # Page 1
    result <- reddit_search(q, limit = 100)
    if (is.null(result) || length(result$children) == 0) {
      cat(" - nothing\n")
      Sys.sleep(2)
      next
    }

    posts <- result$children
    cat(" -", length(posts), "posts")

    # Also grab page 2 if available
    if (!is.null(result$after)) {
      Sys.sleep(2)
      page2 <- reddit_search(q, limit = 100, after = result$after)
      if (!is.null(page2) && length(page2$children) > 0) {
        posts <- c(posts, page2$children)
        cat(" +", length(page2$children), "more")
      }
    }

    # Extract post titles + selftext
    for (p in posts) {
      d <- p$data
      # Post title
      if (!is.null(d$title) && nchar(d$title) > 5) {
        counter <- counter + 1
        all_data[[counter]] <- tibble(
          text = d$title, source = "Reddit", brand = brand_name, type = "post_title"
        )
      }
      # Post body (selftext)
      if (!is.null(d$selftext) && nchar(d$selftext) > 10) {
        counter <- counter + 1
        all_data[[counter]] <- tibble(
          text = d$selftext, source = "Reddit", brand = brand_name, type = "post_body"
        )
      }
    }

    # Pull comments from top 5 posts per query (most commented ones)
    scored_posts <- posts[order(sapply(posts, function(p) -p$data$num_comments))]
    top_posts <- head(scored_posts, 5)

    for (p in top_posts) {
      permalink <- p$data$permalink
      if (is.null(permalink)) next

      comments <- tryCatch(get_post_comments(permalink), error = function(e) character(0))

      for (cmt in comments) {
        if (!is.null(cmt) && nchar(cmt) > 10) {
          counter <- counter + 1
          all_data[[counter]] <- tibble(
            text = cmt, source = "Reddit", brand = brand_name, type = "comment"
          )
        }
      }
      Sys.sleep(1)
    }

    cat(" => collected", counter, "total\n")
    Sys.sleep(2)
  }

  if (length(all_data) == 0) {
    return(tibble(text = character(), source = character(),
                  brand = character(), type = character()))
  }

  bind_rows(all_data)
}

# --- Run for all brands ---
cat("=== Reddit Extraction Starting ===\n")
cat("(Uses Reddit JSON API directly — no Google dependency)\n\n")

reddit_list <- list()
for (name in names(search_queries)) {
  cat("\n--- Scraping", name, "---\n")
  result <- tryCatch(
    scrape_brand(name, search_queries[[name]]),
    error = function(e) {
      cat("  ERROR:", e$message, "\n")
      NULL
    }
  )
  if (!is.null(result) && nrow(result) > 0) {
    reddit_list[[name]] <- result
    cat("  =>", nrow(result), "rows for", name, "\n")
  }
}
reddit_data <- bind_rows(reddit_list)

# Deduplicate
reddit_data <- reddit_data %>% distinct(text, brand, .keep_all = TRUE)

cat("\n=== Results ===\n")
print(table(reddit_data$brand))
cat("Total:", nrow(reddit_data), "\n")

# --- Save ---
reddit_data <- reddit_data %>% mutate(text = str_sub(text, 1, 32000))
write_xlsx(reddit_data, paste0(save_path, "reddit_reviews.xlsx"))
saveRDS(reddit_data, paste0(save_path, "reddit_reviews.rds"))



