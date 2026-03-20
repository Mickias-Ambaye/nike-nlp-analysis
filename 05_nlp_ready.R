# ==============================================================================
# NIKE TEXT ANALYTICS — FULL NLP ANALYSIS
# Team Assignment: Text Analytics for Strategic Brand Intelligence
# Client: Nike | Competitors: Adidas, Under Armour
# ==============================================================================
# Data: master_clean.rds (17,348 documents from App Store, Reddit, Trustpilot)
# Columns: text, source, brand, rating, doc_id
# ==============================================================================

###############################################################
######## SECTION 0: LIBRARIES & DATA LOADING ##################
###############################################################

#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("topicmodels")
#install.packages("SnowballC")
#install.packages("igraph")
#install.packages("ggraph")
#install.packages("widyr")
#install.packages("quanteda")
#install.packages("quanteda.textmodels")

library(tidyverse)      # data wrangling + ggplot2
library(tidytext)       # tokenization, sentiment, tf-idf
library(topicmodels)    # LDA topic modeling
library(SnowballC)      # word stemming
library(igraph)         # network graphs
library(ggraph)         # visualize networks
library(widyr)          # pairwise correlations
library(scales)         # percent formatting for plots

# --- Load the clean master dataset ---
master <- readRDS(url("https://github.com/Mickias-Ambaye/nike-nlp-analysis/raw/main/Dataset/master_clean.rds"))
# --- Quick summary ---
cat("=== NLP Dataset Loaded ===\n")
cat("Total documents:", nrow(master), "\n\n")
cat("Per brand:\n")
print(count(master, brand))
cat("\nPer source:\n")
print(count(master, source))
cat("\nBrand x Source:\n")
print(master %>% count(brand, source) %>% 
        pivot_wider(names_from = source, values_from = n, values_fill = 0))


###############################################################
######## SECTION 1: TEXT PREPROCESSING ########################
###############################################################

# Step 1a: Tokenization — splitting text into individual words
# This follows the tidytext unnest_tokens() approach from class

master_tokens <- master %>%
  unnest_tokens(word, text)

master_tokens
# we can see every word is now a row, linked to its doc_id, brand, source, rating

###############################################################
######## Step 1b: Stopword Removal ############################
###############################################################

# removing common English words that don't carry meaning (the, is, at, etc.)
master_tokens <- master_tokens %>%
  anti_join(stop_words)

master_tokens

# let's also remove some custom stopwords specific to our dataset
# these are words that appear a lot but don't add analytical value
# Custom stopwords WITHOUT brand names — we keep brand names for competitive analysis
# (TF-IDF, bigrams, co-occurrence networks need to see "nike", "adidas", "under", "armour")
custom_stops <- tibble(word = c("app", "http", "https",
                                "www", "com", "quot", "amp", "lt", "gt",
                                "1", "2", "3", "4", "5", "00", "10", "de",
                                "it's", "i'm", "don't", "i've", "didn't",
                                "can't", "doesn't", "isn't", "won't", "it’s", "wiki",
                                "0"))

master_tokens <- master_tokens %>%
  anti_join(custom_stops)

# let's see the most common words after cleaning
master_tokens %>%
  count(word, sort = TRUE)


# Now create a versioFALSE# Now create a version WITHOUT brand names — for sentiment, LDA, and emotion analysis
# where brand names would dominate topics without adding insight
custom_stops_brands <- tibble(word = c("nike", "adidas", "under", "armour"))

master_tokens_no_brands <- master_tokens %>%
  anti_join(custom_stops_brands)

# this is what we'll use for sentiment and topic modeling
master_tokens_no_brands %>%
  count(word, sort = TRUE)

###############################################################
######## Step 1c: Stemming ####################################
###############################################################

# Stemming reduces words to their root form (running -> run, shoes -> shoe)
# We use the SnowballC package as seen in class

master_tokens <- master_tokens %>%
  mutate(word_stem = wordStem(word))

# let's see some examples of stemming
master_tokens %>%
  select(word, word_stem) %>%
  filter(word != word_stem) %>%
  distinct() %>%
  head(20)


# NOTE: for all downstream analysis we continue using the original 'word' column
# because tidytext sentiment lexicons match on original words, not stems.
# The stemmed column (word_stem) is available if needed for specific analyses.

###############################################################
######## Step 1e: N-grams (Bigrams) ###########################
###############################################################

# Bigrams capture two-word phrases — important for context
# e.g. "not good" vs just "good", "air max", "running shoes"

master_bigrams <- master %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

master_bigrams #We want to see the bigrams (words that appear together, "pairs")

master_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them

#to remove stop words from the bigram data, we need to use the separate function:
bigrams_separated <- master_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% custom_stops$word) %>%
  filter(!word2 %in% custom_stops$word)

#creating the new bigram counts, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

# bigram counts by brand — useful for competitive comparison
bigram_counts_brand <- bigrams_filtered %>%
  count(brand, word1, word2, sort = TRUE)

bigram_counts_brand

###############################################################
######## Step 1e.2: Trigrams ##################################
###############################################################

# Trigrams capture three-word phrases
# useful for loyalty language: "worth every penny", "best running shoe"

master_trigrams <- master %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word1 %in% custom_stops$word) %>%
  filter(!word2 %in% custom_stops$word) %>%
  filter(!word3 %in% custom_stops$word)

trigram_counts <- master_trigrams %>%
  count(word1, word2, word3, sort = TRUE)

trigram_counts

###############################################################
######## Step 1f: Document-Term Matrix (DTM) ##################
###############################################################

# The DTM is required for LDA topic modeling
# Each row = a document, each column = a word, values = word counts

master_dtm <- master_tokens %>%
  count(doc_id, word, sort = TRUE) %>%
  cast_dtm(doc_id, word, n)

master_dtm
# this shows us the dimensions: documents x terms and sparsity


###############################################################
###############################################################
######## SECTION 2: SENTIMENT ANALYSIS ########################
###############################################################
###############################################################

# Q1 — Brand Perception: How is Nike emotionally positioned online?

###############################################################
######## Step 2a: NRC Emotion Lexicon — Full Emotion Profile ##
###############################################################

# NRC gives us 8 emotions: joy, trust, anticipation, anger, disgust, 
# sadness, fear, surprise — plus positive/negative
# This directly answers: "What do customers FEEL about Nike?"

# NOTE: the first time you run get_sentiments("nrc") or get_sentiments("afinn"),
# R will prompt you to download the lexicon. Just type 1 (yes) in the console.
# This is built into tidytext — no extra library needed.

nrc_emotions <- get_sentiments("nrc")

# emotion profile for ALL brands
# using master_tokens_no_brands so brand names don't skew the emotion scores
emotion_profile <- master_tokens_no_brands %>%
  inner_join(nrc_emotions) %>%
  count(brand, sentiment) %>%
  group_by(brand) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

emotion_profile

# plotting the emotion breakdown by brand — this is our emotional positioning map
emotion_profile %>%
  ggplot(aes(sentiment, proportion, fill = brand)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Emotional Positioning: Nike vs Adidas vs Under Armour",
       x = "Emotion", y = "Proportion of Emotion Words") +
  theme_minimal()

###############################################################
######## Step 2b: NRC — Specific Emotion Deep Dive (Nike) #####
###############################################################

# let's look at which words drive each emotion for Nike specifically
# this helps us understand WHY customers feel a certain way

nrc_anger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

# what anger words appear in Nike reviews?
master_tokens %>%
  filter(brand == "Nike") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

nrc_trust <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")

# what trust words appear in Nike reviews?
master_tokens %>%
  filter(brand == "Nike") %>%
  inner_join(nrc_trust) %>%
  count(word, sort = TRUE)

nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

# what joy words appear in Nike reviews?
master_tokens %>%
  filter(brand == "Nike") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

###############################################################
######## Step 2c: Comparing Sentiment Libraries ################
###############################################################

# This validates our sentiment findings across multiple methods

nike_tokens <- master_tokens %>%
  filter(brand == "Nike")

afinn <- nike_tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  nike_tokens %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  nike_tokens %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs(title = "Nike Sentiment: Comparing Three Lexicon Methods")

###############################################################
######## Step 2d: Most Common Positive & Negative Words #######
###############################################################

# Using Bing lexicon to classify words as positive or negative
# then finding the most common ones — great for the executive presentation

bing_word_counts <- master_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# top positive and negative words across all brands
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  labs(title = "Most Common Positive & Negative Words (All Brands)",
       y = "Frequency", x = NULL)

# same but broken down by brand
bing_brand <- master_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(brand, word, sentiment, sort = TRUE)

bing_brand %>%
  group_by(brand, sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(brand ~ sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top Positive & Negative Words by Brand")

###############################################################
######## Step 2e: Sentiment by Source #########################
###############################################################

# do App Store reviews feel different from Reddit or Trustpilot?
# this helps us understand sampling bias

sentiment_by_source <- master_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(source, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative)

sentiment_by_source

sentiment_by_source %>%
  ggplot(aes(reorder(source, net_sentiment), net_sentiment, 
             fill = net_sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Net Sentiment by Data Source",
       x = "Source", y = "Net Sentiment (Positive - Negative)")

###############################################################
###############################################################
######## SECTION 3: TOPIC MODELING (LDA) ######################
###############################################################
###############################################################

# LDA helps us discover hidden themes in the reviews
# Two principles (from class):
# 1. Every document is a combination of multiple topics
# 2. Every topic is a combination of multiple words



###############################################################
######## Step 3b: Beta Spread — Comparing Topics ##############
###############################################################

#lets calculate the relative difference between the betas
# comparing topic 1 vs topic 2 to see what differentiates them

beta_spread <- master_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  mutate(log_rate = log2(topic2 / topic1))

beta_spread %>%
  arrange(desc(log_rate))

###############################################################
######## Step 3d: LDA by Brand (Nike-specific) ################
###############################################################

# running a separate LDA just for Nike to get more specific themes
nike_dtm <- master_tokens %>%
  filter(brand == "Nike") %>%
  count(doc_id, word, sort = TRUE) %>%
  cast_dtm(doc_id, word, n)

nike_lda <- LDA(nike_dtm, k = 4, control = list(seed = 123))

nike_topics <- tidy(nike_lda, matrix = "beta")

nike_top_terms <- nike_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# naming the topics for the executive presentation
topic_labels <- tibble(
  topic = 1:4,
  topic_name = c("Running & Performance",
                 "Product Quality & Sizing",
                 "App & Workout Experience",
                 "Customer Service & Returns")
)

nike_top_terms %>%
  left_join(topic_labels) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic_name))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic_name, scales = "free") +
  coord_flip() +
  labs(title = "LDA: Nike-Specific Topics (k=4)",
       x = NULL, y = "Beta (word probability)")


###############################################################
###############################################################
######## SECTION 4: TF-IDF — BRAND DIFFERENTIATION ############
###############################################################
###############################################################

# Q3 — Competitive Positioning: What differentiates Nike's brand language?
# TF-IDF finds words that are uniquely important to each brand
# High TF-IDF = word is common in one brand but rare across others

###############################################################
######## Step 4a: TF-IDF by Brand #############################
###############################################################

#we're grouping by the brand this time
brand_words <- master_tokens %>%
  count(brand, word, sort = TRUE) %>%
  ungroup()

total_words <- brand_words %>%
  group_by(brand) %>%
  summarise(total = sum(n))

brand_words <- left_join(brand_words, total_words)
print(brand_words)

# lets see the distribution of word frequencies per brand
ggplot(brand_words, aes(n/total, fill = brand)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.001) +
  facet_wrap(~brand, ncol = 2, scales = "free_y") +
  labs(title = "Word Frequency Distribution by Brand")

###############################################################
######## Step 4b: Zipf's Law ##################################
###############################################################

freq_by_rank <- brand_words %>%
  group_by(brand) %>%
  mutate(rank = row_number(),
         `term frequency` = n / total)
freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = brand)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = 'gray50', linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Zipf's Law: Word Frequency vs Rank by Brand")

###############################################################
######## Step 4c: TF-IDF Calculation ##########################
###############################################################

brand_tf_idf <- brand_words %>%
  bind_tf_idf(word, brand, n)

brand_tf_idf # we get all the zeros because we are looking at stop words ... too common

# what makes Nike unique?
brand_tf_idf %>%
  filter(brand == "Nike") %>%
  arrange(desc(tf_idf))

# what makes Adidas unique?
brand_tf_idf %>%
  filter(brand == "Adidas") %>%
  arrange(desc(tf_idf))

# looking at the graphical approach:
brand_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(brand) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = brand)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf",
       title = "TF-IDF: Most Distinctive Words per Brand") +
  facet_wrap(~brand, ncol = 2, scales = "free") +
  coord_flip()


###############################################################
###############################################################
######## SECTION 5: PAIRWISE CORRELATIONS & NETWORKS ##########
###############################################################
###############################################################

# Keyword co-occurrence network — what words cluster together?
# This answers Q3: what themes orbit around each brand?

###############################################################
######## Step 5a: Pairwise Correlations (Nike) ################
###############################################################

nike_tidy <- master_tokens %>%
  filter(brand == "Nike")

#taking out the least common words
nike_word_cors <- nike_tidy %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, doc_id, sort = TRUE)
#pairwise_cor() checks correlation based on how often words appear in the same document

nike_word_cors %>%
  filter(item1 == "quality")

nike_word_cors %>%
  filter(item1 == "price")

nike_word_cors %>%
  filter(item1 == "sustainable")

###############################################################
######## Step 5b: Correlation Bar Charts ######################
###############################################################

# what words are most correlated with our key business themes?
nike_word_cors %>%
  filter(item1 %in% c("quality", "price", "comfortable", "workout")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~item1, scales = "free") +
  coord_flip() +
  labs(title = "Nike: Words Most Correlated with Key Themes")

###############################################################
######## Step 5c: Correlation Network #########################
###############################################################

#this will take some time to run, we will need to wait for the result

nike_word_cors %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = F) +
  geom_node_point(color = "lightgreen", size = 6) +
  geom_node_text(aes(label = name), repel = T) +
  theme_void() +
  labs(title = "Nike: Word Co-occurrence Network")


# keep only words that appear frequently enough to matter
nike_tidy_filtered <- master_tokens %>%
  filter(brand == "Nike") %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  ungroup()

# recalculate correlations on this smaller set
nike_word_cors_filtered <- nike_tidy_filtered %>%
  pairwise_cor(word, doc_id, sort = TRUE)

# now plot — this will be much faster
nike_word_cors_filtered %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = F) +
  geom_node_point(color = "lightgreen", size = 6) +
  geom_node_text(aes(label = name), repel = T, max.overlaps = 30) +
  theme_void() +
  labs(title = "Nike: Word Co-occurrence Network")

###############################################################
######## Step 5d: Bigram Network Visualization ################
###############################################################

nike_bigram_counts <- bigrams_filtered %>%
  filter(brand == "Nike") %>%
  count(word1, word2, sort = TRUE)

nike_bigram_graph <- nike_bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

ggraph(nike_bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = T, max.overlaps = 20) +
  theme_void() +
  labs(title = "Nike: Bigram Network (pairs appearing 20+ times)")


###############################################################
###############################################################
######## SECTION 6: ASPECT-BASED SENTIMENT ####################
###############################################################
###############################################################

# Q2 — Pricing Sensitivity
# Instead of scoring overall sentiment, we isolate SENTENCES about specific
# aspects (price, quality, sustainability) and score them separately

###############################################################
######## Step 6a: Extract Price-Related Sentences #############
###############################################################

# first, we split reviews into sentences for more granular analysis

master_sentences <- master %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number())

# find sentences that mention pricing
price_keywords <- c("price", "expensive", "cheap", "overpriced", "afford",
                     "cost", "worth", "money", "budget", "value", "dollar",
                     "pay", "paid", "pricing", "premium")

price_sentences <- master_sentences %>%
  filter(str_detect(sentence, paste(price_keywords, collapse = "|")))

cat("Total sentences:", nrow(master_sentences), "\n")
cat("Price-related sentences:", nrow(price_sentences), "\n")

# now score sentiment on price sentences only
price_sentiment <- price_sentences %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("bing")) %>%
  count(brand, doc_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative)

# average price sentiment by brand
price_sentiment %>%
  group_by(brand) %>%
  summarise(avg_price_sentiment = mean(net_sentiment),
            n_reviews = n())

# plot: price sentiment by brand
price_sentiment %>%
  ggplot(aes(brand, net_sentiment, fill = brand)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Price Sentiment by Brand (Aspect-Based)",
       subtitle = "Only sentences mentioning price/cost/value",
       x = "Brand", y = "Net Sentiment") +
  theme_minimal()

###############################################################
######## Step 6b: Quality Aspect Sentiment ####################
###############################################################

quality_keywords <- c("quality", "durable", "durability", "material", "broke",
                       "falling apart", "defect", "rip", "tear", "wear out",
                       "lasted", "build", "construction", "stitching", "sole")

quality_sentences <- master_sentences %>%
  filter(str_detect(sentence, paste(quality_keywords, collapse = "|")))

quality_sentiment <- quality_sentences %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("bing")) %>%
  count(brand, doc_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative)

quality_sentiment %>%
  group_by(brand) %>%
  summarise(avg_quality_sentiment = mean(net_sentiment),
            n_reviews = n())

quality_sentiment %>%
  ggplot(aes(brand, net_sentiment, fill = brand)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Quality Sentiment by Brand (Aspect-Based)",
       subtitle = "Only sentences mentioning quality/durability/materials",
       x = "Brand", y = "Net Sentiment") +
  theme_minimal()

###############################################################
######## Step 6c: Sustainability Aspect Sentiment #############
###############################################################

# Q4 — Is Nike's sustainability messaging resonating?

sustainability_keywords <- c("sustainable", "sustainability", "eco", "recycle",
                              "recycled", "green", "environment", "carbon",
                              "ethical", "move to zero", "organic", "planet",
                              "climate", "footprint")

sustainability_sentences <- master_sentences %>%
  filter(str_detect(sentence, paste(sustainability_keywords, collapse = "|")))

cat("Sustainability mentions:", nrow(sustainability_sentences), "\n")

# sustainability mentions by brand
sustainability_sentences %>%
  count(brand) %>%
  ggplot(aes(brand, n, fill = brand)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sustainability Mentions by Brand",
       x = "Brand", y = "Number of Sentences")

# sentiment on sustainability sentences
sustainability_sentiment <- sustainability_sentences %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("bing")) %>%
  count(brand, doc_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative)

sustainability_sentiment %>%
  group_by(brand) %>%
  summarise(avg_sustainability_sentiment = mean(net_sentiment),
            n_reviews = n())

###############################################################
###############################################################
######## SECTION 7: WORD FREQUENCY CORRELATIONS ###############
###############################################################
###############################################################

# Comparing word usage across brands — like we did with Netflix countries
# Correlogram approach: are Nike and Adidas using similar language?

###############################################################
######## Step 7a: Word Frequency by Brand #####################
###############################################################

frequency <- bind_rows(
  master_tokens %>% filter(brand == "Nike") %>% mutate(author = "Nike"),
  master_tokens %>% filter(brand == "Adidas") %>% mutate(author = "Adidas"),
  master_tokens %>% filter(brand == "Under Armour") %>% mutate(author = "Under Armour")
) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Adidas`, `Under Armour`)

#let's plot the correlograms:
ggplot(frequency, aes(x = proportion, y = `Nike`,
                      color = abs(`Nike` - proportion))) +
  geom_abline(color = "grey40", lty = 2) +
  geom_jitter(alpha = .1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Nike", x = NULL,
       title = "Word Frequency Correlation: Nike vs Competitors")

###############################################################
######## Step 7b: Correlation Tests ###########################
###############################################################

# how similar is Nike's language to Adidas?
cor.test(data = frequency[frequency$author == "Adidas", ],
         ~proportion + `Nike`)

# how similar is Nike's language to Under Armour?
cor.test(data = frequency[frequency$author == "Under Armour", ],
         ~proportion + `Nike`)


