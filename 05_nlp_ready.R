# ==============================================================================
# NIKE TEXT ANALYTICS — FULL NLP ANALYSIS
# Team Assignment: Text Analytics for Strategic Brand Intelligence
# Brand Perception, Pricing, Competitive Positioning
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
#install.packages("scales")

library(tidyverse)
library(tidytext)
library(topicmodels)
library(SnowballC)
library(igraph)
library(ggraph)
library(widyr)
library(scales)

# load clean dataset from GitHub
master <- readRDS(url("https://github.com/Mickias-Ambaye/nike-nlp-analysis/raw/main/Dataset/master_clean.rds"))

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

# Step 1a: Tokenize — one word per row
master_tokens <- master %>%
  unnest_tokens(word, text)

master_tokens
# each word is now a row linked to doc_id, brand, source, rating


###############################################################
######## Step 1b: Stopword Removal ############################
###############################################################

# drop common English words (the, is, at, etc.)
master_tokens <- master_tokens %>%
  anti_join(stop_words)

master_tokens

# custom stopwords — keeping brand names for competitive analysis later
custom_stops <- tibble(word = c("app", "http", "https",
                                "www", "com", "quot", "amp", "lt", "gt",
                                "1", "2", "3", "4", "5", "00", "10", "de",
                                "it's", "i'm", "don't", "i've", "didn't",
                                "can't", "doesn't", "isn't", "won't", "wiki",
                                "0"))

master_tokens <- master_tokens %>%
  anti_join(custom_stops)

# drop numeric junk, alphanumeric codes, and single-char tokens
# catches things like "232335", "1x", "3d", "00s", "2nd" etc.
master_tokens <- master_tokens %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%        # pure numbers
  filter(!str_detect(word, "^[0-9]+[a-z]{1,2}$")) %>%  # "1x", "3d", "2nd", "00s"
  filter(!str_detect(word, "^[a-z]{1,2}[0-9]+$")) %>%  # "x1", "v2"
  filter(nchar(word) >= 2)                          # single letters

# check top words after cleaning
master_tokens %>%
  count(word, sort = TRUE)

# version without brand names — used for sentiment + LDA so brand names
# don't drown out the actual topic words
custom_stops_brands <- tibble(word = c("nike", "adidas", "under", "armour"))

master_tokens_no_brands <- master_tokens %>%
  anti_join(custom_stops_brands)

master_tokens_no_brands %>%
  count(word, sort = TRUE)


###############################################################
######## Step 1c: Stemming ####################################
###############################################################

# reduce words to root form (running -> run, shoes -> shoe)
master_tokens <- master_tokens %>%
  mutate(word_stem = wordStem(word))

# quick look at some stemmed examples
master_tokens %>%
  select(word, word_stem) %>%
  filter(word != word_stem) %>%
  distinct() %>%
  head(20)

# sticking with original 'word' column for analysis since
# sentiment lexicons match on full words, not stems


###############################################################
######## Step 1d: Bigrams #####################################
###############################################################

# two-word phrases — picks up context like "not good", "air max"
master_bigrams <- master %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# split and remove stopwords from both sides
bigrams_separated <- master_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% custom_stops$word) %>%
  filter(!word2 %in% custom_stops$word)

# bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

# by brand
bigram_counts_brand <- bigrams_filtered %>%
  count(brand, word1, word2, sort = TRUE)

bigram_counts_brand


###############################################################
######## Step 1e: Trigrams ####################################
###############################################################

# three-word phrases — catches stuff like "worth every penny"
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

# DTM needed for LDA — rows = documents, columns = words, values = counts
master_dtm <- master_tokens %>%
  count(doc_id, word, sort = TRUE) %>%
  cast_dtm(doc_id, word, n)

master_dtm


###############################################################
###############################################################
######## SECTION 2: SENTIMENT ANALYSIS ########################
###############################################################
###############################################################

# Q1 — How is Nike emotionally positioned compared to competitors?

###############################################################
######## Step 2a: NRC Emotion Lexicon #########################
###############################################################

# NRC gives 8 emotions + positive/negative
# first run: type 1 to download the lexicon
nrc_emotions <- get_sentiments("nrc")

# emotion breakdown per brand
emotion_profile <- master_tokens_no_brands %>%
  inner_join(nrc_emotions, relationship = "many-to-many") %>%
  count(brand, sentiment) %>%
  group_by(brand) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

emotion_profile

emotion_profile %>%
  ggplot(aes(sentiment, proportion, fill = brand)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Emotional Positioning: Nike vs Adidas vs Under Armour",
       x = "Emotion", y = "Proportion of Emotion Words") +
  theme_minimal()


###############################################################
######## Step 2b: Nike Emotion Deep Dive ######################
###############################################################

# which words are driving anger, trust, joy for Nike?
nrc_anger <- get_sentiments("nrc") %>% filter(sentiment == "anger")
nrc_trust <- get_sentiments("nrc") %>% filter(sentiment == "trust")
nrc_joy   <- get_sentiments("nrc") %>% filter(sentiment == "joy")

# anger words
master_tokens %>%
  filter(brand == "Nike") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# trust words
master_tokens %>%
  filter(brand == "Nike") %>%
  inner_join(nrc_trust) %>%
  count(word, sort = TRUE)

# joy words
master_tokens %>%
  filter(brand == "Nike") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


###############################################################
######## Step 2c: Comparing Sentiment Methods #################
###############################################################

# cross-check Nike sentiment with AFINN, Bing, and NRC
nike_tokens <- master_tokens %>%
  filter(brand == "Nike")

afinn <- nike_tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  nike_tokens %>%
    inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
    mutate(method = "Bing et al."),
  nike_tokens %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative")),
               relationship = "many-to-many") %>%
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
######## Step 2d: Top Positive & Negative Words ###############
###############################################################

bing_word_counts <- master_tokens %>%
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# all brands combined
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

# per brand breakdown
bing_brand <- master_tokens %>%
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
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

sentiment_by_source <- master_tokens %>%
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
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

# LDA finds hidden themes — each doc is a mix of topics,
# each topic is a mix of words

###############################################################
######## Step 3a: Nike-Specific LDA (k=4) #####################
###############################################################

# building DTM for Nike only
nike_dtm <- master_tokens %>%
  filter(brand == "Nike") %>%
  count(doc_id, word, sort = TRUE) %>%
  cast_dtm(doc_id, word, n)

# fit LDA with 4 topics (maps to our 4 business questions)
nike_lda <- LDA(nike_dtm, k = 4, control = list(seed = 123))
nike_topics <- tidy(nike_lda, matrix = "beta")

nike_top_terms <- nike_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# label the topics based on what words show up
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
  labs(title = "LDA: Nike Topics (k=4)",
       x = NULL, y = "Beta (word probability)")


###############################################################
###############################################################
######## SECTION 4: TF-IDF — BRAND DIFFERENTIATION ############
###############################################################
###############################################################

# Q3 — What words set Nike apart from Adidas and Under Armour?
# High TF-IDF = word is common in one brand but rare in others

###############################################################
######## Step 4a: TF-IDF by Brand #############################
###############################################################

brand_words <- master_tokens %>%
  count(brand, word, sort = TRUE) %>%
  ungroup()

total_words <- brand_words %>%
  group_by(brand) %>%
  summarise(total = sum(n))

brand_words <- left_join(brand_words, total_words)

# word frequency distribution
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

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = brand)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = 'gray50', linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Zipf's Law: Word Frequency vs Rank by Brand")


###############################################################
######## Step 4c: TF-IDF Calculation ##########################
###############################################################

brand_tf_idf <- brand_words %>%
  bind_tf_idf(word, brand, n)

# Nike's most unique words
brand_tf_idf %>%
  filter(brand == "Nike") %>%
  arrange(desc(tf_idf))

# Adidas's most unique words
brand_tf_idf %>%
  filter(brand == "Adidas") %>%
  arrange(desc(tf_idf))

# visual comparison
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

# what words tend to show up together in Nike reviews?

###############################################################
######## Step 5a: Pairwise Correlations (Nike) ################
###############################################################

# only words appearing 20+ times (faster and cleaner)
nike_tidy_filtered <- master_tokens %>%
  filter(brand == "Nike") %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  ungroup()

nike_word_cors <- nike_tidy_filtered %>%
  pairwise_cor(word, doc_id, sort = TRUE)

nike_word_cors %>% filter(item1 == "quality")
nike_word_cors %>% filter(item1 == "price")
nike_word_cors %>% filter(item1 == "sustainable")


###############################################################
######## Step 5b: Correlation Bar Charts ######################
###############################################################

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

nike_word_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = F) +
  geom_node_point(color = "lightgreen", size = 6) +
  geom_node_text(aes(label = name), repel = T, max.overlaps = 30) +
  theme_void() +
  labs(title = "Nike: Word Co-occurrence Network")


###############################################################
######## Step 5d: Bigram Network ##############################
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

# Q2 — Pricing: how do people feel about each brand's pricing?

###############################################################
######## Step 6a: Price Sentiment #############################
###############################################################

# split reviews into sentences first
master_sentences <- master %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number())

price_keywords <- c("price", "expensive", "cheap", "overpriced", "afford",
                     "cost", "worth", "money", "budget", "value", "dollar",
                     "pay", "paid", "pricing", "premium")

price_sentences <- master_sentences %>%
  filter(str_detect(sentence, paste(price_keywords, collapse = "|")))

cat("Total sentences:", nrow(master_sentences), "\n")
cat("Price-related sentences:", nrow(price_sentences), "\n")

# score sentiment on price sentences only
price_sentiment <- price_sentences %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
  count(brand, doc_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative)

# avg price sentiment per brand
price_sentiment %>%
  group_by(brand) %>%
  summarise(avg_price_sentiment = mean(net_sentiment),
            n_reviews = n())

price_sentiment %>%
  ggplot(aes(brand, net_sentiment, fill = brand)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Price Sentiment by Brand",
       subtitle = "Sentences mentioning price/cost/value only",
       x = "Brand", y = "Net Sentiment") +
  theme_minimal()


###############################################################
######## Step 6b: Quality Sentiment ###########################
###############################################################

quality_keywords <- c("quality", "durable", "durability", "material", "broke",
                       "falling apart", "defect", "rip", "tear", "wear out",
                       "lasted", "build", "construction", "stitching", "sole")

quality_sentences <- master_sentences %>%
  filter(str_detect(sentence, paste(quality_keywords, collapse = "|")))

quality_sentiment <- quality_sentences %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
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
  labs(title = "Quality Sentiment by Brand",
       subtitle = "Sentences mentioning quality/durability/materials only",
       x = "Brand", y = "Net Sentiment") +
  theme_minimal()


###############################################################
######## Step 6c: Sustainability Sentiment ####################
###############################################################

# Q4 — Is Nike's sustainability messaging landing with consumers?
sustainability_keywords <- c("sustainable", "sustainability", "eco", "recycle",
                              "recycled", "green", "environment", "carbon",
                              "ethical", "move to zero", "organic", "planet",
                              "climate", "footprint")

sustainability_sentences <- master_sentences %>%
  filter(str_detect(sentence, paste(sustainability_keywords, collapse = "|")))

cat("Sustainability mentions:", nrow(sustainability_sentences), "\n")

# which brand gets mentioned most for sustainability?
sustainability_sentences %>%
  count(brand) %>%
  ggplot(aes(brand, n, fill = brand)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sustainability Mentions by Brand",
       x = "Brand", y = "Number of Sentences")

# sentiment around sustainability
sustainability_sentiment <- sustainability_sentences %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
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

# how similar is Nike's vocabulary to competitors?

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

# Nike vs Adidas — how similar is their language?
cor.test(data = frequency[frequency$author == "Adidas", ],
         ~proportion + `Nike`)

# Nike vs Under Armour
cor.test(data = frequency[frequency$author == "Under Armour", ],
         ~proportion + `Nike`)


