library(rvest)
library(purrr)
library(tidyverse)
library(SnowballC)
library(tidytext)
library(stopwords)
library(corpus)
library(textstem)
library(ggwordcloud)
library(ggthemes)
library(caret)
library(igraph)
library(ggraph)
library(topicmodels)
library(tictoc)
library(tm)
library(drlib)


trump_dat <- readRDS(url("https://github.com/codymg/lab_work/raw/master/data/trump_dat.rds"))

warren_dat <- readRDS(url("https://github.com/codymg/lab_work/raw/master/data/warren_dat.rds"))

sanders_dat <- readRDS(url("https://github.com/codymg/lab_work/raw/master/data/sanders_dat.rds"))

romney_dat <- readRDS(url("https://github.com/codymg/lab_work/raw/master/data/romney_dat.rds"))


dat <- trump_dat %>%
  bind_rows(warren_dat, sanders_dat, romney_dat) %>%
  mutate(speech_text = str_squish(speech_text)) %>%
  mutate(speech_text = str_trim(speech_text, "both")) %>%
  mutate(speech_text = str_remove_all(.$speech_text, "applause|laughter|thank|thanks|presidential|president|realdonaldtrump|donald|
                                      trump|bernie|senator|klobuchar|trump|white house|yang|sanders|warren|buttigieg|
                                      biden|joe|jr|mike|bloomberg|mayor|hillary|clinton|view document|pdf format|bush|
                                      romney|mitt|mitch|mcconnell|kevin|mccarthy|john|thune"))



#tokenizing 

token_df <- data.frame(txt = dat$speech_text, politician = dat$politician, speech_type = as.character(dat$speech_type), stringsAsFactors = FALSE)

stopwrds <- data.frame(words = stopwords("SMART"))

otherwrds <- data.frame(words = c("applause", "laughter", "president", "thank", "thanks"))

otherwrds$words <- as.character(otherwrds$words)

stopwrds <- stopwrds %>%
  bind_rows(otherwrds) %>%
  dplyr::select(words) %>%
  distinct(words)

#head(stopwords::stopwords("russian"), 30) provides preview of stop_words being used

word_list <- token_df %>%
  mutate(speech_id = 1:n()) %>%
  tidytext::unnest_tokens(word, txt) %>% 
  anti_join(stopwrds, by = c("word" = "words")) %>% #removing stop words based on snowball set
  filter(nchar(word) > 3) %>% #filters out words with less than 2 characters
  mutate(word_id = 1:n())

word_count <- token_df %>%
  tidytext::unnest_tokens(word, txt) %>% 
  dplyr::anti_join(stopwrds, by = c("word" = "words")) %>% #removing stop words based on snowball set
  dplyr::count(word, sort = TRUE) %>% 
  dplyr::filter(nchar(word) > 3) %>%
  left_join(word_list, by = c("word" = "word")) %>%
  distinct(word, .keep_all = TRUE)


################################################
################################################
#Sentiment
###############################################
###############################################

sentiments %>% 
  filter(lexicon == "AFINN") 

# a list of English words rated for valence with an integer between minus five (negative) and plus five (positive)

word_list %>%
  head(10)

word_list %>%
  left_join(., sentiment_afinn, by = c("word" = "term")) %>%
  group_by(politician) %>%
  summarize(mean_score = mean(score, na.rm=TRUE),
            total_words = n(),
            scored_words = sum(!is.na(score))) %>%
  arrange(desc(mean_score))


################################################
################################################
#Topic Modeling
###############################################
###############################################

token_df <- data.frame(txt = dat$speech_text, politician = dat$politician, speech_type = as.character(dat$speech_type), stringsAsFactors = FALSE)

pol_tokens <- token_df %>%
  mutate(speech_id = 1:n()) %>%
  unnest_tokens(output = word, input = txt) %>%
  anti_join(stop_words) %>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(nchar(word) > 2)


pol_dtm <- pol_tokens %>%
  # get count of each token in each document
  dplyr::count(speech_id, word) %>%
  # create a document-term matrix with all features and tf weighting
  cast_dtm(document = speech_id, term = word, value = n)

pol_dtm


################################################
################################################
#ngrams
###############################################
###############################################

pol_digram <- dat %>%
  unnest_tokens(bigram, speech_text, token = "ngrams", n = 2, collapse = FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ", remove = FALSE)

dat %>%
  unnest_tokens(bigram, speech_text, token ="skip_ngrams", n = 2, k = 1, collapse = FALSE) %>%
  head(10)

my_stop_words <- setdiff(stop_words$word, "not")  # Need

pol_digram <- pol_digram %>%
  filter(!word1 %in% my_stop_words, !word2 %in% my_stop_words) %>%
  mutate_at(vars(word1, word2), SnowballC::wordStem) %>%
  mutate(stemmed_digram = paste0(word1, " ", word2))


res_digram <-  pol_digram %>% 
  group_by(politician, stemmed_digram) %>%
  summarize(n = n()) %>%
  group_by(politician) %>%
  top_n(30, n + runif(n(), 0, 0.01)) %>% # runif to randomly breaks 10th place ties...
  ungroup() %>%
  arrange(politician, n) %>%
  mutate(.r = row_number()) 

res_digram %>% head(20)

gg <- ggplot(res_digram, aes(x=.r, y=n)) +
  facet_wrap(~politician,
             scales="free_y") + 
  geom_col() + 
  coord_flip() + xlab("") +
  scale_x_continuous(  # This handles replacement of .r for x
    breaks = res_digram$.r,     # notice need to reuse data frame
    labels = res_digram$stemmed_digram )
gg

pol_digram %>%
  left_join(sentiments %>% filter(lexicon=="AFINN") %>% mutate(word = SnowballC::wordStem(word)), by=c("word2"="word")) %>% #mutate makes the words in the lexicon has multiple stems and left join copies additional rows for the multiple stems making the list longer than it should (using some unique filtering would help solve thi problem)
  group_by(politician) %>%
  summarize(mean_score=mean(score*(1-2*(word1=="not")), na.rm=TRUE), #logical command will automatically be converted to 1 and 0 from true or false
            total_words = n(), total_nots = sum(word1=="not", na.rm=T), 
            scored_words = sum(!is.na(score))) %>%
  arrange(mean_score)

plt <- res_digram %>%
  filter(n > 10) %>%
  tidyr::separate(stemmed_digram, into = c("word1", "word2"), sep = " ") %>%
  dplyr::select(word1, word2) %>%
  filter(word1 !="NA" | word2 !="NA") %>%
  igraph::graph_from_data_frame() 

par.old <- par(mai=c(0,0,0,0))
plot(plt)
par()


visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

res_digram %>%
  filter(n > 40) %>%
  visualize_bigrams()


pol_lda <- LDA(pol_dtm, k = 5, control = list(seed = 123))

pol_lda <- tidy(pol_lda)

top_terms <- pol_lda %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(topic = factor(topic),
         term = drlib::reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip()

beta_spread <- pol_lda %>%
  filter(topic == 1 | topic == 2) %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>%
  filter(log_ratio > 20 | log_ratio < -20) %>%
  mutate(term = fct_reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip()