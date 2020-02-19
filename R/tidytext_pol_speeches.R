library(tidyverse)
library(SnowballC)
library(tidytext)
library(stopwords)
library(corpus)
library(textstem)
library(ggwordcloud)
library(ggthemes)

#Analysis 

dat <- readRDS("C:/Users/codyg/Documents/GitHub_Projects/Presidential_Speeches/data/pol_2020_speeches.rds") %>% #read in data
  filter(politician == 'donald trump') #filtering to politician of your choice for sake of quicker analysis


#tokenizing 

token_df <- data.frame(txt = dat$speech_text, stringsAsFactors = FALSE)

stopwrds <- data.frame(words = stopwords("english", source = "snowball"))

otherwrds <- data.frame(words = c("applause", "laughter", "president", "thank", "thanks"))

otherwrds$words <- as.character(otherwrds$words)

stopwrds <- stopwrds %>%
  bind_rows(otherwrds) %>%
  dplyr::select(words) %>%
  distinct(words)

#head(stopwords::stopwords("russian", source = "snowball"), 30) provides preview of stop_words being used

count_test <- token_df %>%
  tidytext::unnest_tokens(word, txt) %>% 
  anti_join(stopwrds, by = c("word" = "words")) %>% #removing stop words based on snowball set
  count(word, sort = TRUE) %>% 
  filter(nchar(word) > 2) %>% #filters out words with less than 2 characters
  #filter(!word %in% c("","")) %>% would filter out specified non-meaningful words
  arrange(desc(n)) %>%
  head(25) #produces in UTF-8

wrds <- count_test %>% as.data.frame(.) %>%
  print.corpus_frame(.) #prints in russian and other non_UTF-8 languages

#stemmatizing

stems <- token_df %>%  
  tidytext::unnest_tokens(word, txt) %>%
  anti_join(stopwrds, by = c("word" = "words")) %>% #removing stop words based on snowball set
  mutate(stem = wordStem(word, language = "english")) %>% #getting only stems
  filter(nchar(stem) > 2) %>% #filters out words with less than 2 characters
  count(stem, sort = TRUE) %>%
  #filter(!stem %in% c("", "")) %>% filtering out non-meaningful words
  head(25) #produces in UTF-8

stms <- as.data.frame(stems) %>% 
  print.corpus_frame(.) #prints in russian

#lemmatization  using tidytext (UDpipe is also another option for non-english language NLP)

lemmas <- token_df %>% #without filler words 
  tidytext::unnest_tokens(word, txt) %>%
  anti_join(stopwrds, by = c("word" = "words")) %>%
  mutate(lemma = lemmatize_words(word)) %>% #lemmitizing
  filter(lemma != ("s")) %>% #filtering out non-meaningful words
  count(lemma, sort = TRUE) %>%
  head(25)

lemmas


freqdf <- cbind(wrds,stms,lemmas) %>% as.data.frame(.) %>% print.corpus_frame(.)


cloud <- ggplot(as.data.frame(token_df) %>%
                  tidytext::unnest_tokens(word, txt) %>%
                  anti_join(stop_words) %>%
                  mutate(lemma = lemmatize_words(word)) %>%
                  group_by(lemma) %>%
                  summarize(count = n()) %>%
                  filter(nchar(lemma)>3, str_detect(lemma,"^[a-z]")) %>%
                  top_n(25, count) %>%
                  mutate(count = count/sum(count)) %>%
                  arrange(desc(count)) %>%
                  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1))), 
                aes(label = lemma, size=count, color = count, angle = angle)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 25) +
  theme_minimal() +
  scale_color_gradient(low = "darkred", high = "red") +
  labs(title = "Trump Says", subtitle = "President Trump's Public Speaking Patterns, Jan. 15, 2015 - Feb. 5, 2020")

cloud
