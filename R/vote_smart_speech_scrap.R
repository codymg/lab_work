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


##############################################
##############################################
#Trump
##############################################
##############################################


## Webscrape data from links at http://transcripts.foreverdreaming.org/viewforum.php?f=738
url_home <- "https://justfacts.votesmart.org/candidate/public-statements/15723/donald-trump//?s=date&p="


num_pages <- read_html('https://justfacts.votesmart.org/candidate/public-statements/15723/donald-trump') %>%
  html_nodes(xpath = "//h7") %>% 
  html_text() %>%
  str_remove_all("Page 1 of ") %>%
  as.numeric(.) %>%
  .[1]


num_pages_search <- read_html('https://justfacts.votesmart.org/candidate/public-statements/15723/donald-trump?start=&end=&search=Russia') %>%
  html_nodes('.text-center:nth-child(1) h7') %>% 
  html_text() %>%
  str_remove_all("Page 1 of ") %>%
  as.numeric(.)

test <- lapply(paste0(url_home, 1:num_pages), #using only the first 2 pages so the server does not burn out (to get all docs change "2" to num_pages)
               function(url){
                 url %>% read_html() %>% 
                   html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "card-text", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//span | //td | //*[contains(concat( " ", @class, " " ), concat( " ", "card-title", " " ))]') %>%
                   map(function (node) {data.frame(politician = str_replace_all(url_home, "https://justfacts.votesmart.org/candidate/public-statements/\\d+/|[^[:alnum:]]|date|p=|s=", " ") %>%
                                                     trimws("both") %>%
                                                     rep(.,length.out = .),
                                                   office = node %>% 
                                                     html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "card-text", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//span') %>%
                                                     html_text(trim = TRUE) %>%
                                                     rep(.,length.out = .),
                                                   date = node %>% 
                                                     html_nodes(xpath = '//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
                                                     html_text(trim = TRUE) %>%
                                                     rep(.,length.out = .),
                                                   speech = node %>% 
                                                     html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "statements-table-data", " " ))]') %>%
                                                     html_text(trim = TRUE) %>%
                                                     rep(.,length.out = .),
                                                   speech_urls = node %>% 
                                                     html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "statements-table-data", " " ))]//a') %>%
                                                     html_attr("href") %>%
                                                     rep(.,length.out = .), 
                                                   stringsAsFactors=FALSE)}) %>%
                   bind_rows() %>%
                   distinct()
               })


trump_df <- plyr::ldply(test, data.frame) #install the "plyr" package but do not run it only call it like done so in this line (has conflicts with dplyr)

head(trump_df)

write_rds(trump_df, "trump_df_raw.rds")

trump_df$politician

#cleaning up df

trump_df$speech_urls[1]

trump_df$speech <- trump_df$speech[1:length(trump_df$speech)] %>%
  gsub("\\s+", " ", .) #Reduces to only single spaces/gets rid of extra blank spaces

trump_df$speech_urls <- trump_df$speech_urls[1:length(trump_df$speech_urls)] %>%
  gsub("[[:space:]]", "", .) #eliminates all blank spaces for urls

#getting speech text of each page

urls <- as.list(trump_df$speech_urls)

sites <- urls %>% map(purrr::safely(read_html, NA))

trump_sites <- lapply(sites, '[[', "result") 

trump_sites_df <- plyr::ldply(trump_sites, data.frame)

write_rds(trump_sites, "trump_sites.rds")

speech_text <- trump_sites %>% 
  map_chr(purrr::possibly(. %>% html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "span-20", " " ))]') %>%
                            html_text(trim = TRUE), NA))

location <- trump_sites %>% 
  map_chr(purrr::possibly(. %>% html_node(xpath = '//span[(((count(preceding-sibling::*) + 1) = 8) and parent::*)]') %>% 
                            html_text(trim = TRUE), NA))
time <- trump_sites %>%
  map_chr(purrr::possibly(. %>% html_node(xpath = '//p[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
                            html_text(trim = TRUE), NA))

speech_source_url <- trump_sites %>% 
  map_chr(purrr::possibly(. %>% html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "span-20", " " ))]//a') %>%
                            html_attr("href"), NA))

text_df <- data.frame("speech_text" = speech_text, 
                      "location" = location, 
                      "time" = time,
                      "speech_source_url" = speech_source_url, 
                      stringsAsFactors = FALSE)

write_rds(text_df, "trump_speeches_raw.rds")

dat <- data.frame(trump_df, text_df, stringsAsFactors = FALSE) %>%
  separate(office, into = c("office", "party"), sep = ",") %>%
  mutate(speech_type = str_extract_all(.$speech, "Tweet|Remarks|Message|Proclamation"),
         party = trimws(.$party, "both"),
         office = trimws(.$office, "both"),
         time = str_extract(.$time, "\\d{1,2}:\\d{2}"),
         time_period = str_extract(.$time, "A.M.|P.M."),
         time_zone = str_extract(.$time, "EST|PST|CST|MST")) %>%
  rename(speech_title = speech) %>%
  mutate_all(., ~na_if(., "character(0)")) %>%
  select(politician, office, party, location, date, time, time_period, time_zone, speech_urls, speech_type, speech_title, speech_text, speech_source_url)

dat %>%
  filter(!is.na(speech_type)) %>%
  nrow(.)

dat$speech_text <- dat$speech_text %>% #cleaning document text
  gsub("Source:\\s\\w.{1,}", "", .) %>% #removing "source" at the end of the text
  gsub("\\s+", " ", .) %>% #reduce to only single spaces
  gsub("[[:punct:][:blank:]]+", " ", .) %>% #removing punctuation
  str_to_lower(.) %>% # converting to lower case
  str_replace_all("\n","") %>% #removing all lines
  gsub("[[:digit:]]+", "", .) #removes numbers

write_rds(dat, "trump_dat.rds")
