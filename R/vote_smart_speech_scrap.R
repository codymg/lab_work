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

url_home <- "https://justfacts.votesmart.org/candidate/public-statements/15723/donald-trump//?s=date&p="


num_pages <- read_html('https://justfacts.votesmart.org/candidate/public-statements/15723/donald-trump') %>% # establishing number of total pages of speeches
  html_nodes(xpath = "//h7") %>% #node to numbers
  html_text() %>% #extracting text
  str_remove_all("Page 1 of ") %>% #getting rid of preface
  as.numeric(.) %>% #classifying as numeric
  .[1] #taking only 1 number


num_pages_search <- read_html('https://justfacts.votesmart.org/candidate/public-statements/15723/donald-trump?start=&end=&search=Russia') %>% #can use the following link and its base to search speeches with specific terms
  html_nodes('.text-center:nth-child(1) h7') %>% 
  html_text() %>%
  str_remove_all("Page 1 of ") %>%
  as.numeric(.)

test <- lapply(paste0(url_home, 1:num_pages), #getting all pages, recommend using 5-6 instead of num_pages to be kind
               function(url){
                 url %>% read_html() %>% #running url to get html with the whole page
                   html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "card-text", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//span | //td | //*[contains(concat( " ", @class, " " ), concat( " ", "card-title", " " ))]') %>%
                   map(function (node) {data.frame(politician = str_replace_all(url_home, "https://justfacts.votesmart.org/candidate/public-statements/\\d+/|[^[:alnum:]]|date|p=|s=", " ") %>% #mapping data frame on output 
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

#cleaning up df

trump_df$speech <- trump_df$speech[1:length(trump_df$speech)] %>%
  gsub("\\s+", " ", .) #Reduces to only single spaces/gets rid of extra blank spaces

trump_df$speech_urls <- trump_df$speech_urls[1:length(trump_df$speech_urls)] %>%
  gsub("[[:space:]]", "", .) #eliminates all blank spaces for urls

#getting speech text of each page

urls <- as.list(trump_df$speech_urls) #creating list of urls to speech texts

sites <- urls %>% map(purrr::safely(read_html, NA)) #getting html of all speech texts (safely helps ensure the code runs through completion even in even of 404 or other errors)

trump_sites <- lapply(sites, '[[', "result") #extracting desired html from sits list

speech_text <- trump_sites %>% 
  map_chr(purrr::possibly(. %>% html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "span-20", " " ))]') %>%
                            html_text(trim = TRUE), NA)) #extracting text of speeches from larger html page (if found)

location <- trump_sites %>% 
  map_chr(purrr::possibly(. %>% html_node(xpath = '//span[(((count(preceding-sibling::*) + 1) = 8) and parent::*)]') %>% 
                            html_text(trim = TRUE), NA)) #extracting locations from larger html page (if found)
time <- trump_sites %>%
  map_chr(purrr::possibly(. %>% html_node(xpath = '//p[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
                            html_text(trim = TRUE), NA))  #extracting time from larger html page (if found)

speech_source_url <- trump_sites %>% 
  map_chr(purrr::possibly(. %>% html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "span-20", " " ))]//a') %>%
                            html_attr("href"), NA))  #extracting source of speech text from larger html page (if found)

text_df <- data.frame("speech_text" = speech_text, 
                      "location" = location, 
                      "time" = time,
                      "speech_source_url" = speech_source_url, 
                      stringsAsFactors = FALSE) #binding all variables in to one data frame

write_rds(text_df, "trump_speeches_raw.rds") #saving as raw data

dat <- data.frame(trump_df, text_df, stringsAsFactors = FALSE) %>% #merging speech text data with home page data scraped earlier
  separate(office, into = c("office", "party"), sep = ",") %>% #splitting office into office and political party
  mutate(speech_type = str_extract_all(.$speech, "Tweet|Remarks|Message|Proclamation"), #extracting various types of speeches from text
         party = trimws(.$party, "both"), #getting rid of spaces
         office = trimws(.$office, "both"), #getting rid of spaces
         time = str_extract(.$time, "\\d{1,2}:\\d{2}"), #extracting time of speeches
         time_period = str_extract(.$time, "A.M.|P.M."), #determining period of day
         time_zone = str_extract(.$time, "EST|PST|CST|MST")) %>% #determining time zones
  rename(speech_title = speech) %>%
  mutate_all(., ~na_if(., "character(0)")) %>% #replacing "character(0)" entries with NA
  select(politician, office, party, location, date, time, time_period, time_zone, speech_urls, 
         speech_type, speech_title, speech_text, speech_source_url) #reordering variables

dat %>%
  filter(!is.na(speech_type)) %>% #filter to only non-NA observations
  nrow(.) #getting count of non-NA observations

dat$speech_text <- dat$speech_text %>% #cleaning document text
  gsub("Source:\\s\\w.{1,}", "", .) %>% #removing "source" at the end of the text
  gsub("\\s+", " ", .) %>% #reduce to only single spaces
  gsub("[[:punct:][:blank:]]+", " ", .) %>% #removing punctuation
  str_to_lower(.) %>% # converting to lower case
  str_replace_all("\n","") %>% #removing all lines
  gsub("[[:digit:]]+", "", .) #removes numbers

write_rds(dat, "trump_dat.rds")
