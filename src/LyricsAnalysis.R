# Simon Burrows
# Senior thesis

library(tidytext)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tm)

# Reading in the CSVs to create dataframes
all_songs <- read.csv("~/Documents/CS_600/PunkAnalysis/src/data/all_lyrics.csv", header = TRUE)
early_songs <- read.csv("~/Documents/CS_600/PunkAnalysis/src/data/1994-2007_lyrics.csv", header = TRUE)
late_songs <- read.csv("~/Documents/CS_600/PunkAnalysis/src/data/2008-2019_lyrics.csv", header = TRUE)

# Getting just lyrics
all_lyrics <- all_songs[,5]
early_lyrics <- early_songs[,5]
late_lyrics <- late_songs[,5]

# Creating Dataframes for lyrics
all_lyrics_df <- tibble(song = 1:1438, text = all_lyrics)

# Getting each word, and each sentence
tidy_sentences <- all_lyrics_df %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

graph_count <- tidy_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

plot(graph_count)