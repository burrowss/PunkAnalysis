# Simon Burrows
# Senior thesis

library(tidytext)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tm)
library(stringr)
library(textdata)

# Reading in the CSVs to create dataframes
all_songs <- read_csv("~/Documents/CS_600/PunkAnalysis/src/data/all_lyrics.csv")

# Create dataframes for each year, get words from each year, run afinn on it, get average/count for each year, plot all 25 to show change
# run nrc on it for emotional, compare to tonal?
# other file for tonal analysis. plot year vs strength for each type

# Early songs

songs_1994 <- all_songs %>% filter(date < "1995-01-01")
songs_1995 <- all_songs %>% filter((date >= "1995-01-01") & (date < "1996-01-01"))
songs_1996 <- all_songs %>% filter((date >= "1996-01-01") & (date < "1997-01-01"))
songs_1997 <- all_songs %>% filter((date >= "1997-01-01") & (date < "1998-01-01"))
songs_1998 <- all_songs %>% filter((date >= "1998-01-01") & (date < "1999-01-01"))
songs_1999 <- all_songs %>% filter((date >= "1999-01-01") & (date < "2000-01-01"))
songs_2000 <- all_songs %>% filter((date >= "2000-01-01") & (date < "2001-01-01"))
songs_2001 <- all_songs %>% filter((date >= "2001-01-01") & (date < "2002-01-01"))
songs_2002 <- all_songs %>% filter((date >= "2002-01-01") & (date < "2003-01-01"))
songs_2003 <- all_songs %>% filter((date >= "2003-01-01") & (date < "2004-01-01"))
songs_2004 <- all_songs %>% filter((date >= "2004-01-01") & (date < "2005-01-01"))
songs_2005 <- all_songs %>% filter((date >= "2005-01-01") & (date < "2006-01-01"))
songs_2006 <- all_songs %>% filter((date >= "2006-01-01") & (date < "2007-01-01"))
songs_2007 <- all_songs %>% filter((date >= "2007-01-01") & (date < "2008-01-01"))

# Early range: 1995-2007

early_songs <- all_songs %>% filter(date < "2008-01-01")

# Late songs
songs_2008 <- all_songs %>% filter((date >= "2008-01-01") & (date < "2009-01-01"))
songs_2009 <- all_songs %>% filter((date >= "2009-01-01") & (date < "2010-01-01"))
songs_2010 <- all_songs %>% filter((date >= "2010-01-01") & (date < "2011-01-01"))
songs_2011 <- all_songs %>% filter((date >= "2011-01-01") & (date < "2012-01-01"))
songs_2012 <- all_songs %>% filter((date >= "2012-01-01") & (date < "2013-01-01"))
songs_2013 <- all_songs %>% filter((date >= "2013-01-01") & (date < "2014-01-01"))
songs_2014 <- all_songs %>% filter((date >= "2014-01-01") & (date < "2015-01-01"))
songs_2015 <- all_songs %>% filter((date >= "2015-01-01") & (date < "2016-01-01"))
songs_2016 <- all_songs %>% filter((date >= "2016-01-01") & (date < "2017-01-01"))
songs_2017 <- all_songs %>% filter((date >= "2017-01-01") & (date < "2018-01-01"))
songs_2018 <- all_songs %>% filter((date >= "2018-01-01") & (date < "2019-01-01"))
songs_2019 <- all_songs %>% filter((date > "2018-01-01"))

# Late range: 2008 - 2019
late_songs <- all_songs %>% filter((date >= "2008-01-01") & (date < "2019-01-01"))

# Creating yearly dataframes - Early
df_1994 <- tibble(line = 1:49, text = songs_1994$lyrics)
df_1995 <- tibble(line = 1:62, text = songs_1995$lyrics)
df_1996 <- tibble(line = 1:16, text = songs_1996$lyrics)
df_1997 <- tibble(line = 1:33, text = songs_1997$lyrics)
df_1998 <- tibble(line = 1:52, text = songs_1998$lyrics)
df_1999 <- tibble(line = 1:60, text = songs_1999$lyrics)
df_2000 <- tibble(line = 1:43, text = songs_2000$lyrics)
df_2001 <- tibble(line = 1:66, text = songs_2001$lyrics)
df_2002 <- tibble(line = 1:74, text = songs_2002$lyrics)
df_2003 <- tibble(line = 1:95, text = songs_2003$lyrics)
df_2004 <- tibble(line = 1:65, text = songs_2004$lyrics)
df_2005 <- tibble(line = 1:44, text = songs_2005$lyrics)
df_2006 <- tibble(line = 1:39, text = songs_2006$lyrics)
df_2007 <- tibble(line = 1:85, text = songs_2007$lyrics)

df_early_range <- tibble(line = 1:783, text = early_songs$lyrics)

# Later Years
df_2008 <- tibble(line = 1:41, text = songs_2008$lyrics)
df_2009 <- tibble(line = 1:40, text = songs_2009$lyrics)
df_2010 <- tibble(line = 1:46, text = songs_2010$lyrics)
df_2011 <- tibble(line = 1:51, text = songs_2011$lyrics)
df_2012 <- tibble(line = 1:52, text = songs_2012$lyrics)
df_2013 <- tibble(line = 1:70, text = songs_2013$lyrics)
df_2014 <- tibble(line = 1:67, text = songs_2014$lyrics)
df_2015 <- tibble(line = 1:54, text = songs_2015$lyrics)
df_2016 <- tibble(line = 1:132, text = songs_2016$lyrics)
df_2017 <- tibble(line = 1:72, text = songs_2017$lyrics)
df_2018 <- tibble(line = 1:107, text = songs_2018$lyrics)
df_2019 <- tibble(line = 1:165, text = songs_2019$lyrics)

df_late_range <- tibble(line = 1:790, text = late_songs$lyrics)


# Getting each word, and each sentence for 1994
tidy_sentences <- df_1994 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 1994

afinn_94 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 1995

tidy_sentences <- df_1995 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 1995

afinn_95 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 1996

tidy_sentences <- df_1996 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 1996

afinn_96 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 1997

tidy_sentences <- df_1997 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 1997

afinn_97 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 1998

tidy_sentences <- df_1998 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 1998

afinn_98 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 1999

tidy_sentences <- df_1999 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 1999

afinn_99 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 2000

tidy_sentences <- df_2000 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2000

afinn_00 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 2001

tidy_sentences <- df_2001 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2001

afinn_01 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 2002

tidy_sentences <- df_2002 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2002

afinn_02 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2003

tidy_sentences <- df_2003 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2003

afinn_03 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2004

tidy_sentences <- df_2004 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2004

afinn_04 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 2005

tidy_sentences <- df_2005 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2005

afinn_05 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2006

tidy_sentences <- df_2006 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2006

afinn_06 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2007

tidy_sentences <- df_2007 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2007

afinn_07 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 2008

tidy_sentences <- df_2008 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2008

afinn_08 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 2009

tidy_sentences <- df_2009 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2009

afinn_09 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2010

tidy_sentences <- df_2010 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2010

afinn_10 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 2011

tidy_sentences <- df_2011 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2011

afinn_11 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2012

tidy_sentences <- df_2012 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2012

afinn_12 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2013

tidy_sentences <- df_2013 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2013

afinn_13 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2014

tidy_sentences <- df_2014 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2014

afinn_14 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# Getting each word, and each sentence for 2015

tidy_sentences <- df_2015 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2015

afinn_15 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2016

tidy_sentences <- df_2016 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2016

afinn_16 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2017

tidy_sentences <- df_2017 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2017

afinn_17 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2018

tidy_sentences <- df_2018 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2018

afinn_18 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Getting each word, and each sentence for 2019

tidy_sentences <- df_2019 %>%
  unnest_tokens(sentence, text, token = "lines")

tidy_words <- tidy_sentences %>%
  unnest_tokens(word, sentence)

stop_words <- get_stopwords()

tidy_words <- tidy_words %>%
  anti_join(stop_words)

word_count <- tidy_words %>%
  count(word, sort = TRUE)

# Running afinn on words 2019

afinn_19 <- word_count %>% inner_join(get_sentiments("afinn")) %>% group_by(index = n %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


# First plot for afinn of all years

total_afinn <- bind_rows(afinn_94, afinn_95, afinn_96, afinn_97, afinn_98, afinn_99, afinn_00, afinn_01, afinn_02, afinn_03, afinn_04, afinn_05, afinn_06, afinn_07, afinn_08, afinn_09, afinn_10, afinn_11, afinn_12, afinn_13, afinn_14, afinn_15, afinn_16, afinn_17, afinn_18, afinn_19)

total_afinn <- filter(total_afinn, index == 0)

total_afinn = total_afinn[c(2)]

total_afinn$year <- c(1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)

total_afinn %>% ggplot(aes(year, sentiment)) + geom_col(show.legend = FALSE) + geom_smooth(method = "lm") 

# Significance test

afinn_sig <- lm(data = total_afinn, sentiment ~ year)
summary(afinn_sig) # p-value of 0.005, significant!

# Next: Bing for positive and negative word counts for both sections, positive and negative word clouds
