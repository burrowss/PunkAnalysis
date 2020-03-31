# Simon Burrows
# Senior thesis

library(tidytext)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

# Reading in the CSVs to create dataframes
all_songs <- read_csv("~/Documents/CS_600/PunkAnalysis/src/data/all_lyrics.csv")

# Think about what to do for NAs, plot with 200 at bottom, going up to 1

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
songs_2019 <- all_songs %>% filter((date > "2019-01-01"))

# Late range: 2008 - 2019
late_songs <- all_songs %>% filter(date >= "2008-01-01")

# Creating yearly dataframes - Early
top_ranking_1994 <- tibble(rank = songs_1994$top_ranking)
top_ranking_1995 <- tibble(rank = songs_1995$top_ranking)
top_ranking_1996 <- tibble(rank = songs_1996$top_ranking)
top_ranking_1997 <- tibble(rank = songs_1997$top_ranking)
top_ranking_1998 <- tibble(rank = songs_1998$top_ranking)
top_ranking_1999 <- tibble(rank = songs_1999$top_ranking)
top_ranking_2000 <- tibble(rank = songs_2000$top_ranking)
top_ranking_2001 <- tibble(rank = songs_2001$top_ranking)
top_ranking_2002 <- tibble(rank = songs_2002$top_ranking)
top_ranking_2003 <- tibble(rank = songs_2003$top_ranking)
top_ranking_2004 <- tibble(rank = songs_2004$top_ranking)
top_ranking_2005 <- tibble(rank = songs_2005$top_ranking)
top_ranking_2006 <- tibble(rank = songs_2006$top_ranking)
top_ranking_2007 <- tibble(rank = songs_2007$top_ranking)

top_ranking_early_range <- tibble(rank = early_songs$top_ranking)

# Later Years
top_ranking_2008 <- tibble(rank = songs_2008$top_ranking)
top_ranking_2009 <- tibble(rank = songs_2009$top_ranking)
top_ranking_2010 <- tibble(rank = songs_2010$top_ranking)
top_ranking_2011 <- tibble(rank = songs_2011$top_ranking)
top_ranking_2012 <- tibble(rank = songs_2012$top_ranking)
top_ranking_2013 <- tibble(rank = songs_2013$top_ranking)
top_ranking_2014 <- tibble(rank = songs_2014$top_ranking)
top_ranking_2015 <- tibble(rank = songs_2015$top_ranking)
top_ranking_2016 <- tibble(rank = songs_2016$top_ranking)
top_ranking_2017 <- tibble(rank = songs_2017$top_ranking)
top_ranking_2018 <- tibble(rank = songs_2018$top_ranking)
top_ranking_2019 <- tibble(rank = songs_2019$top_ranking)

top_ranking_late_range <- tibble(rank = late_songs$top_ranking)


# Getting the average ranks for each year

avg_ranking_94 <- mean(top_ranking_1994$rank, na.rm = TRUE)
avg_ranking_95 <- mean(top_ranking_1995$rank, na.rm = TRUE)
avg_ranking_96 <- mean(top_ranking_1996$rank, na.rm = TRUE)
avg_ranking_97 <- mean(top_ranking_1997$rank, na.rm = TRUE)
avg_ranking_98 <- mean(top_ranking_1998$rank, na.rm = TRUE)
avg_ranking_99 <- mean(top_ranking_1999$rank, na.rm = TRUE)
avg_ranking_00 <- mean(top_ranking_2000$rank, na.rm = TRUE)
avg_ranking_01 <- mean(top_ranking_2001$rank, na.rm = TRUE)
avg_ranking_02 <- mean(top_ranking_2002$rank, na.rm = TRUE)
avg_ranking_03 <- mean(top_ranking_2003$rank, na.rm = TRUE)
avg_ranking_04 <- mean(top_ranking_2004$rank, na.rm = TRUE)
avg_ranking_05 <- mean(top_ranking_2005$rank, na.rm = TRUE)
avg_ranking_06 <- mean(top_ranking_2006$rank, na.rm = TRUE)
avg_ranking_07 <- mean(top_ranking_2007$rank, na.rm = TRUE)
avg_ranking_08 <- mean(top_ranking_2008$rank, na.rm = TRUE)
avg_ranking_09 <- mean(top_ranking_2009$rank, na.rm = TRUE)
avg_ranking_10 <- mean(top_ranking_2010$rank, na.rm = TRUE)
avg_ranking_11 <- mean(top_ranking_2011$rank, na.rm = TRUE)
avg_ranking_12 <- mean(top_ranking_2012$rank, na.rm = TRUE)
avg_ranking_13 <- mean(top_ranking_2013$rank, na.rm = TRUE)
avg_ranking_14 <- mean(top_ranking_2014$rank, na.rm = TRUE)
avg_ranking_15 <- mean(top_ranking_2015$rank, na.rm = TRUE)
avg_ranking_16 <- mean(top_ranking_2016$rank, na.rm = TRUE)
avg_ranking_17 <- mean(top_ranking_2017$rank, na.rm = TRUE)
avg_ranking_18 <- mean(top_ranking_2018$rank, na.rm = TRUE)
avg_ranking_19 <- mean(top_ranking_2019$rank, na.rm = TRUE)

# Standard Deviations
sd_ranking_94 <- sd(top_ranking_1994$rank, na.rm = TRUE)
sd_ranking_95 <- sd(top_ranking_1995$rank, na.rm = TRUE)
sd_ranking_96 <- sd(top_ranking_1996$rank, na.rm = TRUE)
sd_ranking_97 <- sd(top_ranking_1997$rank, na.rm = TRUE)
sd_ranking_98 <- sd(top_ranking_1998$rank, na.rm = TRUE)
sd_ranking_99 <- sd(top_ranking_1999$rank, na.rm = TRUE)
sd_ranking_00 <- sd(top_ranking_2000$rank, na.rm = TRUE)
sd_ranking_01 <- sd(top_ranking_2001$rank, na.rm = TRUE)
sd_ranking_02 <- sd(top_ranking_2002$rank, na.rm = TRUE)
sd_ranking_03 <- sd(top_ranking_2003$rank, na.rm = TRUE)
sd_ranking_04 <- sd(top_ranking_2004$rank, na.rm = TRUE)
sd_ranking_05 <- sd(top_ranking_2005$rank, na.rm = TRUE)
sd_ranking_06 <- sd(top_ranking_2006$rank, na.rm = TRUE)
sd_ranking_07 <- sd(top_ranking_2007$rank, na.rm = TRUE)
sd_ranking_08 <- sd(top_ranking_2008$rank, na.rm = TRUE)
sd_ranking_09 <- sd(top_ranking_2009$rank, na.rm = TRUE)
sd_ranking_10 <- sd(top_ranking_2010$rank, na.rm = TRUE)
sd_ranking_11 <- sd(top_ranking_2011$rank, na.rm = TRUE)
sd_ranking_12 <- sd(top_ranking_2012$rank, na.rm = TRUE)
sd_ranking_13 <- sd(top_ranking_2013$rank, na.rm = TRUE)
sd_ranking_14 <- sd(top_ranking_2014$rank, na.rm = TRUE)
sd_ranking_15 <- sd(top_ranking_2015$rank, na.rm = TRUE)
sd_ranking_16 <- sd(top_ranking_2016$rank, na.rm = TRUE)
sd_ranking_17 <- sd(top_ranking_2017$rank, na.rm = TRUE)
sd_ranking_18 <- sd(top_ranking_2018$rank, na.rm = TRUE)
sd_ranking_19 <- sd(top_ranking_2019$rank, na.rm = TRUE)

# Plot average ranks
year <- c(1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
rank <- c(avg_ranking_94, avg_ranking_95, avg_ranking_96, avg_ranking_97, avg_ranking_98, avg_ranking_99, avg_ranking_00, avg_ranking_01, avg_ranking_02, avg_ranking_03, avg_ranking_04, avg_ranking_05, avg_ranking_06, avg_ranking_07, avg_ranking_08, avg_ranking_09, avg_ranking_10, avg_ranking_11, avg_ranking_12, avg_ranking_13, avg_ranking_14, avg_ranking_15, avg_ranking_16, avg_ranking_17, avg_ranking_18, avg_ranking_19)
sd <- c(sd_ranking_94, sd_ranking_95, sd_ranking_96, sd_ranking_97, sd_ranking_98, sd_ranking_99, sd_ranking_00, sd_ranking_01, sd_ranking_02, sd_ranking_03, sd_ranking_04, sd_ranking_05, sd_ranking_06, sd_ranking_07, sd_ranking_08, sd_ranking_09, sd_ranking_10, sd_ranking_11, sd_ranking_12, sd_ranking_13, sd_ranking_14, sd_ranking_15, sd_ranking_16, sd_ranking_17, sd_ranking_18, sd_ranking_19)
total_ranks <- data.frame(rank, year, sd)

limits <- aes(ymax = rank + sd, ymin = rank - sd)

# What type of error is the lm?
total_ranks %>% ggplot(aes(x = year, y = rank)) +
  geom_point(colour = "steelblue3") +
  geom_errorbar(limits, width=.2,
                position=position_dodge(0.05), color ="steelblue3") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  geom_line(color="steelblue3")+
  ggtitle("Average Rankings")

rank_sig <- lm(data = total_ranks, rank ~ year)
summary(rank_sig) # p-value: 0.668014