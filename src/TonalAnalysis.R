# Simon Burrows
# Senior thesis

library(tidytext)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

# Reading in all_tone_data.csv to create a dataframe
all_tone <- read_csv("~/Documents/CS_600/PunkAnalysis/src/tone_resources/all_tone_data.csv")
all_tone <- all_tone %>% rename(tone1_score = "document_tone/tones/0/score", tone1 = "document_tone/tones/0/tone_id", tone2_score = "document_tone/tones/1/score", tone2 = "document_tone/tones/1/tone_id", tone3_score = "document_tone/tones/2/score", tone3 = "document_tone/tones/2/tone_id", tone4_score = "document_tone/tones/3/score", tone4 = "document_tone/tones/3/tone_id", tone5_score = "document_tone/tones/4/score", tone5 = "document_tone/tones/4/tone_id")
all_tone <- data.frame(all_tone)
all_tone <- all_tone[c(T,T,F)]

# Creating tone lists for each year based on lengths from LyricsAnalysis.R
# Slices are inclusive so add one after each year (1 to 49, then add 49 + 62 for the last row. In next slice start at 50 instead of 49)
tone_1994 <- all_tone %>% slice(1:49) # 49
tone_1995 <- all_tone %>% slice(50:111) # 62 
tone_1996 <- all_tone %>% slice(112:127) # 16
tone_1997 <- all_tone %>% slice(128:160) # 33
tone_1998 <- all_tone %>% slice(161:212) # 52
tone_1999 <- all_tone %>% slice(213:272) # 60
tone_2000 <- all_tone %>% slice(273:315) # 43
tone_2001 <- all_tone %>% slice(316:381) # 66
tone_2002 <- all_tone %>% slice(382:455) # 74
tone_2003 <- all_tone %>% slice(456:550) # 95
tone_2004 <- all_tone %>% slice(551:615) # 65
tone_2005 <- all_tone %>% slice(616:659) # 44
tone_2006 <- all_tone %>% slice(660:698) # 39
tone_2007 <- all_tone %>% slice(699:783) # 85
tone_2008 <- all_tone %>% slice(784:824) # 41
tone_2009 <- all_tone %>% slice(825:864) # 40
tone_2010 <- all_tone %>% slice(865:910) # 46
tone_2011 <- all_tone %>% slice(911:961) # 51
tone_2012 <- all_tone %>% slice(962:1013) # 52
tone_2013 <- all_tone %>% slice(1014:1083) # 70
tone_2014 <- all_tone %>% slice(1084:1150) # 67
tone_2015 <- all_tone %>% slice(1151:1204) # 54
tone_2016 <- all_tone %>% slice(1205:1336) # 132
tone_2017 <- all_tone %>% slice(1337:1408) # 72
tone_2018 <- all_tone %>% slice(1409:1515) # 107
tone_2019 <- all_tone %>% slice(1516:1573) # 58

# Creating tone dataframes
# Get: anger, fear, joy, sadness, analytical, confident, tentative

# 1994
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_1994 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_94 <- tone_1994[as.matrix(anger)]
anger_94 <- data.frame(anger_94)
# Gets the average for the year
avg_anger_94 <- mean(as.numeric(as.character(anger_94$anger_94)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_94 <- sd(as.numeric(as.character(unlist(anger_94))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_1994 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_94 <- tone_1994[as.matrix(fear)]
fear_94 <- data.frame(fear_94)
# Gets the average for the year
avg_fear_94 <- mean(as.numeric(as.character(fear_94$fear_94)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_94 <- sd(as.numeric(as.character(unlist(fear_94))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_1994 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_94 <- tone_1994[as.matrix(joy)]
joy_94 <- data.frame(joy_94)
# Gets the average for the year
avg_joy_94 <- mean(as.numeric(as.character(joy_94$joy_94)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_94 <- sd(as.numeric(as.character(unlist(joy_94))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_1994 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_94 <- tone_1994[as.matrix(sadness)]
sadness_94 <- data.frame(sadness_94)
# Gets the average for the year
avg_sadness_94 <- mean(as.numeric(as.character(sadness_94$sadness_94)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_94 <- sd(as.numeric(as.character(unlist(sadness_94))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_1994 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_94 <- tone_1994[as.matrix(analytical)]
analytical_94 <- data.frame(analytical_94)
# Gets the average for the year
avg_analytical_94 <- mean(as.numeric(as.character(analytical_94$analytical_94)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_94 <- sd(as.numeric(as.character(unlist(analytical_94))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_1994 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_94 <- tone_1994[as.matrix(confident)]
confident_94 <- data.frame(confident_94)
# Gets the average for the year
avg_confident_94 <- mean(as.numeric(as.character(confident_94$confident_94)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_94 <- sd(as.numeric(as.character(unlist(confident_94))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_1994 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_94 <- tone_1994[as.matrix(tentative)]
tentative_94 <- data.frame(tentative_94)
# Gets the average for the year
avg_tentative_94 <- mean(as.numeric(as.character(tentative_94$tentative_94)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_94 <- sd(as.numeric(as.character(unlist(tentative_94))), na.rm = TRUE)


# 1995
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_1995 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_95 <- tone_1995[as.matrix(anger)]
anger_95 <- data.frame(anger_95)
# Gets the average for the year
avg_anger_95 <- mean(as.numeric(as.character(anger_95$anger_95)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_95 <- sd(as.numeric(as.character(unlist(anger_95))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_1995 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_95 <- tone_1995[as.matrix(fear)]
fear_95 <- data.frame(fear_95)
# Gets the average for the year
avg_fear_95 <- mean(as.numeric(as.character(fear_95$fear_95)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_95 <- sd(as.numeric(as.character(unlist(fear_95))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_1995 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_95 <- tone_1995[as.matrix(joy)]
joy_95 <- data.frame(joy_95)
# Gets the average for the year
avg_joy_95 <- mean(as.numeric(as.character(joy_95$joy_95)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_95 <- sd(as.numeric(as.character(unlist(joy_95))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_1995 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_95 <- tone_1995[as.matrix(sadness)]
sadness_95 <- data.frame(sadness_95)
# Gets the average for the year
avg_sadness_95 <- mean(as.numeric(as.character(sadness_95$sadness_95)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_95 <- sd(as.numeric(as.character(unlist(sadness_95))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_1995 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_95 <- tone_1995[as.matrix(analytical)]
analytical_95 <- data.frame(analytical_95)
# Gets the average for the year
avg_analytical_95 <- mean(as.numeric(as.character(analytical_95$analytical_95)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_95 <- sd(as.numeric(as.character(unlist(analytical_95))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_1995 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_95 <- tone_1995[as.matrix(confident)]
confident_95 <- data.frame(confident_95)
# Gets the average for the year
avg_confident_95 <- mean(as.numeric(as.character(confident_95$confident_95)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_95 <- sd(as.numeric(as.character(unlist(confident_95))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_1995 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_95 <- tone_1995[as.matrix(tentative)]
tentative_95 <- data.frame(tentative_95)
# Gets the average for the year
avg_tentative_95 <- mean(as.numeric(as.character(tentative_95$tentative_95)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_95 <- sd(as.numeric(as.character(unlist(tentative_95))), na.rm = TRUE)

# 1996
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_1996 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_96 <- tone_1996[as.matrix(anger)]
anger_96 <- data.frame(anger_96)
# Gets the average for the year
avg_anger_96 <- mean(as.numeric(as.character(anger_96$anger_96)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_96 <- sd(as.numeric(as.character(unlist(anger_96))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_1996 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_96 <- tone_1996[as.matrix(fear)]
fear_96 <- data.frame(fear_96)
# Gets the average for the year
avg_fear_96 <- mean(as.numeric(as.character(fear_96$fear_96)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_96 <- sd(as.numeric(as.character(unlist(fear_96))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_1996 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_96 <- tone_1996[as.matrix(joy)]
joy_96 <- data.frame(joy_96)
# Gets the average for the year
avg_joy_96 <- mean(as.numeric(as.character(joy_96$joy_96)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_96 <- sd(as.numeric(as.character(unlist(joy_96))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_1996 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_96 <- tone_1996[as.matrix(sadness)]
sadness_96 <- data.frame(sadness_96)
# Gets the average for the year
avg_sadness_96 <- mean(as.numeric(as.character(sadness_96$sadness_96)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_96 <- sd(as.numeric(as.character(unlist(sadness_96))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_1996 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_96 <- tone_1996[as.matrix(analytical)]
analytical_96 <- data.frame(analytical_96)
# Gets the average for the year
avg_analytical_96 <- mean(as.numeric(as.character(analytical_96$analytical_96)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_96 <- sd(as.numeric(as.character(unlist(analytical_96))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_1996 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_96 <- tone_1996[as.matrix(confident)]
confident_96 <- data.frame(confident_96)
# Gets the average for the year
avg_confident_96 <- mean(as.numeric(as.character(confident_96$confident_96)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_96 <- sd(as.numeric(as.character(unlist(confident_96))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_1996 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_96 <- tone_1996[as.matrix(tentative)]
tentative_96 <- data.frame(tentative_96)
# Gets the average for the year
avg_tentative_96 <- mean(as.numeric(as.character(tentative_96$tentative_96)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_96 <- sd(as.numeric(as.character(unlist(tentative_96))), na.rm = TRUE)


# 1997
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_1997 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_97 <- tone_1997[as.matrix(anger)]
anger_97 <- data.frame(anger_97)
# Gets the average for the year
avg_anger_97 <- mean(as.numeric(as.character(anger_97$anger_97)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_97 <- sd(as.numeric(as.character(unlist(anger_97))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_1997 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_97 <- tone_1997[as.matrix(fear)]
fear_97 <- data.frame(fear_97)
# Gets the average for the year
avg_fear_97 <- mean(as.numeric(as.character(fear_97$fear_97)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_97 <- sd(as.numeric(as.character(unlist(fear_97))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_1997 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_97 <- tone_1997[as.matrix(joy)]
joy_97 <- data.frame(joy_97)
# Gets the average for the year
avg_joy_97 <- mean(as.numeric(as.character(joy_97$joy_97)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_97 <- sd(as.numeric(as.character(unlist(joy_97))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_1997 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_97 <- tone_1997[as.matrix(sadness)]
sadness_97 <- data.frame(sadness_97)
# Gets the average for the year
avg_sadness_97 <- mean(as.numeric(as.character(sadness_97$sadness_97)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_97 <- sd(as.numeric(as.character(unlist(sadness_97))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_1997 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_97 <- tone_1997[as.matrix(analytical)]
analytical_97 <- data.frame(analytical_97)
# Gets the average for the year
avg_analytical_97 <- mean(as.numeric(as.character(analytical_97$analytical_97)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_97 <- sd(as.numeric(as.character(unlist(analytical_97))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_1997 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_97 <- tone_1997[as.matrix(confident)]
confident_97 <- data.frame(confident_97)
# Gets the average for the year
avg_confident_97 <- mean(as.numeric(as.character(confident_97$confident_97)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_97 <- sd(as.numeric(as.character(unlist(confident_97))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_1997 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_97 <- tone_1997[as.matrix(tentative)]
tentative_97 <- data.frame(tentative_97)
# Gets the average for the year
avg_tentative_97 <- mean(as.numeric(as.character(tentative_97$tentative_97)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_97 <- sd(as.numeric(as.character(unlist(tentative_97))), na.rm = TRUE)


# 1998
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_1998 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_98 <- tone_1998[as.matrix(anger)]
anger_98 <- data.frame(anger_98)
# Gets the average for the year
avg_anger_98 <- mean(as.numeric(as.character(anger_98$anger_98)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_98 <- sd(as.numeric(as.character(unlist(anger_98))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_1998 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_98 <- tone_1998[as.matrix(fear)]
fear_98 <- data.frame(fear_98)
# Gets the average for the year
avg_fear_98 <- mean(as.numeric(as.character(fear_98$fear_98)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_98 <- sd(as.numeric(as.character(unlist(fear_98))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_1998 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_98 <- tone_1998[as.matrix(joy)]
joy_98 <- data.frame(joy_98)
# Gets the average for the year
avg_joy_98 <- mean(as.numeric(as.character(joy_98$joy_98)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_98 <- sd(as.numeric(as.character(unlist(joy_98))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_1998 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_98 <- tone_1998[as.matrix(sadness)]
sadness_98 <- data.frame(sadness_98)
# Gets the average for the year
avg_sadness_98 <- mean(as.numeric(as.character(sadness_98$sadness_98)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_98 <- sd(as.numeric(as.character(unlist(sadness_98))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_1998 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_98 <- tone_1998[as.matrix(analytical)]
analytical_98 <- data.frame(analytical_98)
# Gets the average for the year
avg_analytical_98 <- mean(as.numeric(as.character(analytical_98$analytical_98)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_98 <- sd(as.numeric(as.character(unlist(analytical_98))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_1998 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_98 <- tone_1998[as.matrix(confident)]
confident_98 <- data.frame(confident_98)
# Gets the average for the year
avg_confident_98 <- mean(as.numeric(as.character(confident_98$confident_98)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_98 <- sd(as.numeric(as.character(unlist(confident_98))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_1998 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_98 <- tone_1998[as.matrix(tentative)]
tentative_98 <- data.frame(tentative_98)
# Gets the average for the year
avg_tentative_98 <- mean(as.numeric(as.character(tentative_98$tentative_98)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_98 <- sd(as.numeric(as.character(unlist(tentative_98))), na.rm = TRUE)


# 1999
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_1999 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_99 <- tone_1999[as.matrix(anger)]
anger_99 <- data.frame(anger_99)
# Gets the average for the year
avg_anger_99 <- mean(as.numeric(as.character(anger_99$anger_99)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_99 <- sd(as.numeric(as.character(unlist(anger_99))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_1999 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_99 <- tone_1999[as.matrix(fear)]
fear_99 <- data.frame(fear_99)
# Gets the average for the year
avg_fear_99 <- mean(as.numeric(as.character(fear_99$fear_99)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_99 <- sd(as.numeric(as.character(unlist(fear_99))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_1999 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_99 <- tone_1999[as.matrix(joy)]
joy_99 <- data.frame(joy_99)
# Gets the average for the year
avg_joy_99 <- mean(as.numeric(as.character(joy_99$joy_99)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_99 <- sd(as.numeric(as.character(unlist(joy_99))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_1999 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_99 <- tone_1999[as.matrix(sadness)]
sadness_99 <- data.frame(sadness_99)
# Gets the average for the year
avg_sadness_99 <- mean(as.numeric(as.character(sadness_99$sadness_99)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_99 <- sd(as.numeric(as.character(unlist(sadness_99))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_1999 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_99 <- tone_1999[as.matrix(analytical)]
analytical_99 <- data.frame(analytical_99)
# Gets the average for the year
avg_analytical_99 <- mean(as.numeric(as.character(analytical_99$analytical_99)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_99 <- sd(as.numeric(as.character(unlist(analytical_99))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_1999 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_99 <- tone_1999[as.matrix(confident)]
confident_99 <- data.frame(confident_99)
# Gets the average for the year
avg_confident_99 <- mean(as.numeric(as.character(confident_99$confident_99)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_99 <- sd(as.numeric(as.character(unlist(confident_99))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_1999 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_99 <- tone_1999[as.matrix(tentative)]
tentative_99 <- data.frame(tentative_99)
# Gets the average for the year
avg_tentative_99 <- mean(as.numeric(as.character(tentative_99$tentative_99)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_99 <- sd(as.numeric(as.character(unlist(tentative_99))), na.rm = TRUE)


# 2000
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2000 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_00 <- tone_2000[as.matrix(anger)]
anger_00 <- data.frame(anger_00)
# Gets the average for the year
avg_anger_00 <- mean(as.numeric(as.character(anger_00$anger_00)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_00 <- sd(as.numeric(as.character(unlist(anger_00))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2000 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_00 <- tone_2000[as.matrix(fear)]
fear_00 <- data.frame(fear_00)
# Gets the average for the year
avg_fear_00 <- mean(as.numeric(as.character(fear_00$fear_00)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_00 <- sd(as.numeric(as.character(unlist(fear_00))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2000 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_00 <- tone_2000[as.matrix(joy)]
joy_00 <- data.frame(joy_00)
# Gets the average for the year
avg_joy_00 <- mean(as.numeric(as.character(joy_00$joy_00)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_00 <- sd(as.numeric(as.character(unlist(joy_00))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2000 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_00 <- tone_2000[as.matrix(sadness)]
sadness_00 <- data.frame(sadness_00)
# Gets the average for the year
avg_sadness_00 <- mean(as.numeric(as.character(sadness_00$sadness_00)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_00 <- sd(as.numeric(as.character(unlist(sadness_00))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2000 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_00 <- tone_2000[as.matrix(analytical)]
analytical_00 <- data.frame(analytical_00)
# Gets the average for the year
avg_analytical_00 <- mean(as.numeric(as.character(analytical_00$analytical_00)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_00 <- sd(as.numeric(as.character(unlist(analytical_00))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2000 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_00 <- tone_2000[as.matrix(confident)]
confident_00 <- data.frame(confident_00)
# Gets the average for the year
avg_confident_00 <- mean(as.numeric(as.character(confident_00$confident_00)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_00 <- sd(as.numeric(as.character(unlist(confident_00))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2000 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_00 <- tone_2000[as.matrix(tentative)]
tentative_00 <- data.frame(tentative_00)
# Gets the average for the year
avg_tentative_00 <- mean(as.numeric(as.character(tentative_00$tentative_00)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_00 <- sd(as.numeric(as.character(unlist(tentative_00))), na.rm = TRUE)


# 2001
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2001 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_01 <- tone_2001[as.matrix(anger)]
anger_01 <- data.frame(anger_01)
# Gets the average for the year
avg_anger_01 <- mean(as.numeric(as.character(anger_01$anger_01)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_01 <- sd(as.numeric(as.character(unlist(anger_01))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2001 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_01 <- tone_2001[as.matrix(fear)]
fear_01 <- data.frame(fear_01)
# Gets the average for the year
avg_fear_01 <- mean(as.numeric(as.character(fear_01$fear_01)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_01 <- sd(as.numeric(as.character(unlist(fear_01))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2001 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_01 <- tone_2001[as.matrix(joy)]
joy_01 <- data.frame(joy_01)
# Gets the average for the year
avg_joy_01 <- mean(as.numeric(as.character(joy_01$joy_01)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_01 <- sd(as.numeric(as.character(unlist(joy_01))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2001 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_01 <- tone_2001[as.matrix(sadness)]
sadness_01 <- data.frame(sadness_01)
# Gets the average for the year
avg_sadness_01 <- mean(as.numeric(as.character(sadness_01$sadness_01)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_01 <- sd(as.numeric(as.character(unlist(sadness_01))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2001 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_01 <- tone_2001[as.matrix(analytical)]
analytical_01 <- data.frame(analytical_01)
# Gets the average for the year
avg_analytical_01 <- mean(as.numeric(as.character(analytical_01$analytical_01)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_01 <- sd(as.numeric(as.character(unlist(analytical_01))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2001 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_01 <- tone_2001[as.matrix(confident)]
confident_01 <- data.frame(confident_01)
# Gets the average for the year
avg_confident_01 <- mean(as.numeric(as.character(confident_01$confident_01)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_01 <- sd(as.numeric(as.character(unlist(confident_01))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2001 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_01 <- tone_2001[as.matrix(tentative)]
tentative_01 <- data.frame(tentative_01)
# Gets the average for the year
avg_tentative_01 <- mean(as.numeric(as.character(tentative_01$tentative_01)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_01 <- sd(as.numeric(as.character(unlist(tentative_01))), na.rm = TRUE)


# 2002
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2002 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_02 <- tone_2002[as.matrix(anger)]
anger_02 <- data.frame(anger_02)
# Gets the average for the year
avg_anger_02 <- mean(as.numeric(as.character(anger_02$anger_02)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_02 <- sd(as.numeric(as.character(unlist(anger_02))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2002 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_02 <- tone_2002[as.matrix(fear)]
fear_02 <- data.frame(fear_02)
# Gets the average for the year
avg_fear_02 <- mean(as.numeric(as.character(fear_02$fear_02)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_02 <- sd(as.numeric(as.character(unlist(fear_02))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2002 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_02 <- tone_2002[as.matrix(joy)]
joy_02 <- data.frame(joy_02)
# Gets the average for the year
avg_joy_02 <- mean(as.numeric(as.character(joy_02$joy_02)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_02 <- sd(as.numeric(as.character(unlist(joy_02))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2002 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_02 <- tone_2002[as.matrix(sadness)]
sadness_02 <- data.frame(sadness_02)
# Gets the average for the year
avg_sadness_02 <- mean(as.numeric(as.character(sadness_02$sadness_02)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_02 <- sd(as.numeric(as.character(unlist(sadness_02))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2002 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_02 <- tone_2002[as.matrix(analytical)]
analytical_02 <- data.frame(analytical_02)
# Gets the average for the year
avg_analytical_02 <- mean(as.numeric(as.character(analytical_02$analytical_02)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_02 <- sd(as.numeric(as.character(unlist(analytical_02))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2002 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_02 <- tone_2002[as.matrix(confident)]
confident_02 <- data.frame(confident_02)
# Gets the average for the year
avg_confident_02 <- mean(as.numeric(as.character(confident_02$confident_02)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_02 <- sd(as.numeric(as.character(unlist(confident_02))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2002 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_02 <- tone_2002[as.matrix(tentative)]
tentative_02 <- data.frame(tentative_02)
# Gets the average for the year
avg_tentative_02 <- mean(as.numeric(as.character(tentative_02$tentative_02)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_02 <- sd(as.numeric(as.character(unlist(tentative_02))), na.rm = TRUE)


# 2003
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2003 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_03 <- tone_2003[as.matrix(anger)]
anger_03 <- data.frame(anger_03)
# Gets the average for the year
avg_anger_03 <- mean(as.numeric(as.character(anger_03$anger_03)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_03 <- sd(as.numeric(as.character(unlist(anger_03))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2003 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_03 <- tone_2003[as.matrix(fear)]
fear_03 <- data.frame(fear_03)
# Gets the average for the year
avg_fear_03 <- mean(as.numeric(as.character(fear_03$fear_03)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_03 <- sd(as.numeric(as.character(unlist(fear_03))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2003 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_03 <- tone_2003[as.matrix(joy)]
joy_03 <- data.frame(joy_03)
# Gets the average for the year
avg_joy_03 <- mean(as.numeric(as.character(joy_03$joy_03)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_03 <- sd(as.numeric(as.character(unlist(joy_03))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2003 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_03 <- tone_2003[as.matrix(sadness)]
sadness_03 <- data.frame(sadness_03)
# Gets the average for the year
avg_sadness_03 <- mean(as.numeric(as.character(sadness_03$sadness_03)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_03 <- sd(as.numeric(as.character(unlist(sadness_03))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2003 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_03 <- tone_2003[as.matrix(analytical)]
analytical_03 <- data.frame(analytical_03)
# Gets the average for the year
avg_analytical_03 <- mean(as.numeric(as.character(analytical_03$analytical_03)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_03 <- sd(as.numeric(as.character(unlist(analytical_03))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2003 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_03 <- tone_2003[as.matrix(confident)]
confident_03 <- data.frame(confident_03)
# Gets the average for the year
avg_confident_03 <- mean(as.numeric(as.character(confident_03$confident_03)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_03 <- sd(as.numeric(as.character(unlist(confident_03))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2003 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_03 <- tone_2003[as.matrix(tentative)]
tentative_03 <- data.frame(tentative_03)
# Gets the average for the year
avg_tentative_03 <- mean(as.numeric(as.character(tentative_03$tentative_03)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_03 <- sd(as.numeric(as.character(unlist(tentative_03))), na.rm = TRUE)


# 2004
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2004 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_04 <- tone_2004[as.matrix(anger)]
anger_04 <- data.frame(anger_04)
# Gets the average for the year
avg_anger_04 <- mean(as.numeric(as.character(anger_04$anger_04)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_04 <- sd(as.numeric(as.character(unlist(anger_04))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2004 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_04 <- tone_2004[as.matrix(fear)]
fear_04 <- data.frame(fear_04)
# Gets the average for the year
avg_fear_04 <- mean(as.numeric(as.character(fear_04$fear_04)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_04 <- sd(as.numeric(as.character(unlist(fear_04))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2004 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_04 <- tone_2004[as.matrix(joy)]
joy_04 <- data.frame(joy_04)
# Gets the average for the year
avg_joy_04 <- mean(as.numeric(as.character(joy_04$joy_04)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_04 <- sd(as.numeric(as.character(unlist(joy_04))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2004 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_04 <- tone_2004[as.matrix(sadness)]
sadness_04 <- data.frame(sadness_04)
# Gets the average for the year
avg_sadness_04 <- mean(as.numeric(as.character(sadness_04$sadness_04)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_04 <- sd(as.numeric(as.character(unlist(sadness_04))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2004 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_04 <- tone_2004[as.matrix(analytical)]
analytical_04 <- data.frame(analytical_04)
# Gets the average for the year
avg_analytical_04 <- mean(as.numeric(as.character(analytical_04$analytical_04)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_04 <- sd(as.numeric(as.character(unlist(analytical_04))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2004 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_04 <- tone_2004[as.matrix(confident)]
confident_04 <- data.frame(confident_04)
# Gets the average for the year
avg_confident_04 <- mean(as.numeric(as.character(confident_04$confident_04)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_04 <- sd(as.numeric(as.character(unlist(confident_04))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2004 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_04 <- tone_2004[as.matrix(tentative)]
tentative_04 <- data.frame(tentative_04)
# Gets the average for the year
avg_tentative_04 <- mean(as.numeric(as.character(tentative_04$tentative_04)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_04 <- sd(as.numeric(as.character(unlist(tentative_04))), na.rm = TRUE)


# 2005
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2005 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_05 <- tone_2005[as.matrix(anger)]
anger_05 <- data.frame(anger_05)
# Gets the average for the year
avg_anger_05 <- mean(as.numeric(as.character(anger_05$anger_05)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_05 <- sd(as.numeric(as.character(unlist(anger_05))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2005 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_05 <- tone_2005[as.matrix(fear)]
fear_05 <- data.frame(fear_05)
# Gets the average for the year
avg_fear_05 <- mean(as.numeric(as.character(fear_05$fear_05)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_05 <- sd(as.numeric(as.character(unlist(fear_05))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2005 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_05 <- tone_2005[as.matrix(joy)]
joy_05 <- data.frame(joy_05)
# Gets the average for the year
avg_joy_05 <- mean(as.numeric(as.character(joy_05$joy_05)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_05 <- sd(as.numeric(as.character(unlist(joy_05))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2005 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_05 <- tone_2005[as.matrix(sadness)]
sadness_05 <- data.frame(sadness_05)
# Gets the average for the year
avg_sadness_05 <- mean(as.numeric(as.character(sadness_05$sadness_05)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_05 <- sd(as.numeric(as.character(unlist(sadness_05))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2005 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_05 <- tone_2005[as.matrix(analytical)]
analytical_05 <- data.frame(analytical_05)
# Gets the average for the year
avg_analytical_05 <- mean(as.numeric(as.character(analytical_05$analytical_05)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_05 <- sd(as.numeric(as.character(unlist(analytical_05))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2005 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_05 <- tone_2005[as.matrix(confident)]
confident_05 <- data.frame(confident_05)
# Gets the average for the year
avg_confident_05 <- mean(as.numeric(as.character(confident_05$confident_05)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_05 <- sd(as.numeric(as.character(unlist(confident_05))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2005 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_05 <- tone_2005[as.matrix(tentative)]
tentative_05 <- data.frame(tentative_05)
# Gets the average for the year
avg_tentative_05 <- mean(as.numeric(as.character(tentative_05$tentative_05)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_05 <- sd(as.numeric(as.character(unlist(tentative_05))), na.rm = TRUE)


# 2006
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2006 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_06 <- tone_2006[as.matrix(anger)]
anger_06 <- data.frame(anger_06)
# Gets the average for the year
avg_anger_06 <- mean(as.numeric(as.character(anger_06$anger_06)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_06 <- sd(as.numeric(as.character(unlist(anger_06))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2006 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_06 <- tone_2006[as.matrix(fear)]
fear_06 <- data.frame(fear_06)
# Gets the average for the year
avg_fear_06 <- mean(as.numeric(as.character(fear_06$fear_06)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_06 <- sd(as.numeric(as.character(unlist(fear_06))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2006 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_06 <- tone_2006[as.matrix(joy)]
joy_06 <- data.frame(joy_06)
# Gets the average for the year
avg_joy_06 <- mean(as.numeric(as.character(joy_06$joy_06)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_06 <- sd(as.numeric(as.character(unlist(joy_06))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2006 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_06 <- tone_2006[as.matrix(sadness)]
sadness_06 <- data.frame(sadness_06)
# Gets the average for the year
avg_sadness_06 <- mean(as.numeric(as.character(sadness_06$sadness_06)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_06 <- sd(as.numeric(as.character(unlist(sadness_06))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2006 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_06 <- tone_2006[as.matrix(analytical)]
analytical_06 <- data.frame(analytical_06)
# Gets the average for the year
avg_analytical_06 <- mean(as.numeric(as.character(analytical_06$analytical_06)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_06 <- sd(as.numeric(as.character(unlist(analytical_06))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2006 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_06 <- tone_2006[as.matrix(confident)]
confident_06 <- data.frame(confident_06)
# Gets the average for the year
avg_confident_06 <- mean(as.numeric(as.character(confident_06$confident_06)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_06 <- sd(as.numeric(as.character(unlist(confident_06))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2006 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_06 <- tone_2006[as.matrix(tentative)]
tentative_06 <- data.frame(tentative_06)
# Gets the average for the year
avg_tentative_06 <- mean(as.numeric(as.character(tentative_06$tentative_06)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_06 <- sd(as.numeric(as.character(unlist(tentative_06))), na.rm = TRUE)

# 2007
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2007 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_07 <- tone_2007[as.matrix(anger)]
anger_07 <- data.frame(anger_07)
# Gets the average for the year
avg_anger_07 <- mean(as.numeric(as.character(anger_07$anger_07)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_07 <- sd(as.numeric(as.character(unlist(anger_07))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2007 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_07 <- tone_2007[as.matrix(fear)]
fear_07 <- data.frame(fear_07)
# Gets the average for the year
avg_fear_07 <- mean(as.numeric(as.character(fear_07$fear_07)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_07 <- sd(as.numeric(as.character(unlist(fear_07))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2007 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_07 <- tone_2007[as.matrix(joy)]
joy_07 <- data.frame(joy_07)
# Gets the average for the year
avg_joy_07 <- mean(as.numeric(as.character(joy_07$joy_07)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_07 <- sd(as.numeric(as.character(unlist(joy_07))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2007 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_07 <- tone_2007[as.matrix(sadness)]
sadness_07 <- data.frame(sadness_07)
# Gets the average for the year
avg_sadness_07 <- mean(as.numeric(as.character(sadness_07$sadness_07)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_07 <- sd(as.numeric(as.character(unlist(sadness_07))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2007 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_07 <- tone_2007[as.matrix(analytical)]
analytical_07 <- data.frame(analytical_07)
# Gets the average for the year
avg_analytical_07 <- mean(as.numeric(as.character(analytical_07$analytical_07)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_07 <- sd(as.numeric(as.character(unlist(analytical_07))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2007 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_07 <- tone_2007[as.matrix(confident)]
confident_07 <- data.frame(confident_07)
# Gets the average for the year
avg_confident_07 <- mean(as.numeric(as.character(confident_07$confident_07)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_07 <- sd(as.numeric(as.character(unlist(confident_07))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2007 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_07 <- tone_2007[as.matrix(tentative)]
tentative_07 <- data.frame(tentative_07)
# Gets the average for the year
avg_tentative_07 <- mean(as.numeric(as.character(tentative_07$tentative_07)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_07 <- sd(as.numeric(as.character(unlist(tentative_07))), na.rm = TRUE)


# 2008
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2008 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_08 <- tone_2008[as.matrix(anger)]
anger_08 <- data.frame(anger_08)
# Gets the average for the year
avg_anger_08 <- mean(as.numeric(as.character(anger_08$anger_08)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_08 <- sd(as.numeric(as.character(unlist(anger_08))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2008 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_08 <- tone_2008[as.matrix(fear)]
fear_08 <- data.frame(fear_08)
# Gets the average for the year
avg_fear_08 <- mean(as.numeric(as.character(fear_08$fear_08)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_08 <- sd(as.numeric(as.character(unlist(fear_08))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2008 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_08 <- tone_2008[as.matrix(joy)]
joy_08 <- data.frame(joy_08)
# Gets the average for the year
avg_joy_08 <- mean(as.numeric(as.character(joy_08$joy_08)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_08 <- sd(as.numeric(as.character(unlist(joy_08))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2008 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_08 <- tone_2008[as.matrix(sadness)]
sadness_08 <- data.frame(sadness_08)
# Gets the average for the year
avg_sadness_08 <- mean(as.numeric(as.character(sadness_08$sadness_08)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_08 <- sd(as.numeric(as.character(unlist(sadness_08))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2008 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_08 <- tone_2008[as.matrix(analytical)]
analytical_08 <- data.frame(analytical_08)
# Gets the average for the year
avg_analytical_08 <- mean(as.numeric(as.character(analytical_08$analytical_08)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_08 <- sd(as.numeric(as.character(unlist(analytical_08))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2008 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_08 <- tone_2008[as.matrix(confident)]
confident_08 <- data.frame(confident_08)
# Gets the average for the year
avg_confident_08 <- mean(as.numeric(as.character(confident_08$confident_08)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_08 <- sd(as.numeric(as.character(unlist(confident_08))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2008 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_08 <- tone_2008[as.matrix(tentative)]
tentative_08 <- data.frame(tentative_08)
# Gets the average for the year
avg_tentative_08 <- mean(as.numeric(as.character(tentative_08$tentative_08)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_08 <- sd(as.numeric(as.character(unlist(tentative_08))), na.rm = TRUE)


# 2009
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2009 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_09 <- tone_2009[as.matrix(anger)]
anger_09 <- data.frame(anger_09)
# Gets the average for the year
avg_anger_09 <- mean(as.numeric(as.character(anger_09$anger_09)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_09 <- sd(as.numeric(as.character(unlist(anger_09))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2009 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_09 <- tone_2009[as.matrix(fear)]
fear_09 <- data.frame(fear_09)
# Gets the average for the year
avg_fear_09 <- mean(as.numeric(as.character(fear_09$fear_09)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_09 <- sd(as.numeric(as.character(unlist(fear_09))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2009 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_09 <- tone_2009[as.matrix(joy)]
joy_09 <- data.frame(joy_09)
# Gets the average for the year
avg_joy_09 <- mean(as.numeric(as.character(joy_09$joy_09)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_09 <- sd(as.numeric(as.character(unlist(joy_09))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2009 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_09 <- tone_2009[as.matrix(sadness)]
sadness_09 <- data.frame(sadness_09)
# Gets the average for the year
avg_sadness_09 <- mean(as.numeric(as.character(sadness_09$sadness_09)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_09 <- sd(as.numeric(as.character(unlist(sadness_09))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2009 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_09 <- tone_2009[as.matrix(analytical)]
analytical_09 <- data.frame(analytical_09)
# Gets the average for the year
avg_analytical_09 <- mean(as.numeric(as.character(analytical_09$analytical_09)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_09 <- sd(as.numeric(as.character(unlist(analytical_09))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2009 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_09 <- tone_2009[as.matrix(confident)]
confident_09 <- data.frame(confident_09)
# Gets the average for the year
avg_confident_09 <- mean(as.numeric(as.character(confident_09$confident_09)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_09 <- sd(as.numeric(as.character(unlist(confident_09))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2009 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_09 <- tone_2009[as.matrix(tentative)]
tentative_09 <- data.frame(tentative_09)
# Gets the average for the year
avg_tentative_09 <- mean(as.numeric(as.character(tentative_09$tentative_09)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_09 <- sd(as.numeric(as.character(unlist(tentative_09))), na.rm = TRUE)


# 2010
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2010 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_10 <- tone_2010[as.matrix(anger)]
anger_10 <- data.frame(anger_10)
# Gets the average for the year
avg_anger_10 <- mean(as.numeric(as.character(anger_10$anger_10)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_10 <- sd(as.numeric(as.character(unlist(anger_10))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2010 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_10 <- tone_2010[as.matrix(fear)]
fear_10 <- data.frame(fear_10)
# Gets the average for the year
avg_fear_10 <- mean(as.numeric(as.character(fear_10$fear_10)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_10 <- sd(as.numeric(as.character(unlist(fear_10))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2010 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_10 <- tone_2010[as.matrix(joy)]
joy_10 <- data.frame(joy_10)
# Gets the average for the year
avg_joy_10 <- mean(as.numeric(as.character(joy_10$joy_10)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_10 <- sd(as.numeric(as.character(unlist(joy_10))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2010 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_10 <- tone_2010[as.matrix(sadness)]
sadness_10 <- data.frame(sadness_10)
# Gets the average for the year
avg_sadness_10 <- mean(as.numeric(as.character(sadness_10$sadness_10)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_10 <- sd(as.numeric(as.character(unlist(sadness_10))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2010 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_10 <- tone_2010[as.matrix(analytical)]
analytical_10 <- data.frame(analytical_10)
# Gets the average for the year
avg_analytical_10 <- mean(as.numeric(as.character(analytical_10$analytical_10)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_10 <- sd(as.numeric(as.character(unlist(analytical_10))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2010 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_10 <- tone_2010[as.matrix(confident)]
confident_10 <- data.frame(confident_10)
# Gets the average for the year
avg_confident_10 <- mean(as.numeric(as.character(confident_10$confident_10)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_10 <- sd(as.numeric(as.character(unlist(confident_10))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2010 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_10 <- tone_2010[as.matrix(tentative)]
tentative_10 <- data.frame(tentative_10)
# Gets the average for the year
avg_tentative_10 <- mean(as.numeric(as.character(tentative_10$tentative_10)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_10 <- sd(as.numeric(as.character(unlist(tentative_10))), na.rm = TRUE)


# 2011
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2011 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_11 <- tone_2011[as.matrix(anger)]
anger_11 <- data.frame(anger_11)
# Gets the average for the year
avg_anger_11 <- mean(as.numeric(as.character(anger_11$anger_11)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_11 <- sd(as.numeric(as.character(unlist(anger_11))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2011 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_11 <- tone_2011[as.matrix(fear)]
fear_11 <- data.frame(fear_11)
# Gets the average for the year
avg_fear_11 <- mean(as.numeric(as.character(fear_11$fear_11)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_11 <- sd(as.numeric(as.character(unlist(fear_11))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2011 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_11 <- tone_2011[as.matrix(joy)]
joy_11 <- data.frame(joy_11)
# Gets the average for the year
avg_joy_11 <- mean(as.numeric(as.character(joy_11$joy_11)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_11 <- sd(as.numeric(as.character(unlist(joy_11))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2011 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_11 <- tone_2011[as.matrix(sadness)]
sadness_11 <- data.frame(sadness_11)
# Gets the average for the year
avg_sadness_11 <- mean(as.numeric(as.character(sadness_11$sadness_11)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_11 <- sd(as.numeric(as.character(unlist(sadness_11))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2011 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_11 <- tone_2011[as.matrix(analytical)]
analytical_11 <- data.frame(analytical_11)
# Gets the average for the year
avg_analytical_11 <- mean(as.numeric(as.character(analytical_11$analytical_11)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_11 <- sd(as.numeric(as.character(unlist(analytical_11))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2011 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_11 <- tone_2011[as.matrix(confident)]
confident_11 <- data.frame(confident_11)
# Gets the average for the year
avg_confident_11 <- mean(as.numeric(as.character(confident_11$confident_11)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_11 <- sd(as.numeric(as.character(unlist(confident_11))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2011 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_11 <- tone_2011[as.matrix(tentative)]
tentative_11 <- data.frame(tentative_11)
# Gets the average for the year
avg_tentative_11 <- mean(as.numeric(as.character(tentative_11$tentative_11)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_11 <- sd(as.numeric(as.character(unlist(tentative_11))), na.rm = TRUE)


# 2012
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2012 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_12 <- tone_2012[as.matrix(anger)]
anger_12 <- data.frame(anger_12)
# Gets the average for the year
avg_anger_12 <- mean(as.numeric(as.character(anger_12$anger_12)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_12 <- sd(as.numeric(as.character(unlist(anger_12))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2012 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_12 <- tone_2012[as.matrix(fear)]
fear_12 <- data.frame(fear_12)
# Gets the average for the year
avg_fear_12 <- mean(as.numeric(as.character(fear_12$fear_12)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_12 <- sd(as.numeric(as.character(unlist(fear_12))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2012 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_12 <- tone_2012[as.matrix(joy)]
joy_12 <- data.frame(joy_12)
# Gets the average for the year
avg_joy_12 <- mean(as.numeric(as.character(joy_12$joy_12)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_12 <- sd(as.numeric(as.character(unlist(joy_12))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2012 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_12 <- tone_2012[as.matrix(sadness)]
sadness_12 <- data.frame(sadness_12)
# Gets the average for the year
avg_sadness_12 <- mean(as.numeric(as.character(sadness_12$sadness_12)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_12 <- sd(as.numeric(as.character(unlist(sadness_12))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2012 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_12 <- tone_2012[as.matrix(analytical)]
analytical_12 <- data.frame(analytical_12)
# Gets the average for the year
avg_analytical_12 <- mean(as.numeric(as.character(analytical_12$analytical_12)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_12 <- sd(as.numeric(as.character(unlist(analytical_12))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2012 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_12 <- tone_2012[as.matrix(confident)]
confident_12 <- data.frame(confident_12)
# Gets the average for the year
avg_confident_12 <- mean(as.numeric(as.character(confident_12$confident_12)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_12 <- sd(as.numeric(as.character(unlist(confident_12))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2012 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_12 <- tone_2012[as.matrix(tentative)]
tentative_12 <- data.frame(tentative_12)
# Gets the average for the year
avg_tentative_12 <- mean(as.numeric(as.character(tentative_12$tentative_12)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_12 <- sd(as.numeric(as.character(unlist(tentative_12))), na.rm = TRUE)


# 2013
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2013 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_13 <- tone_2013[as.matrix(anger)]
anger_13 <- data.frame(anger_13)
# Gets the average for the year
avg_anger_13 <- mean(as.numeric(as.character(anger_13$anger_13)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_13 <- sd(as.numeric(as.character(unlist(anger_13))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2013 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_13 <- tone_2013[as.matrix(fear)]
fear_13 <- data.frame(fear_13)
# Gets the average for the year
avg_fear_13 <- mean(as.numeric(as.character(fear_13$fear_13)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_13 <- sd(as.numeric(as.character(unlist(fear_13))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2013 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_13 <- tone_2013[as.matrix(joy)]
joy_13 <- data.frame(joy_13)
# Gets the average for the year
avg_joy_13 <- mean(as.numeric(as.character(joy_13$joy_13)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_13 <- sd(as.numeric(as.character(unlist(joy_13))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2013 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_13 <- tone_2013[as.matrix(sadness)]
sadness_13 <- data.frame(sadness_13)
# Gets the average for the year
avg_sadness_13 <- mean(as.numeric(as.character(sadness_13$sadness_13)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_13 <- sd(as.numeric(as.character(unlist(sadness_13))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2013 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_13 <- tone_2013[as.matrix(analytical)]
analytical_13 <- data.frame(analytical_13)
# Gets the average for the year
avg_analytical_13 <- mean(as.numeric(as.character(analytical_13$analytical_13)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_13 <- sd(as.numeric(as.character(unlist(analytical_13))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2013 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_13 <- tone_2013[as.matrix(confident)]
confident_13 <- data.frame(confident_13)
# Gets the average for the year
avg_confident_13 <- mean(as.numeric(as.character(confident_13$confident_13)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_13 <- sd(as.numeric(as.character(unlist(confident_13))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2013 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_13 <- tone_2013[as.matrix(tentative)]
tentative_13 <- data.frame(tentative_13)
# Gets the average for the year
avg_tentative_13 <- mean(as.numeric(as.character(tentative_13$tentative_13)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_13 <- sd(as.numeric(as.character(unlist(tentative_13))), na.rm = TRUE)


# 2014
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2014 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_14 <- tone_2014[as.matrix(anger)]
anger_14 <- data.frame(anger_14)
# Gets the average for the year
avg_anger_14 <- mean(as.numeric(as.character(anger_14$anger_14)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_14 <- sd(as.numeric(as.character(unlist(anger_14))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2014 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_14 <- tone_2014[as.matrix(fear)]
fear_14 <- data.frame(fear_14)
# Gets the average for the year
avg_fear_14 <- mean(as.numeric(as.character(fear_14$fear_14)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_14 <- sd(as.numeric(as.character(unlist(fear_14))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2014 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_14 <- tone_2014[as.matrix(joy)]
joy_14 <- data.frame(joy_14)
# Gets the average for the year
avg_joy_14 <- mean(as.numeric(as.character(joy_14$joy_14)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_14 <- sd(as.numeric(as.character(unlist(joy_14))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2014 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_14 <- tone_2014[as.matrix(sadness)]
sadness_14 <- data.frame(sadness_14)
# Gets the average for the year
avg_sadness_14 <- mean(as.numeric(as.character(sadness_14$sadness_14)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_14 <- sd(as.numeric(as.character(unlist(sadness_14))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2014 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_14 <- tone_2014[as.matrix(analytical)]
analytical_14 <- data.frame(analytical_14)
# Gets the average for the year
avg_analytical_14 <- mean(as.numeric(as.character(analytical_14$analytical_14)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_14 <- sd(as.numeric(as.character(unlist(analytical_14))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2014 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_14 <- tone_2014[as.matrix(confident)]
confident_14 <- data.frame(confident_14)
# Gets the average for the year
avg_confident_14 <- mean(as.numeric(as.character(confident_14$confident_14)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_14 <- sd(as.numeric(as.character(unlist(confident_14))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2014 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_14 <- tone_2014[as.matrix(tentative)]
tentative_14 <- data.frame(tentative_14)
# Gets the average for the year
avg_tentative_14 <- mean(as.numeric(as.character(tentative_14$tentative_14)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_14 <- sd(as.numeric(as.character(unlist(tentative_14))), na.rm = TRUE)


# 2015
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2015 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_15 <- tone_2015[as.matrix(anger)]
anger_15 <- data.frame(anger_15)
# Gets the average for the year
avg_anger_15 <- mean(as.numeric(as.character(anger_15$anger_15)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_15 <- sd(as.numeric(as.character(unlist(anger_15))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2015 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_15 <- tone_2015[as.matrix(fear)]
fear_15 <- data.frame(fear_15)
# Gets the average for the year
avg_fear_15 <- mean(as.numeric(as.character(fear_15$fear_15)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_15 <- sd(as.numeric(as.character(unlist(fear_15))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2015 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_15 <- tone_2015[as.matrix(joy)]
joy_15 <- data.frame(joy_15)
# Gets the average for the year
avg_joy_15 <- mean(as.numeric(as.character(joy_15$joy_15)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_15 <- sd(as.numeric(as.character(unlist(joy_15))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2015 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_15 <- tone_2015[as.matrix(sadness)]
sadness_15 <- data.frame(sadness_15)
# Gets the average for the year
avg_sadness_15 <- mean(as.numeric(as.character(sadness_15$sadness_15)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_15 <- sd(as.numeric(as.character(unlist(sadness_15))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2015 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_15 <- tone_2015[as.matrix(analytical)]
analytical_15 <- data.frame(analytical_15)
# Gets the average for the year
avg_analytical_15 <- mean(as.numeric(as.character(analytical_15$analytical_15)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_15 <- sd(as.numeric(as.character(unlist(analytical_15))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2015 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_15 <- tone_2015[as.matrix(confident)]
confident_15 <- data.frame(confident_15)
# Gets the average for the year
avg_confident_15 <- mean(as.numeric(as.character(confident_15$confident_15)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_15 <- sd(as.numeric(as.character(unlist(confident_15))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2015 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_15 <- tone_2015[as.matrix(tentative)]
tentative_15 <- data.frame(tentative_15)
# Gets the average for the year
avg_tentative_15 <- mean(as.numeric(as.character(tentative_15$tentative_15)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_15 <- sd(as.numeric(as.character(unlist(tentative_15))), na.rm = TRUE)


# 2016
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2016 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_16 <- tone_2016[as.matrix(anger)]
anger_16 <- data.frame(anger_16)
# Gets the average for the year
avg_anger_16 <- mean(as.numeric(as.character(anger_16$anger_16)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_16 <- sd(as.numeric(as.character(unlist(anger_16))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2016 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_16 <- tone_2016[as.matrix(fear)]
fear_16 <- data.frame(fear_16)
# Gets the average for the year
avg_fear_16 <- mean(as.numeric(as.character(fear_16$fear_16)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_16 <- sd(as.numeric(as.character(unlist(fear_16))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2016 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_16 <- tone_2016[as.matrix(joy)]
joy_16 <- data.frame(joy_16)
# Gets the average for the year
avg_joy_16 <- mean(as.numeric(as.character(joy_16$joy_16)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_16 <- sd(as.numeric(as.character(unlist(joy_16))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2016 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_16 <- tone_2016[as.matrix(sadness)]
sadness_16 <- data.frame(sadness_16)
# Gets the average for the year
avg_sadness_16 <- mean(as.numeric(as.character(sadness_16$sadness_16)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_16 <- sd(as.numeric(as.character(unlist(sadness_16))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2016 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_16 <- tone_2016[as.matrix(analytical)]
analytical_16 <- data.frame(analytical_16)
# Gets the average for the year
avg_analytical_16 <- mean(as.numeric(as.character(analytical_16$analytical_16)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_16 <- sd(as.numeric(as.character(unlist(analytical_16))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2016 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_16 <- tone_2016[as.matrix(confident)]
confident_16 <- data.frame(confident_16)
# Gets the average for the year
avg_confident_16 <- mean(as.numeric(as.character(confident_16$confident_16)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_16 <- sd(as.numeric(as.character(unlist(confident_16))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2016 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_16 <- tone_2016[as.matrix(tentative)]
tentative_16 <- data.frame(tentative_16)
# Gets the average for the year
avg_tentative_16 <- mean(as.numeric(as.character(tentative_16$tentative_16)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_16 <- sd(as.numeric(as.character(unlist(tentative_16))), na.rm = TRUE)


# 2017
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2017 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_17 <- tone_2017[as.matrix(anger)]
anger_17 <- data.frame(anger_17)
# Gets the average for the year
avg_anger_17 <- mean(as.numeric(as.character(anger_17$anger_17)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_17 <- sd(as.numeric(as.character(unlist(anger_17))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2017 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_17 <- tone_2017[as.matrix(fear)]
fear_17 <- data.frame(fear_17)
# Gets the average for the year
avg_fear_17 <- mean(as.numeric(as.character(fear_17$fear_17)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_17 <- sd(as.numeric(as.character(unlist(fear_17))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2017 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_17 <- tone_2017[as.matrix(joy)]
joy_17 <- data.frame(joy_17)
# Gets the average for the year
avg_joy_17 <- mean(as.numeric(as.character(joy_17$joy_17)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_17 <- sd(as.numeric(as.character(unlist(joy_17))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2017 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_17 <- tone_2017[as.matrix(sadness)]
sadness_17 <- data.frame(sadness_17)
# Gets the average for the year
avg_sadness_17 <- mean(as.numeric(as.character(sadness_17$sadness_17)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_17 <- sd(as.numeric(as.character(unlist(sadness_17))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2017 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_17 <- tone_2017[as.matrix(analytical)]
analytical_17 <- data.frame(analytical_17)
# Gets the average for the year
avg_analytical_17 <- mean(as.numeric(as.character(analytical_17$analytical_17)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_17 <- sd(as.numeric(as.character(unlist(analytical_17))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2017 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_17 <- tone_2017[as.matrix(confident)]
confident_17 <- data.frame(confident_17)
# Gets the average for the year
avg_confident_17 <- mean(as.numeric(as.character(confident_17$confident_17)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_17 <- sd(as.numeric(as.character(unlist(confident_17))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2017 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_17 <- tone_2017[as.matrix(tentative)]
tentative_17 <- data.frame(tentative_17)
# Gets the average for the year
avg_tentative_17 <- mean(as.numeric(as.character(tentative_17$tentative_17)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_17 <- sd(as.numeric(as.character(unlist(tentative_17))), na.rm = TRUE)


# 2018
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2018 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_18 <- tone_2018[as.matrix(anger)]
anger_18 <- data.frame(anger_18)
# Gets the average for the year
avg_anger_18 <- mean(as.numeric(as.character(anger_18$anger_18)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_18 <- sd(as.numeric(as.character(unlist(anger_18))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2018 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_18 <- tone_2018[as.matrix(fear)]
fear_18 <- data.frame(fear_18)
# Gets the average for the year
avg_fear_18 <- mean(as.numeric(as.character(fear_18$fear_18)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_18 <- sd(as.numeric(as.character(unlist(fear_18))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2018 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_18 <- tone_2018[as.matrix(joy)]
joy_18 <- data.frame(joy_18)
# Gets the average for the year
avg_joy_18 <- mean(as.numeric(as.character(joy_18$joy_18)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_18 <- sd(as.numeric(as.character(unlist(joy_18))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2018 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_18 <- tone_2018[as.matrix(sadness)]
sadness_18 <- data.frame(sadness_18)
# Gets the average for the year
avg_sadness_18 <- mean(as.numeric(as.character(sadness_18$sadness_18)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_18 <- sd(as.numeric(as.character(unlist(sadness_18))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2018 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_18 <- tone_2018[as.matrix(analytical)]
analytical_18 <- data.frame(analytical_18)
# Gets the average for the year
avg_analytical_18 <- mean(as.numeric(as.character(analytical_18$analytical_18)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_18 <- sd(as.numeric(as.character(unlist(analytical_18))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2018 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_18 <- tone_2018[as.matrix(confident)]
confident_18 <- data.frame(confident_18)
# Gets the average for the year
avg_confident_18 <- mean(as.numeric(as.character(confident_18$confident_18)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_18 <- sd(as.numeric(as.character(unlist(confident_18))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2018 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_18 <- tone_2018[as.matrix(tentative)]
tentative_18 <- data.frame(tentative_18)
# Gets the average for the year
avg_tentative_18 <- mean(as.numeric(as.character(tentative_18$tentative_18)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_18 <- sd(as.numeric(as.character(unlist(tentative_18))), na.rm = TRUE)


# 2019
# Anger
# Gets each instance of a label
anger <- data.frame(which(tone_2019 == "anger", arr.ind = TRUE))
anger$col <- anger$col - 1
anger_19 <- tone_2019[as.matrix(anger)]
anger_19 <- data.frame(anger_19)
# Gets the average for the year
avg_anger_19 <- mean(as.numeric(as.character(anger_19$anger_19)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_anger_19 <- sd(as.numeric(as.character(unlist(anger_19))), na.rm = TRUE)

# Fear
# Gets each instance of a label
fear <- data.frame(which(tone_2019 == "fear", arr.ind = TRUE))
fear$col <- fear$col - 1
fear_19 <- tone_2019[as.matrix(fear)]
fear_19 <- data.frame(fear_19)
# Gets the average for the year
avg_fear_19 <- mean(as.numeric(as.character(fear_19$fear_19)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_fear_19 <- sd(as.numeric(as.character(unlist(fear_19))), na.rm = TRUE)

# Joy
joy <- data.frame(which(tone_2019 == "joy", arr.ind = TRUE))
joy$col <- joy$col - 1
joy_19 <- tone_2019[as.matrix(joy)]
joy_19 <- data.frame(joy_19)
# Gets the average for the year
avg_joy_19 <- mean(as.numeric(as.character(joy_19$joy_19)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_joy_19 <- sd(as.numeric(as.character(unlist(joy_19))), na.rm = TRUE)

# Sadness
sadness <- data.frame(which(tone_2019 == "sadness", arr.ind = TRUE))
sadness$col <- sadness$col - 1
sadness_19 <- tone_2019[as.matrix(sadness)]
sadness_19 <- data.frame(sadness_19)
# Gets the average for the year
avg_sadness_19 <- mean(as.numeric(as.character(sadness_19$sadness_19)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_sadness_19 <- sd(as.numeric(as.character(unlist(sadness_19))), na.rm = TRUE)

# Analytical
analytical <- data.frame(which(tone_2019 == "analytical", arr.ind = TRUE))
analytical$col <- analytical$col - 1
analytical_19 <- tone_2019[as.matrix(analytical)]
analytical_19 <- data.frame(analytical_19)
# Gets the average for the year
avg_analytical_19 <- mean(as.numeric(as.character(analytical_19$analytical_19)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_analytical_19 <- sd(as.numeric(as.character(unlist(analytical_19))), na.rm = TRUE)

# Confident
confident <- data.frame(which(tone_2019 == "confident", arr.ind = TRUE))
confident$col <- confident$col - 1
confident_19 <- tone_2019[as.matrix(confident)]
confident_19 <- data.frame(confident_19)
# Gets the average for the year
avg_confident_19 <- mean(as.numeric(as.character(confident_19$confident_19)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_confident_19 <- sd(as.numeric(as.character(unlist(confident_19))), na.rm = TRUE)

# Tentative
tentative <- data.frame(which(tone_2019 == "tentative", arr.ind = TRUE))
tentative$col <- tentative$col - 1
tentative_19 <- tone_2019[as.matrix(tentative)]
tentative_19 <- data.frame(tentative_19)
# Gets the average for the year
avg_tentative_19 <- mean(as.numeric(as.character(tentative_19$tentative_19)), na.rm = TRUE)
# Limits for standard deviation (error bars)
sd_tentative_19 <- sd(as.numeric(as.character(unlist(tentative_19))), na.rm = TRUE)

# Get the final dataframes for each tone
year <- c(1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)

# Anger
score <- c(avg_anger_94, avg_anger_95, avg_anger_96, avg_anger_97, avg_anger_98, avg_anger_99, avg_anger_00, avg_anger_01, avg_anger_02, avg_anger_03, avg_anger_04, avg_anger_05, avg_anger_06, avg_anger_07, avg_anger_08, avg_anger_09, avg_anger_10, avg_anger_11, avg_anger_12, avg_anger_13, avg_anger_14, avg_anger_15, avg_anger_16, avg_anger_17, avg_anger_18, avg_anger_19)
sd <- c(sd_anger_94, sd_anger_95, sd_anger_96, sd_anger_97, sd_anger_98, sd_anger_99, sd_anger_00, sd_anger_01, sd_anger_02, sd_anger_03, sd_anger_04, sd_anger_05, sd_anger_06, sd_anger_07, sd_anger_08, sd_anger_09, sd_anger_10, sd_anger_11, sd_anger_12, sd_anger_13, sd_anger_14, sd_anger_15, sd_anger_16, sd_anger_17, sd_anger_18, sd_anger_19)

total_anger <- data.frame(score, year)

limits <- aes(ymax = score + sd, ymin = score - sd)

total_anger %>% ggplot(aes(x = year, y = score)) +
  geom_point(colour = "steelblue3") +
  geom_errorbar(limits, width=.2,
                position=position_dodge(0.05), color ="steelblue3") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  geom_line(color="steelblue3")+
  ggtitle("Anger")

# Fear
score <- c(avg_fear_94, avg_fear_95, avg_fear_96, avg_fear_97, avg_fear_98, avg_fear_99, avg_fear_00, avg_fear_01, avg_fear_02, avg_fear_03, avg_fear_04, avg_fear_05, avg_fear_06, avg_fear_07, avg_fear_08, avg_fear_09, avg_fear_10, avg_fear_11, avg_fear_12, avg_fear_13, avg_fear_14, avg_fear_15, avg_fear_16, avg_fear_17, avg_fear_18, avg_fear_19)
sd <- c(sd_fear_94, sd_fear_95, sd_fear_96, sd_fear_97, sd_fear_98, sd_fear_99, sd_fear_00, sd_fear_01, sd_fear_02, sd_fear_03, sd_fear_04, sd_fear_05, sd_fear_06, sd_fear_07, sd_fear_08, sd_fear_09, sd_fear_10, sd_fear_11, sd_fear_12, sd_fear_13, sd_fear_14, sd_fear_15, sd_fear_16, sd_fear_17, sd_fear_18, sd_fear_19)

total_fear <- data.frame(score, year)

total_fear %>% ggplot(aes(x = year, y = score)) +
  geom_point(colour = "steelblue3") +
  geom_errorbar(limits, width=.2,
                position=position_dodge(0.05), color ="steelblue3") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  geom_line(color="steelblue3")+
  ggtitle("Fear")

# Joy
score <- c(avg_joy_94, avg_joy_95, avg_joy_96, avg_joy_97, avg_joy_98, avg_joy_99, avg_joy_00, avg_joy_01, avg_joy_02, avg_joy_03, avg_joy_04, avg_joy_05, avg_joy_06, avg_joy_07, avg_joy_08, avg_joy_09, avg_joy_10, avg_joy_11, avg_joy_12, avg_joy_13, avg_joy_14, avg_joy_15, avg_joy_16, avg_joy_17, avg_joy_18, avg_joy_19)
sd <- c(sd_joy_94, sd_joy_95, sd_joy_96, sd_joy_97, sd_joy_98, sd_joy_99, sd_joy_00, sd_joy_01, sd_joy_02, sd_joy_03, sd_joy_04, sd_joy_05, sd_joy_06, sd_joy_07, sd_joy_08, sd_joy_09, sd_joy_10, sd_joy_11, sd_joy_12, sd_joy_13, sd_joy_14, sd_joy_15, sd_joy_16, sd_joy_17, sd_joy_18, sd_joy_19)

total_joy <- data.frame(score, year)

limits <- aes(ymax = score + sd, ymin = score - sd)

total_joy %>% ggplot(aes(x = year, y = score)) +
  geom_point(colour = "steelblue3") +
  geom_errorbar(limits, width=.2,
                position=position_dodge(0.05), color ="steelblue3") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  geom_line(color="steelblue3")+
  ggtitle("Joy")

# Sadness
score <- c(avg_sadness_94, avg_sadness_95, avg_sadness_96, avg_sadness_97, avg_sadness_98, avg_sadness_99, avg_sadness_00, avg_sadness_01, avg_sadness_02, avg_sadness_03, avg_sadness_04, avg_sadness_05, avg_sadness_06, avg_sadness_07, avg_sadness_08, avg_sadness_09, avg_sadness_10, avg_sadness_11, avg_sadness_12, avg_sadness_13, avg_sadness_14, avg_sadness_15, avg_sadness_16, avg_sadness_17, avg_sadness_18, avg_sadness_19)
sd <- c(sd_sadness_94, sd_sadness_95, sd_sadness_96, sd_sadness_97, sd_sadness_98, sd_sadness_99, sd_sadness_00, sd_sadness_01, sd_sadness_02, sd_sadness_03, sd_sadness_04, sd_sadness_05, sd_sadness_06, sd_sadness_07, sd_sadness_08, sd_sadness_09, sd_sadness_10, sd_sadness_11, sd_sadness_12, sd_sadness_13, sd_sadness_14, sd_sadness_15, sd_sadness_16, sd_sadness_17, sd_sadness_18, sd_sadness_19)

total_sadness <- data.frame(score, year)

limits <- aes(ymax = score + sd, ymin = score - sd)

total_sadness %>% ggplot(aes(x = year, y = score)) +
  geom_point(colour = "steelblue3") +
  geom_errorbar(limits, width=.2,
                position=position_dodge(0.05), color ="steelblue3") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  geom_line(color="steelblue3")+
  ggtitle("Sadness")

# Analytical
score <- c(avg_analytical_94, avg_analytical_95, avg_analytical_96, avg_analytical_97, avg_analytical_98, avg_analytical_99, avg_analytical_00, avg_analytical_01, avg_analytical_02, avg_analytical_03, avg_analytical_04, avg_analytical_05, avg_analytical_06, avg_analytical_07, avg_analytical_08, avg_analytical_09, avg_analytical_10, avg_analytical_11, avg_analytical_12, avg_analytical_13, avg_analytical_14, avg_analytical_15, avg_analytical_16, avg_analytical_17, avg_analytical_18, avg_analytical_19)
sd <- c(sd_analytical_94, sd_analytical_95, sd_analytical_96, sd_analytical_97, sd_analytical_98, sd_analytical_99, sd_analytical_00, sd_analytical_01, sd_analytical_02, sd_analytical_03, sd_analytical_04, sd_analytical_05, sd_analytical_06, sd_analytical_07, sd_analytical_08, sd_analytical_09, sd_analytical_10, sd_analytical_11, sd_analytical_12, sd_analytical_13, sd_analytical_14, sd_analytical_15, sd_analytical_16, sd_analytical_17, sd_analytical_18, sd_analytical_19)

total_analytical <- data.frame(score, year)

limits <- aes(ymax = score + sd, ymin = score - sd)

total_analytical %>% ggplot(aes(x = year, y = score)) +
  geom_point(colour = "steelblue3") +
  geom_errorbar(limits, width=.2,
                position=position_dodge(0.05), color ="steelblue3") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  geom_line(color="steelblue3")+
  ggtitle("Analytical")

# Confident
score <- c(avg_confident_94, avg_confident_95, avg_confident_96, avg_confident_97, avg_confident_98, avg_confident_99, avg_confident_00, avg_confident_01, avg_confident_02, avg_confident_03, avg_confident_04, avg_confident_05, avg_confident_06, avg_confident_07, avg_confident_08, avg_confident_09, avg_confident_10, avg_confident_11, avg_confident_12, avg_confident_13, avg_confident_14, avg_confident_15, avg_confident_16, avg_confident_17, avg_confident_18, avg_confident_19)
sd <- c(sd_confident_94, sd_confident_95, sd_confident_96, sd_confident_97, sd_confident_98, sd_confident_99, sd_confident_00, sd_confident_01, sd_confident_02, sd_confident_03, sd_confident_04, sd_confident_05, sd_confident_06, sd_confident_07, sd_confident_08, sd_confident_09, sd_confident_10, sd_confident_11, sd_confident_12, sd_confident_13, sd_confident_14, sd_confident_15, sd_confident_16, sd_confident_17, sd_confident_18, sd_confident_19)

total_confident <- data.frame(score, year)

limits <- aes(ymax = score + sd, ymin = score - sd)

total_confident %>% ggplot(aes(x = year, y = score)) +
  geom_point(colour = "steelblue3") +
  geom_errorbar(limits, width=.2,
                position=position_dodge(0.05), color ="steelblue3") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  geom_line(color="steelblue3")+
  ggtitle("Confident")

# Tentative
score <- c(avg_tentative_94, avg_tentative_95, avg_tentative_96, avg_tentative_97, avg_tentative_98, avg_tentative_99, avg_tentative_00, avg_tentative_01, avg_tentative_02, avg_tentative_03, avg_tentative_04, avg_tentative_05, avg_tentative_06, avg_tentative_07, avg_tentative_08, avg_tentative_09, avg_tentative_10, avg_tentative_11, avg_tentative_12, avg_tentative_13, avg_tentative_14, avg_tentative_15, avg_tentative_16, avg_tentative_17, avg_tentative_18, avg_tentative_19)
sd <- c(sd_tentative_94, sd_tentative_95, sd_tentative_96, sd_tentative_97, sd_tentative_98, sd_tentative_99, sd_tentative_00, sd_tentative_01, sd_tentative_02, sd_tentative_03, sd_tentative_04, sd_tentative_05, sd_tentative_06, sd_tentative_07, sd_tentative_08, sd_tentative_09, sd_tentative_10, sd_tentative_11, sd_tentative_12, sd_tentative_13, sd_tentative_14, sd_tentative_15, sd_tentative_16, sd_tentative_17, sd_tentative_18, sd_tentative_19)

total_tentative <- data.frame(score, year)

limits <- aes(ymax = score + sd, ymin = score - sd)

total_tentative %>% ggplot(aes(x = year, y = score)) +
  geom_point(colour = "steelblue3") +
  geom_errorbar(limits, width=.2,
                position=position_dodge(0.05), color ="steelblue3") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  geom_line(color="steelblue3")+
  ggtitle("Tentative")

# Significance tests

anger_sig <- lm(data = total_anger, score ~ year)
summary(anger_sig) # p-value: 0.425338

fear_sig <- lm(data = total_fear, score ~ year)
summary(fear_sig) # p-value: 0.955374

joy_sig <- lm(data = total_joy, score ~ year)
summary(joy_sig) # p-value: 0.40132

sadness_sig <- lm(data = total_sadness, score ~ year)
summary(sadness_sig) # p-value: 0.500714

analytical_sig <- lm(data = total_analytical, score ~ year)
summary(analytical_sig) # p-value: 0.41322

confident_sig <- lm(data = total_confident, score ~ year)
summary(confident_sig) # p-value: 0.17675

tentative_sig <- lm(data = total_tentative, score ~ year)
summary(tentative_sig) # p-value: 0.886113