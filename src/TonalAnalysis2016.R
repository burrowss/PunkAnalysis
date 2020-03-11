# Simon Burrows
# Senior thesis

# Ask about including social tone

library(tidytext)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

# Reading in all_tone_data.csv to create a dataframe
all_tone_v2016 <- read_csv("~/Documents/CS_600/PunkAnalysis/src/tone_resources/all_tone_data_v2016.csv")
all_tone_v2016 <- data.frame(all_tone_v2016)
all_tone_v2016 <- all_tone_v2016 %>% rename(anger_score = "document_tone.tone_categories.0.tones.0.score",
                                            disgust_score = "document_tone.tone_categories.0.tones.1.score",
                                            fear_score = "document_tone.tone_categories.0.tones.2.score",
                                            joy_score = "document_tone.tone_categories.0.tones.3.score",
                                            sadness_score = "document_tone.tone_categories.0.tones.4.score",
                                            analytical_score = "document_tone.tone_categories.1.tones.0.score",
                                            confident_score = "document_tone.tone_categories.1.tones.1.score",
                                            tentative_score = "document_tone.tone_categories.1.tones.2.score",
                                            openness_score = "document_tone.tone_categories.2.tones.0.score",
                                            conscientiousness_score = "document_tone.tone_categories.2.tones.1.score",
                                            extraversion_score = "document_tone.tone_categories.2.tones.2.score",
                                            agreeableness_score = "document_tone.tone_categories.2.tones.3.score",
                                            emotional_range_score = "document_tone.tone_categories.2.tones.4.score")

all_tone_v2016 <- all_tone_v2016 %>% select(anger_score, disgust_score, fear_score, joy_score, sadness_score,
                                            analytical_score, confident_score, tentative_score, openness_score,
                                            conscientiousness_score, extraversion_score, agreeableness_score, emotional_range_score)

tone_1994_v2016 <- all_tone_v2016 %>% slice(1:49) # 49
tone_1995_v2016 <- all_tone_v2016 %>% slice(50:111) # 62 
tone_1996_v2016 <- all_tone_v2016 %>% slice(112:127) # 16
tone_1997_v2016 <- all_tone_v2016 %>% slice(128:160) # 33
tone_1998_v2016 <- all_tone_v2016 %>% slice(161:212) # 52
tone_1999_v2016 <- all_tone_v2016 %>% slice(213:272) # 60
tone_2000_v2016 <- all_tone_v2016 %>% slice(273:315) # 43
tone_2001_v2016 <- all_tone_v2016 %>% slice(316:381) # 66
tone_2002_v2016 <- all_tone_v2016 %>% slice(382:455) # 74
tone_2003_v2016 <- all_tone_v2016 %>% slice(456:550) # 95
tone_2004_v2016 <- all_tone_v2016 %>% slice(551:615) # 65
tone_2005_v2016 <- all_tone_v2016 %>% slice(616:659) # 44
tone_2006_v2016 <- all_tone_v2016 %>% slice(660:698) # 39
tone_2007_v2016 <- all_tone_v2016 %>% slice(699:783) # 85
tone_2008_v2016 <- all_tone_v2016 %>% slice(784:824) # 41
tone_2009_v2016 <- all_tone_v2016 %>% slice(825:864) # 40
tone_2010_v2016 <- all_tone_v2016 %>% slice(865:910) # 46
tone_2011_v2016 <- all_tone_v2016 %>% slice(911:961) # 51
tone_2012_v2016 <- all_tone_v2016 %>% slice(962:1013) # 52
tone_2013_v2016 <- all_tone_v2016 %>% slice(1014:1083) # 70
tone_2014_v2016 <- all_tone_v2016 %>% slice(1084:1150) # 67
tone_2015_v2016 <- all_tone_v2016 %>% slice(1151:1204) # 54
tone_2016_v2016 <- all_tone_v2016 %>% slice(1205:1336) # 132
tone_2017_v2016 <- all_tone_v2016 %>% slice(1337:1408) # 72
tone_2018_v2016 <- all_tone_v2016 %>% slice(1409:1515) # 107
tone_2019_v2016 <- all_tone_v2016 %>% slice(1516:1573) # 58

# Average 1994 Tones
avg_anger_94_v2016 <- mean(tone_1994_v2016$anger_score)
avg_disgust_94_v2016 <- mean(tone_1994_v2016$disgust_score)
avg_fear_94_v2016 <- mean(tone_1994_v2016$fear_score)
avg_joy_94_v2016 <- mean(tone_1994_v2016$joy_score)
avg_sadness_94_v2016 <- mean(tone_1994_v2016$sadness_score)
avg_analytical_94_v2016 <- mean(tone_1994_v2016$analytical_score)
avg_confident_94_v2016 <- mean(tone_1994_v2016$confident_score)
avg_tentative_94_v2016 <- mean(tone_1994_v2016$tentative_score)
avg_openness_94_v2016 <- mean(tone_1994_v2016$openness_score)
avg_conscientiousness_94_v2016 <- mean(tone_1994_v2016$conscientiousness_score)
avg_extraversion_94_v2016 <- mean(tone_1994_v2016$extraversion_score)
avg_agreeableness_94_v2016 <- mean(tone_1994_v2016$agreeableness_score)
avg_emotional_range_94_v2016 <- mean(tone_1994_v2016$emotional_range_score)

# Average 1995 Tones
avg_anger_95_v2016 <- mean(tone_1995_v2016$anger_score)
avg_disgust_95_v2016 <- mean(tone_1995_v2016$disgust_score)
avg_fear_95_v2016 <- mean(tone_1995_v2016$fear_score)
avg_joy_95_v2016 <- mean(tone_1995_v2016$joy_score)
avg_sadness_95_v2016 <- mean(tone_1995_v2016$sadness_score)
avg_analytical_95_v2016 <- mean(tone_1995_v2016$analytical_score)
avg_confident_95_v2016 <- mean(tone_1995_v2016$confident_score)
avg_tentative_95_v2016 <- mean(tone_1995_v2016$tentative_score)
avg_openness_95_v2016 <- mean(tone_1995_v2016$openness_score)
avg_conscientiousness_95_v2016 <- mean(tone_1995_v2016$conscientiousness_score)
avg_extraversion_95_v2016 <- mean(tone_1995_v2016$extraversion_score)
avg_agreeableness_95_v2016 <- mean(tone_1995_v2016$agreeableness_score)
avg_emotional_range_95_v2016 <- mean(tone_1995_v2016$emotional_range_score)

# Average 1996 Tones
avg_anger_96_v2016 <- mean(tone_1996_v2016$anger_score)
avg_disgust_96_v2016 <- mean(tone_1996_v2016$disgust_score)
avg_fear_96_v2016 <- mean(tone_1996_v2016$fear_score)
avg_joy_96_v2016 <- mean(tone_1996_v2016$joy_score)
avg_sadness_96_v2016 <- mean(tone_1996_v2016$sadness_score)
avg_analytical_96_v2016 <- mean(tone_1996_v2016$analytical_score)
avg_confident_96_v2016 <- mean(tone_1996_v2016$confident_score)
avg_tentative_96_v2016 <- mean(tone_1996_v2016$tentative_score)
avg_openness_96_v2016 <- mean(tone_1996_v2016$openness_score)
avg_conscientiousness_96_v2016 <- mean(tone_1996_v2016$conscientiousness_score)
avg_extraversion_96_v2016 <- mean(tone_1996_v2016$extraversion_score)
avg_agreeableness_96_v2016 <- mean(tone_1996_v2016$agreeableness_score)
avg_emotional_range_96_v2016 <- mean(tone_1996_v2016$emotional_range_score)

# Average 1997 Tones
avg_anger_97_v2016 <- mean(tone_1997_v2016$anger_score)
avg_disgust_97_v2016 <- mean(tone_1997_v2016$disgust_score)
avg_fear_97_v2016 <- mean(tone_1997_v2016$fear_score)
avg_joy_97_v2016 <- mean(tone_1997_v2016$joy_score)
avg_sadness_97_v2016 <- mean(tone_1997_v2016$sadness_score)
avg_analytical_97_v2016 <- mean(tone_1997_v2016$analytical_score)
avg_confident_97_v2016 <- mean(tone_1997_v2016$confident_score)
avg_tentative_97_v2016 <- mean(tone_1997_v2016$tentative_score)
avg_openness_97_v2016 <- mean(tone_1997_v2016$openness_score)
avg_conscientiousness_97_v2016 <- mean(tone_1997_v2016$conscientiousness_score)
avg_extraversion_97_v2016 <- mean(tone_1997_v2016$extraversion_score)
avg_agreeableness_97_v2016 <- mean(tone_1997_v2016$agreeableness_score)
avg_emotional_range_97_v2016 <- mean(tone_1997_v2016$emotional_range_score)

# Average 1998 Tones
avg_anger_98_v2016 <- mean(tone_1998_v2016$anger_score)
avg_disgust_98_v2016 <- mean(tone_1998_v2016$disgust_score)
avg_fear_98_v2016 <- mean(tone_1998_v2016$fear_score)
avg_joy_98_v2016 <- mean(tone_1998_v2016$joy_score)
avg_sadness_98_v2016 <- mean(tone_1998_v2016$sadness_score)
avg_analytical_98_v2016 <- mean(tone_1998_v2016$analytical_score)
avg_confident_98_v2016 <- mean(tone_1998_v2016$confident_score)
avg_tentative_98_v2016 <- mean(tone_1998_v2016$tentative_score)
avg_openness_98_v2016 <- mean(tone_1998_v2016$openness_score)
avg_conscientiousness_98_v2016 <- mean(tone_1998_v2016$conscientiousness_score)
avg_extraversion_98_v2016 <- mean(tone_1998_v2016$extraversion_score)
avg_agreeableness_98_v2016 <- mean(tone_1998_v2016$agreeableness_score)
avg_emotional_range_98_v2016 <- mean(tone_1998_v2016$emotional_range_score)


# Average 1999 Tones
avg_anger_99_v2016 <- mean(tone_1999_v2016$anger_score)
avg_disgust_99_v2016 <- mean(tone_1999_v2016$disgust_score)
avg_fear_99_v2016 <- mean(tone_1999_v2016$fear_score)
avg_joy_99_v2016 <- mean(tone_1999_v2016$joy_score)
avg_sadness_99_v2016 <- mean(tone_1999_v2016$sadness_score)
avg_analytical_99_v2016 <- mean(tone_1999_v2016$analytical_score)
avg_confident_99_v2016 <- mean(tone_1999_v2016$confident_score)
avg_tentative_99_v2016 <- mean(tone_1999_v2016$tentative_score)
avg_openness_99_v2016 <- mean(tone_1999_v2016$openness_score)
avg_conscientiousness_99_v2016 <- mean(tone_1999_v2016$conscientiousness_score)
avg_extraversion_99_v2016 <- mean(tone_1999_v2016$extraversion_score)
avg_agreeableness_99_v2016 <- mean(tone_1999_v2016$agreeableness_score)
avg_emotional_range_99_v2016 <- mean(tone_1999_v2016$emotional_range_score)

# Average 2000 Tones
avg_anger_00_v2016 <- mean(tone_2000_v2016$anger_score)
avg_disgust_00_v2016 <- mean(tone_2000_v2016$disgust_score)
avg_fear_00_v2016 <- mean(tone_2000_v2016$fear_score)
avg_joy_00_v2016 <- mean(tone_2000_v2016$joy_score)
avg_sadness_00_v2016 <- mean(tone_2000_v2016$sadness_score)
avg_analytical_00_v2016 <- mean(tone_2000_v2016$analytical_score)
avg_confident_00_v2016 <- mean(tone_2000_v2016$confident_score)
avg_tentative_00_v2016 <- mean(tone_2000_v2016$tentative_score)
avg_openness_00_v2016 <- mean(tone_2000_v2016$openness_score)
avg_conscientiousness_00_v2016 <- mean(tone_2000_v2016$conscientiousness_score)
avg_extraversion_00_v2016 <- mean(tone_2000_v2016$extraversion_score)
avg_agreeableness_00_v2016 <- mean(tone_2000_v2016$agreeableness_score)
avg_emotional_range_00_v2016 <- mean(tone_2000_v2016$emotional_range_score)

# Average 2001 Tones
avg_anger_01_v2016 <- mean(tone_2001_v2016$anger_score)
avg_disgust_01_v2016 <- mean(tone_2001_v2016$disgust_score)
avg_fear_01_v2016 <- mean(tone_2001_v2016$fear_score)
avg_joy_01_v2016 <- mean(tone_2001_v2016$joy_score)
avg_sadness_01_v2016 <- mean(tone_2001_v2016$sadness_score)
avg_analytical_01_v2016 <- mean(tone_2001_v2016$analytical_score)
avg_confident_01_v2016 <- mean(tone_2001_v2016$confident_score)
avg_tentative_01_v2016 <- mean(tone_2001_v2016$tentative_score)
avg_openness_01_v2016 <- mean(tone_2001_v2016$openness_score)
avg_conscientiousness_01_v2016 <- mean(tone_2001_v2016$conscientiousness_score)
avg_extraversion_01_v2016 <- mean(tone_2001_v2016$extraversion_score)
avg_agreeableness_01_v2016 <- mean(tone_2001_v2016$agreeableness_score)
avg_emotional_range_01_v2016 <- mean(tone_2001_v2016$emotional_range_score)

# Average 2002 Tones
avg_anger_02_v2016 <- mean(tone_2002_v2016$anger_score)
avg_disgust_02_v2016 <- mean(tone_2002_v2016$disgust_score)
avg_fear_02_v2016 <- mean(tone_2002_v2016$fear_score)
avg_joy_02_v2016 <- mean(tone_2002_v2016$joy_score)
avg_sadness_02_v2016 <- mean(tone_2002_v2016$sadness_score)
avg_analytical_02_v2016 <- mean(tone_2002_v2016$analytical_score)
avg_confident_02_v2016 <- mean(tone_2002_v2016$confident_score)
avg_tentative_02_v2016 <- mean(tone_2002_v2016$tentative_score)
avg_openness_02_v2016 <- mean(tone_2002_v2016$openness_score)
avg_conscientiousness_02_v2016 <- mean(tone_2002_v2016$conscientiousness_score)
avg_extraversion_02_v2016 <- mean(tone_2002_v2016$extraversion_score)
avg_agreeableness_02_v2016 <- mean(tone_2002_v2016$agreeableness_score)
avg_emotional_range_02_v2016 <- mean(tone_2002_v2016$emotional_range_score)

# Average 2003 Tones
avg_anger_03_v2016 <- mean(tone_2003_v2016$anger_score)
avg_disgust_03_v2016 <- mean(tone_2003_v2016$disgust_score)
avg_fear_03_v2016 <- mean(tone_2003_v2016$fear_score)
avg_joy_03_v2016 <- mean(tone_2003_v2016$joy_score)
avg_sadness_03_v2016 <- mean(tone_2003_v2016$sadness_score)
avg_analytical_03_v2016 <- mean(tone_2003_v2016$analytical_score)
avg_confident_03_v2016 <- mean(tone_2003_v2016$confident_score)
avg_tentative_03_v2016 <- mean(tone_2003_v2016$tentative_score)
avg_openness_03_v2016 <- mean(tone_2003_v2016$openness_score)
avg_conscientiousness_03_v2016 <- mean(tone_2003_v2016$conscientiousness_score)
avg_extraversion_03_v2016 <- mean(tone_2003_v2016$extraversion_score)
avg_agreeableness_03_v2016 <- mean(tone_2003_v2016$agreeableness_score)
avg_emotional_range_03_v2016 <- mean(tone_2003_v2016$emotional_range_score)

# Average 2004 Tones
avg_anger_04_v2016 <- mean(tone_2004_v2016$anger_score)
avg_disgust_04_v2016 <- mean(tone_2004_v2016$disgust_score)
avg_fear_04_v2016 <- mean(tone_2004_v2016$fear_score)
avg_joy_04_v2016 <- mean(tone_2004_v2016$joy_score)
avg_sadness_04_v2016 <- mean(tone_2004_v2016$sadness_score)
avg_analytical_04_v2016 <- mean(tone_2004_v2016$analytical_score)
avg_confident_04_v2016 <- mean(tone_2004_v2016$confident_score)
avg_tentative_04_v2016 <- mean(tone_2004_v2016$tentative_score)
avg_openness_04_v2016 <- mean(tone_2004_v2016$openness_score)
avg_conscientiousness_04_v2016 <- mean(tone_2004_v2016$conscientiousness_score)
avg_extraversion_04_v2016 <- mean(tone_2004_v2016$extraversion_score)
avg_agreeableness_04_v2016 <- mean(tone_2004_v2016$agreeableness_score)
avg_emotional_range_04_v2016 <- mean(tone_2004_v2016$emotional_range_score)

# Average 2005 Tones
avg_anger_05_v2016 <- mean(tone_2005_v2016$anger_score)
avg_disgust_05_v2016 <- mean(tone_2005_v2016$disgust_score)
avg_fear_05_v2016 <- mean(tone_2005_v2016$fear_score)
avg_joy_05_v2016 <- mean(tone_2005_v2016$joy_score)
avg_sadness_05_v2016 <- mean(tone_2005_v2016$sadness_score)
avg_analytical_05_v2016 <- mean(tone_2005_v2016$analytical_score)
avg_confident_05_v2016 <- mean(tone_2005_v2016$confident_score)
avg_tentative_05_v2016 <- mean(tone_2005_v2016$tentative_score)
avg_openness_05_v2016 <- mean(tone_2005_v2016$openness_score)
avg_conscientiousness_05_v2016 <- mean(tone_2005_v2016$conscientiousness_score)
avg_extraversion_05_v2016 <- mean(tone_2005_v2016$extraversion_score)
avg_agreeableness_05_v2016 <- mean(tone_2005_v2016$agreeableness_score)
avg_emotional_range_05_v2016 <- mean(tone_2005_v2016$emotional_range_score)

# Average 2006 Tones
avg_anger_06_v2016 <- mean(tone_2006_v2016$anger_score)
avg_disgust_06_v2016 <- mean(tone_2006_v2016$disgust_score)
avg_fear_06_v2016 <- mean(tone_2006_v2016$fear_score)
avg_joy_06_v2016 <- mean(tone_2006_v2016$joy_score)
avg_sadness_06_v2016 <- mean(tone_2006_v2016$sadness_score)
avg_analytical_06_v2016 <- mean(tone_2006_v2016$analytical_score)
avg_confident_06_v2016 <- mean(tone_2006_v2016$confident_score)
avg_tentative_06_v2016 <- mean(tone_2006_v2016$tentative_score)
avg_openness_06_v2016 <- mean(tone_2006_v2016$openness_score)
avg_conscientiousness_06_v2016 <- mean(tone_2006_v2016$conscientiousness_score)
avg_extraversion_06_v2016 <- mean(tone_2006_v2016$extraversion_score)
avg_agreeableness_06_v2016 <- mean(tone_2006_v2016$agreeableness_score)
avg_emotional_range_06_v2016 <- mean(tone_2006_v2016$emotional_range_score)

# Average 2007 Tones
avg_anger_07_v2016 <- mean(tone_2007_v2016$anger_score)
avg_disgust_07_v2016 <- mean(tone_2007_v2016$disgust_score)
avg_fear_07_v2016 <- mean(tone_2007_v2016$fear_score)
avg_joy_07_v2016 <- mean(tone_2007_v2016$joy_score)
avg_sadness_07_v2016 <- mean(tone_2007_v2016$sadness_score)
avg_analytical_07_v2016 <- mean(tone_2007_v2016$analytical_score)
avg_confident_07_v2016 <- mean(tone_2007_v2016$confident_score)
avg_tentative_07_v2016 <- mean(tone_2007_v2016$tentative_score)
avg_openness_07_v2016 <- mean(tone_2007_v2016$openness_score)
avg_conscientiousness_07_v2016 <- mean(tone_2007_v2016$conscientiousness_score)
avg_extraversion_07_v2016 <- mean(tone_2007_v2016$extraversion_score)
avg_agreeableness_07_v2016 <- mean(tone_2007_v2016$agreeableness_score)
avg_emotional_range_07_v2016 <- mean(tone_2007_v2016$emotional_range_score)

# Average 2008 Tones
avg_anger_08_v2016 <- mean(tone_2008_v2016$anger_score)
avg_disgust_08_v2016 <- mean(tone_2008_v2016$disgust_score)
avg_fear_08_v2016 <- mean(tone_2008_v2016$fear_score)
avg_joy_08_v2016 <- mean(tone_2008_v2016$joy_score)
avg_sadness_08_v2016 <- mean(tone_2008_v2016$sadness_score)
avg_analytical_08_v2016 <- mean(tone_2008_v2016$analytical_score)
avg_confident_08_v2016 <- mean(tone_2008_v2016$confident_score)
avg_tentative_08_v2016 <- mean(tone_2008_v2016$tentative_score)
avg_openness_08_v2016 <- mean(tone_2008_v2016$openness_score)
avg_conscientiousness_08_v2016 <- mean(tone_2008_v2016$conscientiousness_score)
avg_extraversion_08_v2016 <- mean(tone_2008_v2016$extraversion_score)
avg_agreeableness_08_v2016 <- mean(tone_2008_v2016$agreeableness_score)
avg_emotional_range_08_v2016 <- mean(tone_2008_v2016$emotional_range_score)

# Average 2009 Tones
avg_anger_09_v2016 <- mean(tone_2009_v2016$anger_score)
avg_disgust_09_v2016 <- mean(tone_2009_v2016$disgust_score)
avg_fear_09_v2016 <- mean(tone_2009_v2016$fear_score)
avg_joy_09_v2016 <- mean(tone_2009_v2016$joy_score)
avg_sadness_09_v2016 <- mean(tone_2009_v2016$sadness_score)
avg_analytical_09_v2016 <- mean(tone_2009_v2016$analytical_score)
avg_confident_09_v2016 <- mean(tone_2009_v2016$confident_score)
avg_tentative_09_v2016 <- mean(tone_2009_v2016$tentative_score)
avg_openness_09_v2016 <- mean(tone_2009_v2016$openness_score)
avg_conscientiousness_09_v2016 <- mean(tone_2009_v2016$conscientiousness_score)
avg_extraversion_09_v2016 <- mean(tone_2009_v2016$extraversion_score)
avg_agreeableness_09_v2016 <- mean(tone_2009_v2016$agreeableness_score)
avg_emotional_range_09_v2016 <- mean(tone_2009_v2016$emotional_range_score)

# Average 2010 Tones
avg_anger_10_v2016 <- mean(tone_2010_v2016$anger_score)
avg_disgust_10_v2016 <- mean(tone_2010_v2016$disgust_score)
avg_fear_10_v2016 <- mean(tone_2010_v2016$fear_score)
avg_joy_10_v2016 <- mean(tone_2010_v2016$joy_score)
avg_sadness_10_v2016 <- mean(tone_2010_v2016$sadness_score)
avg_analytical_10_v2016 <- mean(tone_2010_v2016$analytical_score)
avg_confident_10_v2016 <- mean(tone_2010_v2016$confident_score)
avg_tentative_10_v2016 <- mean(tone_2010_v2016$tentative_score)
avg_openness_10_v2016 <- mean(tone_2010_v2016$openness_score)
avg_conscientiousness_10_v2016 <- mean(tone_2010_v2016$conscientiousness_score)
avg_extraversion_10_v2016 <- mean(tone_2010_v2016$extraversion_score)
avg_agreeableness_10_v2016 <- mean(tone_2010_v2016$agreeableness_score)
avg_emotional_range_10_v2016 <- mean(tone_2010_v2016$emotional_range_score)

# Average 2011 Tones
avg_anger_11_v2016 <- mean(tone_2011_v2016$anger_score)
avg_disgust_11_v2016 <- mean(tone_2011_v2016$disgust_score)
avg_fear_11_v2016 <- mean(tone_2011_v2016$fear_score)
avg_joy_11_v2016 <- mean(tone_2011_v2016$joy_score)
avg_sadness_11_v2016 <- mean(tone_2011_v2016$sadness_score)
avg_analytical_11_v2016 <- mean(tone_2011_v2016$analytical_score)
avg_confident_11_v2016 <- mean(tone_2011_v2016$confident_score)
avg_tentative_11_v2016 <- mean(tone_2011_v2016$tentative_score)
avg_openness_11_v2016 <- mean(tone_2011_v2016$openness_score)
avg_conscientiousness_11_v2016 <- mean(tone_2011_v2016$conscientiousness_score)
avg_extraversion_11_v2016 <- mean(tone_2011_v2016$extraversion_score)
avg_agreeableness_11_v2016 <- mean(tone_2011_v2016$agreeableness_score)
avg_emotional_range_11_v2016 <- mean(tone_2011_v2016$emotional_range_score)

# Average 2012 Tones
avg_anger_12_v2016 <- mean(tone_2012_v2016$anger_score)
avg_disgust_12_v2016 <- mean(tone_2012_v2016$disgust_score)
avg_fear_12_v2016 <- mean(tone_2012_v2016$fear_score)
avg_joy_12_v2016 <- mean(tone_2012_v2016$joy_score)
avg_sadness_12_v2016 <- mean(tone_2012_v2016$sadness_score)
avg_analytical_12_v2016 <- mean(tone_2012_v2016$analytical_score)
avg_confident_12_v2016 <- mean(tone_2012_v2016$confident_score)
avg_tentative_12_v2016 <- mean(tone_2012_v2016$tentative_score)
avg_openness_12_v2016 <- mean(tone_2012_v2016$openness_score)
avg_conscientiousness_12_v2016 <- mean(tone_2012_v2016$conscientiousness_score)
avg_extraversion_12_v2016 <- mean(tone_2012_v2016$extraversion_score)
avg_agreeableness_12_v2016 <- mean(tone_2012_v2016$agreeableness_score)
avg_emotional_range_12_v2016 <- mean(tone_2012_v2016$emotional_range_score)

# Average 2013 Tones
avg_anger_13_v2016 <- mean(tone_2013_v2016$anger_score)
avg_disgust_13_v2016 <- mean(tone_2013_v2016$disgust_score)
avg_fear_13_v2016 <- mean(tone_2013_v2016$fear_score)
avg_joy_13_v2016 <- mean(tone_2013_v2016$joy_score)
avg_sadness_13_v2016 <- mean(tone_2013_v2016$sadness_score)
avg_analytical_13_v2016 <- mean(tone_2013_v2016$analytical_score)
avg_confident_13_v2016 <- mean(tone_2013_v2016$confident_score)
avg_tentative_13_v2016 <- mean(tone_2013_v2016$tentative_score)
avg_openness_13_v2016 <- mean(tone_2013_v2016$openness_score)
avg_conscientiousness_13_v2016 <- mean(tone_2013_v2016$conscientiousness_score)
avg_extraversion_13_v2016 <- mean(tone_2013_v2016$extraversion_score)
avg_agreeableness_13_v2016 <- mean(tone_2013_v2016$agreeableness_score)
avg_emotional_range_13_v2016 <- mean(tone_2013_v2016$emotional_range_score)

# Average 2014 Tones
avg_anger_14_v2016 <- mean(tone_2014_v2016$anger_score)
avg_disgust_14_v2016 <- mean(tone_2014_v2016$disgust_score)
avg_fear_14_v2016 <- mean(tone_2014_v2016$fear_score)
avg_joy_14_v2016 <- mean(tone_2014_v2016$joy_score)
avg_sadness_14_v2016 <- mean(tone_2014_v2016$sadness_score)
avg_analytical_14_v2016 <- mean(tone_2014_v2016$analytical_score)
avg_confident_14_v2016 <- mean(tone_2014_v2016$confident_score)
avg_tentative_14_v2016 <- mean(tone_2014_v2016$tentative_score)
avg_openness_14_v2016 <- mean(tone_2014_v2016$openness_score)
avg_conscientiousness_14_v2016 <- mean(tone_2014_v2016$conscientiousness_score)
avg_extraversion_14_v2016 <- mean(tone_2014_v2016$extraversion_score)
avg_agreeableness_14_v2016 <- mean(tone_2014_v2016$agreeableness_score)
avg_emotional_range_14_v2016 <- mean(tone_2014_v2016$emotional_range_score)

# Average 2015 Tones
avg_anger_15_v2016 <- mean(tone_2015_v2016$anger_score)
avg_disgust_15_v2016 <- mean(tone_2015_v2016$disgust_score)
avg_fear_15_v2016 <- mean(tone_2015_v2016$fear_score)
avg_joy_15_v2016 <- mean(tone_2015_v2016$joy_score)
avg_sadness_15_v2016 <- mean(tone_2015_v2016$sadness_score)
avg_analytical_15_v2016 <- mean(tone_2015_v2016$analytical_score)
avg_confident_15_v2016 <- mean(tone_2015_v2016$confident_score)
avg_tentative_15_v2016 <- mean(tone_2015_v2016$tentative_score)
avg_openness_15_v2016 <- mean(tone_2015_v2016$openness_score)
avg_conscientiousness_15_v2016 <- mean(tone_2015_v2016$conscientiousness_score)
avg_extraversion_15_v2016 <- mean(tone_2015_v2016$extraversion_score)
avg_agreeableness_15_v2016 <- mean(tone_2015_v2016$agreeableness_score)
avg_emotional_range_15_v2016 <- mean(tone_2015_v2016$emotional_range_score)

# Average 2016 Tones
avg_anger_16_v2016 <- mean(tone_2016_v2016$anger_score)
avg_disgust_16_v2016 <- mean(tone_2016_v2016$disgust_score)
avg_fear_16_v2016 <- mean(tone_2016_v2016$fear_score)
avg_joy_16_v2016 <- mean(tone_2016_v2016$joy_score)
avg_sadness_16_v2016 <- mean(tone_2016_v2016$sadness_score)
avg_analytical_16_v2016 <- mean(tone_2016_v2016$analytical_score)
avg_confident_16_v2016 <- mean(tone_2016_v2016$confident_score)
avg_tentative_16_v2016 <- mean(tone_2016_v2016$tentative_score)
avg_openness_16_v2016 <- mean(tone_2016_v2016$openness_score)
avg_conscientiousness_16_v2016 <- mean(tone_2016_v2016$conscientiousness_score)
avg_extraversion_16_v2016 <- mean(tone_2016_v2016$extraversion_score)
avg_agreeableness_16_v2016 <- mean(tone_2016_v2016$agreeableness_score)
avg_emotional_range_16_v2016 <- mean(tone_2016_v2016$emotional_range_score)

# Average 2017 Tones
avg_anger_17_v2016 <- mean(tone_2017_v2016$anger_score)
avg_disgust_17_v2016 <- mean(tone_2017_v2016$disgust_score)
avg_fear_17_v2016 <- mean(tone_2017_v2016$fear_score)
avg_joy_17_v2016 <- mean(tone_2017_v2016$joy_score)
avg_sadness_17_v2016 <- mean(tone_2017_v2016$sadness_score)
avg_analytical_17_v2016 <- mean(tone_2017_v2016$analytical_score)
avg_confident_17_v2016 <- mean(tone_2017_v2016$confident_score)
avg_tentative_17_v2016 <- mean(tone_2017_v2016$tentative_score)
avg_openness_17_v2016 <- mean(tone_2017_v2016$openness_score)
avg_conscientiousness_17_v2016 <- mean(tone_2017_v2016$conscientiousness_score)
avg_extraversion_17_v2016 <- mean(tone_2017_v2016$extraversion_score)
avg_agreeableness_17_v2016 <- mean(tone_2017_v2016$agreeableness_score)
avg_emotional_range_17_v2016 <- mean(tone_2017_v2016$emotional_range_score)

# Average 2018 Tones
avg_anger_18_v2016 <- mean(tone_2018_v2016$anger_score)
avg_disgust_18_v2016 <- mean(tone_2018_v2016$disgust_score)
avg_fear_18_v2016 <- mean(tone_2018_v2016$fear_score)
avg_joy_18_v2016 <- mean(tone_2018_v2016$joy_score)
avg_sadness_18_v2016 <- mean(tone_2018_v2016$sadness_score)
avg_analytical_18_v2016 <- mean(tone_2018_v2016$analytical_score)
avg_confident_18_v2016 <- mean(tone_2018_v2016$confident_score)
avg_tentative_18_v2016 <- mean(tone_2018_v2016$tentative_score)
avg_openness_18_v2016 <- mean(tone_2018_v2016$openness_score)
avg_conscientiousness_18_v2016 <- mean(tone_2018_v2016$conscientiousness_score)
avg_extraversion_18_v2016 <- mean(tone_2018_v2016$extraversion_score)
avg_agreeableness_18_v2016 <- mean(tone_2018_v2016$agreeableness_score)
avg_emotional_range_18_v2016 <- mean(tone_2018_v2016$emotional_range_score)

# Average 2019 Tones
avg_anger_19_v2016 <- mean(tone_2019_v2016$anger_score)
avg_disgust_19_v2016 <- mean(tone_2019_v2016$disgust_score)
avg_fear_19_v2016 <- mean(tone_2019_v2016$fear_score)
avg_joy_19_v2016 <- mean(tone_2019_v2016$joy_score)
avg_sadness_19_v2016 <- mean(tone_2019_v2016$sadness_score)
avg_analytical_19_v2016 <- mean(tone_2019_v2016$analytical_score)
avg_confident_19_v2016 <- mean(tone_2019_v2016$confident_score)
avg_tentative_19_v2016 <- mean(tone_2019_v2016$tentative_score)
avg_openness_19_v2016 <- mean(tone_2019_v2016$openness_score)
avg_conscientiousness_19_v2016 <- mean(tone_2019_v2016$conscientiousness_score)
avg_extraversion_19_v2016 <- mean(tone_2019_v2016$extraversion_score)
avg_agreeableness_19_v2016 <- mean(tone_2019_v2016$agreeableness_score)
avg_emotional_range_19_v2016 <- mean(tone_2019_v2016$emotional_range_score)

# Get the final dataframes for each tone
year <- c(1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)

# Anger
score <- c(avg_anger_94_v2016, avg_anger_95_v2016, avg_anger_96_v2016, avg_anger_97_v2016, avg_anger_98_v2016, avg_anger_99_v2016, avg_anger_00_v2016,
           avg_anger_01_v2016, avg_anger_02_v2016, avg_anger_03_v2016, avg_anger_04_v2016, avg_anger_05_v2016, avg_anger_06_v2016, avg_anger_07_v2016,
           avg_anger_08_v2016, avg_anger_09_v2016, avg_anger_10_v2016, avg_anger_11_v2016, avg_anger_12_v2016, avg_anger_13_v2016, avg_anger_14_v2016,
           avg_anger_15_v2016, avg_anger_16_v2016, avg_anger_17_v2016, avg_anger_18_v2016, avg_anger_19_v2016)

total_anger_v2016 <- data.frame(score, year)

total_anger_v2016 %>% ggplot(aes(x = year, y = score)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Anger (v2016)")

# Disgust
score <- c(avg_disgust_94_v2016, avg_disgust_95_v2016, avg_disgust_96_v2016, avg_disgust_97_v2016, avg_disgust_98_v2016, avg_disgust_99_v2016, avg_disgust_00_v2016,
           avg_disgust_01_v2016, avg_disgust_02_v2016, avg_disgust_03_v2016, avg_disgust_04_v2016, avg_disgust_05_v2016, avg_disgust_06_v2016, avg_disgust_07_v2016,
           avg_disgust_08_v2016, avg_disgust_09_v2016, avg_disgust_10_v2016, avg_disgust_11_v2016, avg_disgust_12_v2016, avg_disgust_13_v2016, avg_disgust_14_v2016,
           avg_disgust_15_v2016, avg_disgust_16_v2016, avg_disgust_17_v2016, avg_disgust_18_v2016, avg_disgust_19_v2016)

total_disgust_v2016 <- data.frame(score, year)

total_disgust_v2016 %>% ggplot(aes(x = year, y = score)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("disgust (v2016)")


# Fear
score <- c(avg_fear_94_v2016, avg_fear_95_v2016, avg_fear_96_v2016, avg_fear_97_v2016, avg_fear_98_v2016, avg_fear_99_v2016, avg_fear_00_v2016,
           avg_fear_01_v2016, avg_fear_02_v2016, avg_fear_03_v2016, avg_fear_04_v2016, avg_fear_05_v2016, avg_fear_06_v2016, avg_fear_07_v2016,
           avg_fear_08_v2016, avg_fear_09_v2016, avg_fear_10_v2016, avg_fear_11_v2016, avg_fear_12_v2016, avg_fear_13_v2016, avg_fear_14_v2016, 
           avg_fear_15_v2016, avg_fear_16_v2016, avg_fear_17_v2016, avg_fear_18_v2016, avg_fear_19_v2016)

total_fear_v2016 <- data.frame(score, year)

total_fear_v2016 %>% ggplot(aes(x = year, y = score)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Fear (v2016)")

# Joy
score <- c(avg_joy_94_v2016, avg_joy_95_v2016, avg_joy_96_v2016, avg_joy_97_v2016, avg_joy_98_v2016, avg_joy_99_v2016, avg_joy_00_v2016,
           avg_joy_01_v2016, avg_joy_02_v2016, avg_joy_03_v2016, avg_joy_04_v2016, avg_joy_05_v2016, avg_joy_06_v2016, avg_joy_07_v2016,
           avg_joy_08_v2016, avg_joy_09_v2016, avg_joy_10_v2016, avg_joy_11_v2016, avg_joy_12_v2016, avg_joy_13_v2016, avg_joy_14_v2016,
           avg_joy_15_v2016, avg_joy_16_v2016, avg_joy_17_v2016, avg_joy_18_v2016, avg_joy_19_v2016)

total_joy_v2016 <- data.frame(score, year)

total_joy_v2016 %>% ggplot(aes(x = year, y = score)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Joy (v2016)")

# Sadness
score <- c(avg_sadness_94_v2016, avg_sadness_95_v2016, avg_sadness_96_v2016, avg_sadness_97_v2016, avg_sadness_98_v2016, avg_sadness_99_v2016, avg_sadness_00_v2016,
           avg_sadness_01_v2016, avg_sadness_02_v2016, avg_sadness_03_v2016, avg_sadness_04_v2016, avg_sadness_05_v2016, avg_sadness_06_v2016, avg_sadness_07_v2016,
           avg_sadness_08_v2016, avg_sadness_09_v2016, avg_sadness_10_v2016, avg_sadness_11_v2016, avg_sadness_12_v2016, avg_sadness_13_v2016, avg_sadness_14_v2016, 
           avg_sadness_15_v2016, avg_sadness_16_v2016, avg_sadness_17_v2016, avg_sadness_18_v2016, avg_sadness_19_v2016)

total_sadness_v2016 <- data.frame(score, year)

total_sadness_v2016 %>% ggplot(aes(x = year, y = score)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Sadness (v2016)")

# Analytical
score <- c(avg_analytical_94_v2016, avg_analytical_95_v2016, avg_analytical_96_v2016, avg_analytical_97_v2016, avg_analytical_98_v2016, avg_analytical_99_v2016, avg_analytical_00_v2016,
           avg_analytical_01_v2016, avg_analytical_02_v2016, avg_analytical_03_v2016, avg_analytical_04_v2016, avg_analytical_05_v2016, avg_analytical_06_v2016, avg_analytical_07_v2016, 
           avg_analytical_08_v2016, avg_analytical_09_v2016, avg_analytical_10_v2016, avg_analytical_11_v2016, avg_analytical_12_v2016, avg_analytical_13_v2016, avg_analytical_14_v2016,
           avg_analytical_15_v2016, avg_analytical_16_v2016, avg_analytical_17_v2016, avg_analytical_18_v2016, avg_analytical_19_v2016)

total_analytical_v2016 <- data.frame(score, year)

total_analytical_v2016 %>% ggplot(aes(x = year, y = score)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Analytical (v2016)")

# Confident
score <- c(avg_confident_94_v2016, avg_confident_95_v2016, avg_confident_96_v2016, avg_confident_97_v2016, avg_confident_98_v2016, avg_confident_99_v2016, avg_confident_00_v2016,
           avg_confident_01_v2016, avg_confident_02_v2016, avg_confident_03_v2016, avg_confident_04_v2016, avg_confident_05_v2016, avg_confident_06_v2016, avg_confident_07_v2016,
           avg_confident_08_v2016, avg_confident_09_v2016, avg_confident_10_v2016, avg_confident_11_v2016, avg_confident_12_v2016, avg_confident_13_v2016, avg_confident_14_v2016,
           avg_confident_15_v2016, avg_confident_16_v2016, avg_confident_17_v2016, avg_confident_18_v2016, avg_confident_19_v2016)

total_confident_v2016 <- data.frame(score, year)

total_confident_v2016 %>% ggplot(aes(x = year, y = score)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Confident (v2016)")

# Tentative
score <- c(avg_tentative_94_v2016, avg_tentative_95_v2016, avg_tentative_96_v2016, avg_tentative_97_v2016, avg_tentative_98_v2016, avg_tentative_99_v2016, avg_tentative_00_v2016,
           avg_tentative_01_v2016, avg_tentative_02_v2016, avg_tentative_03_v2016, avg_tentative_04_v2016, avg_tentative_05_v2016, avg_tentative_06_v2016, avg_tentative_07_v2016,
           avg_tentative_08_v2016, avg_tentative_09_v2016, avg_tentative_10_v2016, avg_tentative_11_v2016, avg_tentative_12_v2016, avg_tentative_13_v2016, avg_tentative_14_v2016,
           avg_tentative_15_v2016, avg_tentative_16_v2016, avg_tentative_17_v2016, avg_tentative_18_v2016, avg_tentative_19_v2016)

total_tentative_v2016 <- data.frame(score, year)

total_tentative_v2016 %>% ggplot(aes(x = year, y = score)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Tentative (v2016)")

# Significance tests

anger_sig_v2016 <- lm(data = total_anger_v2016, score ~ year)
summary(anger_sig_v2016) # p-value: 0.4988

disgust_sig_v2016 <- lm(data = total_disgust_v2016, score ~ year)
summary(disgust_sig_v2016) # p-value: 0.005833

fear_sig_v2016 <- lm(data = total_fear_v2016, score ~ year)
summary(fear_sig_v2016) # p-value: 0.01596

joy_sig_v2016 <- lm(data = total_joy_v2016, score ~ year)
summary(joy_sig_v2016) # p-value: 0.6591

sadness_sig_v2016 <- lm(data = total_sadness_v2016, score ~ year)
summary(sadness_sig_v2016) # p-value: 0.1308

analytical_sig_v2016 <- lm(data = total_analytical_v2016, score ~ year)
summary(analytical_sig_v2016) # p-value: 0.41322

confident_sig_v2016 <- lm(data = total_confident_v2016, score ~ year)
summary(confident_sig_v2016) # p-value: 0.17675

tentative_sig_v2016 <- lm(data = total_tentative_v2016, score ~ year)
summary(tentative_sig_v2016) # p-value: 0.886113