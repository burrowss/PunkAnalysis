
# Punk's Not Dead, it Just Got Sadder: Sentiment Analysis of Recent Pop-Punk Songs

## Simon Burrows

### PunkAnalysis

In order to utilize this project, first clone the repository. You can recreate the visualizations and analyses of my project by using the data provided and the programs as they are now. If new data is
desired, there is quite a lot of manual changes that are required. These are specified

## Data Collection

### LyricsScraper.py

Each file of the source code has its own specific use. In order to recreate, or to perform
a similar project, there is an order that should be followed. First is to gather the
initial lyrical data. In order to do so, you must have an API authentication token
from Genius. Follow the instructions at the Genius Lyrics API documentation to do
so. Once completed, save this key to a file and call name it `AccessToken.txt`. Next,
in the `LyricsScraper.py` file, each artist that is downloaded is a block of code that
looks as follows:

    """ The Wonder Years """
    artist = genius.search_artist(
    "The Wonder Years " , max_songs=50, sort="popularity")
    song = genius.search_song("", artist.name)
    print(artist.songs)
    """ Adding songs to artist object """
    artist.add_song(song)
    artist.save_lyrics( )

To download the top 50 popular songs of an artist, simply replace the first parameter in the artist object ("The Wonder Years" in this example). Do this for each artist that is desired. After the artists have been set, run `python LyricsScraper.py` to download the songs. It will download each one as its own JSON file in a separate
folder. These files can be combined by using the jq library, which can be installed
on your machine. For Linux systems, `jq -s '{ songs: map(.songs[0]) }' \*.json`
is the command that I used to combine the files. It outputs into the terminal which
can then be pasted into a new file. Finally, the completed JSON file should be transformed into a CSV, which can be done using ConvertCSV.com. This is a point at which manual verification is recommended in order to ensure the data is correct and complete.

### BillboardCharts.py

Following the creation of the CSV, `BillboardCharts.py` is the next program to be
run. It is how the popularity data, the rankings, are obtained by using the billboard
library. In this program, you must specify different dates to retrieve. The most recent
date that is being retrieved is in the second parameter of the chart variable:

    chart = billboard.ChartData (
    'billboard−200', date ='1995−01−01', fetch=True, timeout=25)

The last date that is retrieved is at the start of the first while loop:
`while chart.date >= '1994−01−01':`
There is a known issue with this library where it may fail to download all the rankings for older years (before 2010) and so smaller intervals are recommended the further
back in time you are retrieving. This program will find the rankings for albums being
analyzed by finding said albums in the Billboard charts and returning the chart and
its date. A list of each album, its ranking, and the date is created and written to a
CSV, which can be renamed at the bottom of the program's code. Once the CSV file
has been created, it is again recommended to manually verify the data. Because the
program is finding exact matches for the names of the albums, it is possible for some
rankings to be missed. This is often due to additional words in the album titles such
as "Deluxe Edition" or "Live from CITY/COUNTRY". This is a threat to validity
and room for improvement that is discussed further in a future chapter. It is then
recommended to add the rankings to the previously created lyrics CSV(s). For my use,
I took the highest ranking for each album and then assigned those values to each song
from the album. If there was no ranking, I assigned the value to "NA".


### ToneAnalyzer.py

To begin the tonal analysis, you must run IBM Tone Analyzer on the lyrical data.
In order to do this, however, an IBM authentication token and URL are needed. To
get one, follow instructions at the IBM Tone Analyzer documentation. Save the token
into `imb-credentials.txt` and the URL into `ibm-url.txt` or rename the files in the
program. To load the lyrics, put the desired file into the directory path for the lyrics
variable (i.e. `../src/data/all_lyrics.json`). Additionally, change the file name
for the output JSON at the end of the program to what you desire. To finally perform
the tonal analysis, simply run the program (i.e. in Linux: `python ToneAnalyzer.py`).
This will output a resultant JSON file with the tonal analysis for each song in the
initial JSON file. For each song, there are tones and scores associated with them. The
scores range from 0 to 1.0, with respect to the percentage of confidence (0% - 100%) of
the presence of the tone. A score of at least 0.5 indicates the presence of a given tone,
while a score greater than 0.75 indicates a high likelihood that the tone is perceived
in the content


## Analyses in R

To recreate the visualizations and analyses used in my thesis, you will need to
use R Studio. First, source the `LyricsAnalysisInstall.R` file to install the
necessary packages.

Then depending on what analysis is desired, you will need to source a different
file.

For sentiment analysis, run `LyricsAnalysis.R` and when prompted to
select a file, choose `PunkAnalysis/src/data/all_lyrics.csv`.
It will produce the graphs and statistical analyses using sentiment data from
various lexicons in `tidytext`.

For tonal analysis, run `TonalAnalysis2016.R` and when prompted to
select a file, choose `PunkAnalysis/src/tone_resources/all_tone_data_v2016.csv`.
It will produce the graphs and statistical analyses using tone data that was
previously collected from IBM ToneAnalyzer.

For popularity analysis, run `PopularityAnalysis.R` and when prompted to
select a file, choose `PunkAnalysis/src/data/all_lyrics.csv`.
It will produce the graphs and statistical analyses using ranking data that was
previously collected from Billboard charts.
