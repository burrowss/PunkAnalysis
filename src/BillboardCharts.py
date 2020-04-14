import billboard
import json
import re
import csv
from dateutil.parser import parse
""" Program used to scrape popularity data (Billboard 200),
find matches, and retrieve the rankings. """

# ChartData(name, date=None, fetch=True, timeout=25)
# name = billboard-200, date = Upper date (YYYY-MM-DD)
# test dates = 10/31/2016, 7/2/2011
chart = billboard.ChartData(
    'billboard-200', date='1995-01-01', fetch=True, timeout=25)
x = 0
all_charts = []

# all weeks from 2019 to 1994 (52 x 25) = 1300
# Need to run this a couple of years at a time so it doesnt timeout

# chart.date is lower date
while chart.date >= '1994-01-01':
    chart_list = []
    chart_title = []
    chart_artist = []
    chart_ranking = []
    final_chart = []

    for line in chart:
        chart_list.append(str(line))

    # Finding the titles and artists in charts
    i = 1

    chart_date = chart.date

    for entry in chart_list:
        chart_ranking.append([i])
        chart_title.append((re.findall("'(.*)'", entry)))
        chart_artist.append(re.findall("(?<= by ).*$", entry))
        i += 1

    # Removing inner lists using nested for loops
    chart_title = [val for sublist in chart_title for val in sublist]

    chart_artist = [val for sublist in chart_artist for val in sublist]

    chart_ranking = [val for sublist in chart_ranking for val in sublist]

    # Combining all aspects of the charts

    final_chart = list(zip(chart_ranking, chart_title, chart_artist))

    final_chart.append(chart_date)

    all_charts.append(final_chart)

    chart = billboard.ChartData('billboard-200', chart.previousDate)

# print(all_charts)
album_list = []
artist_list = []

lyrics = '../src/data/all_lyrics.json'

# Gets all the albums and artists being evaluated
with open(lyrics, 'r') as lyrics_json:
    data = json.load(lyrics_json)
    length = len(data)
    for entry in range(length):
        album_list.append(data[entry]['album'])
        artist_list.append(data[entry]['artist'])

# Creates a dictionary of Pop-punk albums and their artists

data_list = list(zip(album_list, artist_list))
data_list = list(set(data_list))

# Function that returns whether the string can be interpreted as a date.

# :param string: str, string to check for date
# :param fuzzy: bool, ignore unknown tokens in string if True


def is_date(string, fuzzy=False):
    try:
        parse(string, fuzzy=fuzzy)
        return True

    except ValueError:
        return False


match_dict = []
temp_album = []
temp_artist = []
ranking = []
final_list = []
data_strings = []
match_strings = []
found_matches = []
ranking_list = []

# Gets a list of album and artist from Billboard 200
for week in all_charts:
    for entry in week:
        ranking.append(str(entry))
        temp_album.append(entry[1])
        temp_artist.append(entry[2])

# List of Billboard 200
match_list = list(zip(temp_album, temp_artist))

# Lower case for comparison
for element in data_list:
    data_strings.append(str(element).lower())

for element in match_list:
    match_strings.append(str(element).lower())

# Finding matches
for element in data_strings:
    if element in match_strings:
        found_matches.append(element)

# Make ranking strings for comparing to matches
# Gets the ranking/original list for matching songs and date
# Date of album ranking is the date after the album (or final album if multiple in one week)
for entry in ranking:
    entry = entry.lower()
    for match in found_matches:
        match = match.strip("'()'")
        if (match in entry):
            entry = entry.strip("'()'")
            final_list.append(entry)
    if (is_date(str(entry))):
        final_list.append(str(entry))

# Writing the final list to a CSV
with open("../src/data/1994_AlbumRankings.csv", 'w') as ranking_file:
    wr = csv.writer(ranking_file, delimiter='\n')
    wr.writerow(final_list)
