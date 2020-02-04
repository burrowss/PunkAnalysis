import billboard
import json
import re
import csv
from dateutil.parser import parse

""" Program used to scrape popularity data (Billboard 200),
find matches, and retrieve the rankings. """

# ChartData(name, date=None, fetch=True, timeout=25)
# name = billboard-200, date = 1994 to 2019 (YYYY-MM-DD)
# test dates = 10/31/2016, 7/2/2011
chart = billboard.ChartData(
    'billboard-200', date='2016-10-31', fetch=True, timeout=25)

x = 0
all_charts = []

# x = all weeks from 2019 to 1994 (52 x 25) = 1300
while x < 2:
    chart_list = []
    chart_title = []
    chart_artist = []
    chart_ranking = []
    final_chart = []

    chart = billboard.ChartData('billboard-200', chart.previousDate)
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

    x += 1
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

data_dict = dict(zip(album_list, artist_list))

# Matches albums in from charts to albums being evaluated

# all_charts = List of lists -> list (week) ->
# -> Entry of list (138, 'California', 'Blink-182') -> Element of entry
# Need second two Elements of Entry to Compare, then add that ENTRY


"""
Function that returns whether the string can be interpreted as a date.

:param string: str, string to check for date
:param fuzzy: bool, ignore unknown tokens in string if True
"""


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

# Gets a dictionary of album and artist from Billboard 200
for week in all_charts:
    for entry in week:
        ranking.append(entry)
        for item in range(len(entry)):
            temp_album.append(entry[1])
            temp_artist.append(entry[2])

# Dictionary of Billboard 200
match_dict = dict(zip(temp_album, temp_artist))

# if both dictionaries match, add to matching_list (album and artist)
matching_list = dict(match_dict.items() & data_dict.items())

# Gets the ranking/original list for matching songs and date
for entry in ranking:
    for match in matching_list:
        if (match in entry):
            entry = str(entry).strip("'()'")
            entry = entry.replace("'", '')
            final_list.append(entry)
    if(is_date(str(entry))):
        final_list.append(str(entry))

# print(final_list)

with open("../src/data/AlbumRankings.csv", 'w') as ranking_file:
    wr = csv.writer(ranking_file, delimiter='\n')
    wr.writerow(final_list)
