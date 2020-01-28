import billboard
import json
import re

# ChartData(name, date=None, fetch=True, timeout=25)
# name = billboard-200, date = 1994 to 2019 (YYYY-MM-DD)
# test dates = 10/31/2016, 7/2/2011
chart = billboard.ChartData(
    'billboard-200', date='2016-10-31', fetch=True, timeout=25)

x = 0
all_charts = []

# x = all weeks from 2019 to 1994 (52 x 25) = 1300
while x < 1:
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
final_list = []

lyrics = '../src/data/all_lyrics.json'

# Gets all the albums and artists being evaluated
with open(lyrics, 'r') as lyrics_json:
    data = json.load(lyrics_json)
    length = len(data)
    for entry in range(length):
        album_list.append(data[entry]['album'])
        artist_list.append(data[entry]['artist'])

# Creates a dictionary of albums and their artists
# 269 albums

data_dict = dict(zip(album_list, artist_list))
# print(data_dict)
# print()


# Matches albums in from charts to albums being evaluated
# look at each album in all charts. If the second two values match the values from dictionary, add to list

# Issue: List of lists -> list (week) -> Entry of list (138, 'California', 'Blink-182') -> Element of entry
# Need second two Elements of Entry to Compare, then add that ENTRY

match_dict = []
temp_album = []
temp_artist = []

# Gets a dictionary of album and artist

# \'([^\"]*?[^\"]*?)\' or \"([^\"]*?[^\"]*?)\"
for week in all_charts:
    for entry in week:
        print(entry)
        for item in range(len(entry)):
            temp_album.append(entry[1])
            temp_artist.append(entry[2])


match_dict = dict(zip(temp_album, temp_artist))
# print(match_dict)
# print()

# if both dictionaries match, add to final_list
matching_list = (match_dict.items() & data_dict.items())
print(matching_list)

# Combine matching list and entry of all_charts (level 2) for ranking. Add to last list
for entry in matching_list:
    for item in range(len(entry)):
        if (entry[0] and entry[1]):
            print(item)
# print(final_list)
