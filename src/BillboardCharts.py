import billboard
import json
import re

# ChartData(name, date=None, fetch=True, timeout=25)
# name = billboard-200, date = 1994 to 2019 (YYYY-MM-DD)
chart = billboard.ChartData(
    'billboard-200', date='2019-12-31', fetch=True, timeout=25)

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
print(all_charts)

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

# Creates a dictionary of albums and their artists
# 269 albums

final_dict = dict(zip(album_list, artist_list))

# Matches albums in from charts to albums being evaluated

# Add to conjoined list

# chart_list.append(album_list[i])
#
# # For album in all_lyrics.json
#     # if album in chart
#     # add album to list
#    i += 1

# print(chart_list)
