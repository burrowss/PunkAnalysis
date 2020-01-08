import billboard

# ChartData(name, date=None, fetch=True, timeout=25)
# name = billboard-200, date = 1994 to 2019 (YYYY-MM-DD)
chart = billboard.ChartData('billboard-200', date='2019-12-31', fetch=True, timeout=25)

i = 0
while i < 5:
    print(chart)
    chart = billboard.ChartData('billboard-200', chart.previousDate)
    # For album in all_lyrics.json
        # if album in chart
            # add album to list
    i += 1
