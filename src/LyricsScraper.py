from bs4 import BeautifulSoup
import requests

URL = 'https://genius.com/Andy-shauf-the-magician-lyrics'
page = requests.get(URL)
# Extract the page's HTML as a string
html = BeautifulSoup(page.text, "html.parser")

# Scrape the song lyrics from the HTML
lyrics = html.find("div", class_="lyrics").get_text()
