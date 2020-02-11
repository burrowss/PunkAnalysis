import json
from ibm_watson import ToneAnalyzerV3
from ibm_cloud_sdk_core.authenticators import IAMAuthenticator

# Reading token
with open("ibm-credentials.txt", "r") as file:
    token = file.read().replace('\n', '')

# Reading IBM URL
with open("ibm-url.txt", "r") as file:
    url = file.read().replace('\n', '')

# Authenticates and sets service to ToneAnalyzer
authenticator = IAMAuthenticator(token)
service = ToneAnalyzerV3(
    version='2017-09-21',
    authenticator=authenticator)
service.set_service_url(url)

# Establishing directory path
lyrics = '../src/data/1994-2007_lyrics.json'

final_dict = []
counter = 0


with open(lyrics, 'r') as lyrics_json:
    song_count = json.load(lyrics_json)

for line in song_count:
    counter += 1

# print(counter)
i = 0

while i < counter:
    with open(lyrics, 'r') as lyrics_json:
            tone = service.tone(
                tone_input=json.load(lyrics_json)[i]['lyrics'],
                content_type='text/plain',
                sentences=False).get_result()

            final_dict.append(tone)
    i += 1
# print(final_dict)

with open('1994-2007_tone_data_SONGSONLY.json', 'a') as output:
    json.dump(final_dict, output)
