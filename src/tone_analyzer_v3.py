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
lyrics = '../src/data/2008-2019_lyrics.json'

final_dict = []


i = 0
while i < 657:
    with open(lyrics, 'r') as tone_json:
        tone = service.tone(
            tone_input=json.load(tone_json)[i]['lyrics'],
            content_type='text/plain',
            sentences=True).get_result()

        final_dict.append(tone)
    i += 1

with open('2008-2019_tone_data.json', 'a') as output:
    json.dump(final_dict, output)
