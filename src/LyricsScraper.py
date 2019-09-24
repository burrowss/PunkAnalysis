import lyricsgenius

with open("AccessToken.txt", "r") as file:
    token = file.read().replace('\n', '')

genius = lyricsgenius.Genius(token)

# Turn off status messages
genius.verbose = False

# Remove section headers (e.g. [Chorus]) from lyrics when searching
genius.remove_section_headers = True

# Exclude songs with these words in their title
genius.excluded_terms = ["(Remix)", "(Live)"]

# Include hits thought to be non-songs (e.g. track lists)
genius.skip_non_songs = True

""" Search by artist """

artist = genius.search_artist("The Wonder Years", max_songs=3, sort="title")
# print(artist.songs)

""" Single Song search """
# song = genius.search_song("Cardinals", artist.name)
# print(song.lyrics)
song = genius.search_song("", artist.name)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

print(artist.songs)
