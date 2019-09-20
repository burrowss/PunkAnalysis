import lyricsgenius
from AccessToken import token

genius = lyricsgenius.Genius(token)

genius.verbose = False  #Turn off status messages
genius.remove_section_headers = True  # Remove section headers (e.g. [Chorus]) from lyrics when searching
genius.excluded_terms = ["(Remix)", "(Live)"]  # Exclude songs with these words in their title
genius.skip_non_songs = True  # Include hits thought to be non-songs (e.g. track lists)

artist = genius.search_artist("Andy Shauf", max_songs=3, sort="title")
print(artist.songs)

song = genius.search_song("To You", artist.name)
print(song.lyrics)

artist.add_song(song)

artist.save_lyrics()
