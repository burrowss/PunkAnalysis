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

""" Single Song search """
# song = genius.search_song("Cardinals", artist.name)
# print(song.lyrics)

""" Search by artist """

artist = genius.search_artist("The Wonder Years", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()


""" The Front Bottoms """

artist = genius.search_artist("The Front Bottoms", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Green Day """

artist = genius.search_artist("Green Day", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Blink-182 """

artist = genius.search_artist("Blink-182", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Neck Deep """

artist = genius.search_artist("Neck Deep", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Trophy Eyes """

artist = genius.search_artist("Trophy Eyes", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" PUP """

artist = genius.search_artist("PUP", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" A Day to Remember """

artist = genius.search_artist("A Day to Remember", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Modern Baseball """

artist = genius.search_artist("Modern Baseball", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Real Friends """

artist = genius.search_artist("Real Friends", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Sum 41 """

artist = genius.search_artist("Sum 41", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Tigers Jaw """

artist = genius.search_artist("Tigers Jaw", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Trash Boat """

artist = genius.search_artist("Trash Boat", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" WSTR """

artist = genius.search_artist("WSTR", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Brand New """

artist = genius.search_artist("Brand New", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" The Menzingers """

artist = genius.search_artist("The Menzingers", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" State Champs """

artist = genius.search_artist("State Champs", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Fall Out Boy """

artist = genius.search_artist("Fall Out Boy", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" The Story So Far """

artist = genius.search_artist("The Story So Far", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Joyce Manor """

artist = genius.search_artist("Joyce Manor", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

# Others: Tiny Moving Parts, Panic! At The Disco, The Offspring,
# My Chemical Romance, Moose Blood, Knuckle Puck
# 20 artists, 50 songs = 1000
