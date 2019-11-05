import lyricsgenius

with open("AccessToken.txt", "r") as file:
    token = file.read().replace('\n', '')

genius = lyricsgenius.Genius(token)

# Turn off status messages
genius.verbose = False

# Remove section headers (e.g. [Chorus]) from lyrics when searching
genius.remove_section_headers = True

# Exclude songs with these words in their title
genius.excluded_terms = ["(Remix)", "(Live)", "(Acoustic)",
                         "(Demo)" "Acoustic", "Demo", "Remix", "Live"]

# Include hits thought to be non-songs (e.g. track lists)
genius.skip_non_songs = True

""" Single Song search """
# song = genius.search_song("Cardinals", artist.name)
# print(song.lyrics)

""" Artist Search """
#
# artist = genius.search_artist(
#     "The Wonder Years", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
""" Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()

# """ Discount """
#
# artist = genius.search_artist(
#     "Discount", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ RVIVR """
#
# artist = genius.search_artist(
#     "RVIVR", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ 5 Seconds of Summer """
#
# artist = genius.search_artist(
#     "5 Seconds of Summer", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ Joyce Manor """
#
# artist = genius.search_artist(
#     "Joyce Manor", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ Good Charlotte """
#
# artist = genius.search_artist(
#     "Good Charlotte", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ The Distillers """
#
# artist = genius.search_artist(
#     "The Distillers", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ The Ataris """
#
# artist = genius.search_artist(
#     "The Ataris", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ Lagwagon """
#
# artist = genius.search_artist(
#     "Lagwagon", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ The Wonder Years """
#
# artist = genius.search_artist(
#     "The Wonder Years", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ Bouncing Souls """
#
# artist = genius.search_artist(
#     "Bouncing Souls", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ Yellowcard """
#
# artist = genius.search_artist(
#     "Yellowcard", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ Saves the Day """
#
# artist = genius.search_artist(
#     "Saves the Day", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ The Ergs """
#
# artist = genius.search_artist(
#     "The Ergs", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()
#
# """ Simple Plan """
#
# artist = genius.search_artist(
#     "Simple Plan", max_songs=50, sort="popularity")
# song = genius.search_song("", artist.name)
# # print(artist.songs)
#
# """ Adding songs to artist object """
# artist.add_song(song)
#
# artist.save_lyrics()

""" Lifetime """

artist = genius.search_artist(
    "Lifetime", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Tsunami Bomb """

artist = genius.search_artist(
    "Tsunami Bomb", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" AFI """

artist = genius.search_artist(
    "AFI", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" MxPx """

artist = genius.search_artist(
    "MxPx", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Jimmy Eat World """

artist = genius.search_artist(
    "Jimmy Eat World", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Alkaline Trio """

artist = genius.search_artist(
    "Alkaline Trio", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" All Time Low """

artist = genius.search_artist(
    "All Time Low", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Bad Religion """

artist = genius.search_artist(
    "Bad Religion", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Rancid """

artist = genius.search_artist(
    "Rancid", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Green Day """

artist = genius.search_artist(
    "Green Day", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Sum 41 """

artist = genius.search_artist(
    "Sum 41", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" New Found Glory """

artist = genius.search_artist(
    "New Found Glory", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" The Offspring """

artist = genius.search_artist(
    "The Offspring", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Jawbreaker """

artist = genius.search_artist(
    "Jawbreaker", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" NOFX """

artist = genius.search_artist(
    "NOFX", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Paramore """

artist = genius.search_artist(
    "Paramore", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Blink-182 """

artist = genius.search_artist(
    "Blink-182", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()

""" Fall Out Boy """

artist = genius.search_artist(
    "Fall Out Boy", max_songs=50, sort="popularity")
song = genius.search_song("", artist.name)
# print(artist.songs)

""" Adding songs to artist object """
artist.add_song(song)

artist.save_lyrics()
