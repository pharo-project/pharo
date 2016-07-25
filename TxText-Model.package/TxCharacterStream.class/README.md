I am a specialized read-only stream for extracting  characters out of text. Text may hold any objects, not just characters. I will filter out those that can not be represented by characters.

Please note: modifying the text while using gives undefined results, since any modification of text invalidates all external positions. Thus, special care must be taken to reinitialize my position to a valid  position. Handling the position updates is, of course, not the responsibility of this classs, since it cannot know, nor predict, what kind of modification it may be and where it will occur and how it will affect the position it holds.

The only responsibility of this class is to provide a convenient stream for reading character data from text.