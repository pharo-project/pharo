CharacterSetComplement is a space efficient implementation of (CharacterSet complement) taking care of WideCharacter (code > 255)

However, it will maintain a byteArrayMap for character <= 255 in a cache keeping 

instance variables:
	absent <CharacterSet> contains character that are not in the set (i.e. my complement)
	byteArrayMapCache <ByteArray | nil> cache this information because it has to be used in tight loops where efficiency matters