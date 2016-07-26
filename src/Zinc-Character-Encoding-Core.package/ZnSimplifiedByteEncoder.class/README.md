I am ZnSimplifiedByteEncoder, a concrete subclass of ZnCharacterEncoder.
I handle single byte encodings where byte values 0 to 127 map to ASCII and 128 to 255 are a permutation to Unicode characters.

I am like ZnByteEncoder, a subclass of me, but I implement just two mappings, latin1 or iso-8859-1 and ASCII, to conserve memory.