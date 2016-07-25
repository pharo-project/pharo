I am ZnByteEncoder, a concrete subclass of ZnCharacterEncoder.
I handle single byte encodings where byte values 0 to 127 map to ASCII and 128 to 255 are a permutation to Unicode characters.

I derive my mappings by parsing official unicode.org specifications.

The list of encodings and their names/aliases was taken from http://encoding.spec.whatwg.org/#legacy-single-byte-encodings

I basically support ISO/IEC 8859 1, 2, 3, 4, 5, 6, 7, 8, 10, 13, 14, 15 and 16, Windows Codepages 866, 874, 1250, 1251, 1252, 1253, 1253, 1254, 1255, 1256, 1257, 1258, KOI8 R & U as well as Mac Roman & Cyrillic - each of these with a number of aliases like latin1, latin2, latin3, latin4, latin5, latin6, cyrillic, arabic, greek and hebrew. See #mappingToIdentifiers

Note that most/all of these encodings should be considered legacy, with UTF-8 being the preferred encoding going forward.  

Part of Zinc HTTP Components.