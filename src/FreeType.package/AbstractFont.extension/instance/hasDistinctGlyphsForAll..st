*FreeType-addition
hasDistinctGlyphsForAll: asciiString
	"Answer true if the receiver has glyphs for all the characters
	in asciiString and no single glyph is shared by more than one character, false otherwise.
	The default behaviour is to answer true, but subclasses may reimplement"
	
	^true