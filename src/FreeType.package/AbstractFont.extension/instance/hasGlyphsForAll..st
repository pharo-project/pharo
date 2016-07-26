*FreeType-addition
hasGlyphsForAll: asciiString
	"Answer true if the receiver has glyphs for all the characters
	in asciiString, false otherwise.
	The default behaviour is to answer true, but subclasses may reimplement"
	
	^true