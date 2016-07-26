testing
hasGlyphsForAll: asciiString
	"Answer true if the receiver has glyphs for all the characters
	in asciiString, false otherwise.
	The default behaviour is to answer true, but subclasses may reimplement"
	
	self face isValid ifFalse:[^false].
	asciiString do:[:c |
		(self face primGetCharIndex: c asInteger) = 0
			ifTrue:[^false]].
	^true