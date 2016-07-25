testing
hasDistinctGlyphsForAll: asciiString
	"Answer true if the receiver has glyphs for all the characters
	in asciiString and no single glyph is shared by more than one character, false otherwise.
	The default behaviour is to answer true, but subclasses may reimplement"
	| setOfIndices |
	self face isValid ifFalse:[^false].
	setOfIndices := Set new.
	asciiString asSet do:[:c | | i |
		(i := self face primGetCharIndex: c asInteger) = 0
			ifTrue:[^false]
			ifFalse:[
				(setOfIndices includes: i)
					ifTrue:[^false]
					ifFalse:[setOfIndices add: i]]].
	^true