character shapes
makeLfInvisible
	self characterToGlyphMap.
	characterToGlyphMap at: 11 put: (11 < minAscii ifFalse: [11] ifTrue: [maxAscii+1])