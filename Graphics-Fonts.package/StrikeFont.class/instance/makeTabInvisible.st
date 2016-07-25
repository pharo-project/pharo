character shapes
makeTabInvisible
	self characterToGlyphMap.
	characterToGlyphMap at: 10 put: (10 < minAscii ifFalse: [10] ifTrue:[maxAscii+1])