accessing
keywordsPositions
	^keywordsPositions ifNil: [ (selector keywords collect: [:char| 0 ]) asIntegerArray ]