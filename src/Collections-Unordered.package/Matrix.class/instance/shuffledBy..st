copying
shuffledBy: aRandom
	^self class rows: numberOfRows columns: numberOfColumns contents: (contents copy shuffleBy: aRandom)