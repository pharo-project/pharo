Extension { #name : 'Array2D' }

{ #category : '*Random-Core' }
Array2D >> shuffled [
	^self class rows: numberOfRows columns: numberOfColumns contents: (contents shuffled)
]

{ #category : '*Random-Core' }
Array2D >> shuffledBy: aRandom [
	^self class rows: numberOfRows columns: numberOfColumns contents: (contents copy shuffleBy: aRandom)
]
