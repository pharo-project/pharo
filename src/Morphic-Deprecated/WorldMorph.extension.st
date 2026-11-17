Extension { #name : 'WorldMorph' }

{ #category : '*Morphic-Deprecated' }
WorldMorph class >> scaleFactor [

	self
		deprecated: 'Use #zoomFactor instead.'
		transformWith: '`@rcv scaleFactor' -> '`@rcv zoomFactor'.
	^ self zoomFactor
]

{ #category : '*Morphic-Deprecated' }
WorldMorph class >> scaleFactor: aValue [

	self
		deprecated: 'Use #zoomFactor: instead.'
		transformWith: '`@rcv scaleFactor: `@arg1' -> '`@rcv zoomFactor: `@arg1'.
	^ self zoomFactor: aValue
]
