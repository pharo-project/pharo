Extension { #name : 'WorldMorph' }

{ #category : '*Morphic-Deprecated' }
WorldMorph class >> scaleFactor [

	self
		deprecated: 'Use #zoomFactor instead.'
		on: '2025-10-29'
		in: #Pharo14
		transformWith: '`@rcv scaleFactor' -> '`@rcv zoomFactor'.
	^ self zoomFactor
]

{ #category : '*Morphic-Deprecated' }
WorldMorph class >> scaleFactor: aValue [

	self
		deprecated: 'Use #zoomFactor: instead.'
		on: '2025-10-29'
		in: #Pharo14
		transformWith: '`@rcv scaleFactor: `@arg1' -> '`@rcv zoomFactor: `@arg1'.
	^ self zoomFactor: aValue
]
