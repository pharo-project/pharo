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

{ #category : '*Morphic-Deprecated' }
WorldMorph >> scaleFactor: newZoomFactor [

	self
		deprecated: 'Use #zoomFactor: instead'
		on: '20/12/2024'
		in: 'pharo13'
		transformWith:
		'`@receiver scaleFactor: `@arg' -> '`@receiver zoomFactor: `@arg'.
	self zoomFactor: newZoomFactor
]
