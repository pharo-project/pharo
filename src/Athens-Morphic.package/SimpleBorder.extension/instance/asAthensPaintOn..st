*Athens-Morphic
asAthensPaintOn: anAthensCanvas
	^ self topLeftColor = self bottomRightColor
		ifTrue: [ super asAthensPaintOn: anAthensCanvas ]
		ifFalse: [ self asTwoColorStrokePaintOn: anAthensCanvas ]