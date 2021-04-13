Extension { #name : #SimpleBorder }

{ #category : #'*Athens-Morphic' }
SimpleBorder >> asAthensPaintOn: anAthensCanvas [
	^ self topLeftColor = self bottomRightColor
		ifTrue: [ super asAthensPaintOn: anAthensCanvas ]
		ifFalse: [ self asTwoColorStrokePaintOn: anAthensCanvas ]
]

{ #category : #'*Athens-Morphic' }
SimpleBorder >> asTwoColorStrokePaintOn: anAthensCanvas [
	^ AthensBorderStrokePaint
		topLeftColor: self topLeftColor
		bottomRightColor: self bottomRightColor
		width: self width
		on: anAthensCanvas
]
