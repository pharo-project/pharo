Extension { #name : #CheckboxMorph }

{ #category : #'*Athens-Morphic' }
CheckboxMorph >> drawSubmorphsOnAthensCanvas: anAthensCanvas [
	super 	drawSubmorphsOnAthensCanvas: anAthensCanvas.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOnAthensCanvas: anAthensCanvas]
]
