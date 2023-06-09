Extension { #name : #PluggableButtonMorph }

{ #category : #'*Athens-Morphic' }
PluggableButtonMorph >> drawSubmorphsOnAthensCanvas: anAthensCanvas [
	super 	drawSubmorphsOnAthensCanvas: anAthensCanvas.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOnAthensCanvas: anAthensCanvas]
]
