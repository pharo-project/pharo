Extension { #name : #ScrollPane }

{ #category : #'*Athens-Morphic' }
ScrollPane >> drawSubmorphsOnAthensCanvas: anAthensCanvas [
	super 	drawSubmorphsOnAthensCanvas: anAthensCanvas.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOnAthensCanvas: anAthensCanvas]
]
