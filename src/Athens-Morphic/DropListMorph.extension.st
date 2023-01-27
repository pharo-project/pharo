Extension { #name : #DropListMorph }

{ #category : #'*Athens-Morphic' }
DropListMorph >> drawSubmorphsOnAthensCanvas: anAthensCanvas [
	super 	drawSubmorphsOnAthensCanvas: anAthensCanvas.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOnAthensCanvas: anAthensCanvas]
]
