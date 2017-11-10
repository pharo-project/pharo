Extension { #name : #EmbeddedMenuMorph }

{ #category : #'*Athens-Morphic' }
EmbeddedMenuMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Draw the receiver on the canvas."

	self perform: #drawOnAthensCanvas: withArguments: {anAthensCanvas} inSuperclass: Morph.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOnAthensCanvas: anAthensCanvas]
]
