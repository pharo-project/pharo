*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas
	"Draw the receiver on the canvas."

	self perform: #drawOnAthensCanvas: withArguments: {anAthensCanvas} inSuperclass: Morph.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOnAthensCanvas: anAthensCanvas]