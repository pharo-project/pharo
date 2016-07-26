drawing
drawOn: aCanvas
	"Draw the receiver on the canvas."

	self perform: #drawOn: withArguments: {aCanvas} inSuperclass: Morph.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOn: aCanvas]