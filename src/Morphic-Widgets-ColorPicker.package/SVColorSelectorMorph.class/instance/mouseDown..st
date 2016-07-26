event handling
mouseDown: evt 
	"Handle a mouse down event. Select the color at the mouse position."
	
	evt redButtonPressed
		ifFalse: [^super mouseDown: evt].
	evt hand showTemporaryCursor: (Cursor crossHair copy offset: -9 @ -9).
	self hideLocation.
	self selectColorAt: evt position.
	^super mouseDown: evt