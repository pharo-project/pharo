event handling
mouseMove: evt 
	"Handle a mouse move event. Select the color at the mouse position."
	
	evt redButtonPressed
		ifFalse: [^super mouseMove: evt].
	self selectColorAt: evt position.
	^super mouseMove: evt