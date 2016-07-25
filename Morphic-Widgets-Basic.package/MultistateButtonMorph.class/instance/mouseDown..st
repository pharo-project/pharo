event handling
mouseDown: evt 
	"Handle a mouse down event."
	
	super mouseDown: evt.
	self enabled ifFalse: [^self].
	self down: true.