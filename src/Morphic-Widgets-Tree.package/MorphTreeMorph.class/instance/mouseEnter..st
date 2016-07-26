events-processing
mouseEnter: event
	"Changed to take keyboardFocusOnMouseDown preference into account."
	
	super mouseEnter: event.
	self wantsKeyboardFocus ifFalse: [^self].
	self keyboardFocusOnMouseDown ifFalse: [self takeKeyboardFocus]