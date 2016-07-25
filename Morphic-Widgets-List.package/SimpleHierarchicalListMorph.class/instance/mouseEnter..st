event handling
mouseEnter: event
	"Changed to take keyboardFocusOnMouseDown into account."
	
	super mouseEnter: event.
	self wantsKeyboardFocus ifFalse: [^self].
	self keyboardFocusOnMouseDown
		ifFalse: [self takeKeyboardFocus]