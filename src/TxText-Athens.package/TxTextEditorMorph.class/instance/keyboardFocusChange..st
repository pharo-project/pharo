focus management
keyboardFocusChange: gotFocus

	super keyboardFocusChange: gotFocus.
	
	cursorMorph visible: gotFocus; stopStepping.