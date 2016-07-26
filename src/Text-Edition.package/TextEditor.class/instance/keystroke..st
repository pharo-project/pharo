typing support
keystroke: aKeyboardEvent
	self handleEditionAction: [ self dispatchOn: aKeyboardEvent ] 
		fromKeyboardEvent: aKeyboardEvent.