event handling
mouseDown: evt 
	"Make this TextMorph be the keyboard input focus, if it isn't  
	already, and repond to the text selection gesture"
	
	evt yellowButtonPressed
		ifTrue: ["First check for option (menu) click"
			(self yellowButtonActivity: evt shiftPressed) 
				ifTrue: [ ^ self ]].
	self handleInteraction: [editor mouseDown: evt].
	self hasKeyboardFocus
		ifFalse: [self takeKeyboardFocus].
	super mouseDown: evt