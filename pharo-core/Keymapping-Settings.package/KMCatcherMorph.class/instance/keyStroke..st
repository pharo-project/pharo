event handling
keyStroke: aKeyStroke 
	edited := true.
	self keystrokes add: aKeyStroke.
	self showKeystrokes.