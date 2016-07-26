typing support
processKeyStroke: aKeyboardEvent
	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters.  Undoer & Redoer: see closeTypeIn."
	
	^self handleKeystrokeAction: [self dispatchOn: aKeyboardEvent].