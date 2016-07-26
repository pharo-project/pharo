typing support
processKeyStroke: aKeyboardEvent

	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters.  Undoer & Redoer: see closeTypeIn."

	(self dispatchOn: aKeyboardEvent) 
		ifTrue: [self doneTyping.
			^self].
	self openTypeIn.
	self unselect