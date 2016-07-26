typing/selecting keys
findAgain: aKeyboardEvent 
	"Find the desired text again."
	self closeTypeIn.
	self findAgain.
	^ true