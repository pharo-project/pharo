mouse
waitButton
	"Wait for the user to press any mouse button and then answer with the 
	current location of the cursor."

	| delay |
	delay := Delay forMilliseconds: 50.
	[self anyButtonPressed] whileFalse: [ delay wait ].
	^self cursorPoint
