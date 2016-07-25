mouse
waitNoButton
	"Wait for the user to release any mouse button and then answer the current location of the cursor."

	| delay |
	delay := Delay forMilliseconds: 50.
	[self anyButtonPressed] whileTrue: [ delay wait].
	^self cursorPoint
