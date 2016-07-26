mouse
waitClickButton
	"Wait for the user to click (press and then release) any mouse button and 
	then answer with the current location of the cursor."

	self waitButton.
	^self waitNoButton