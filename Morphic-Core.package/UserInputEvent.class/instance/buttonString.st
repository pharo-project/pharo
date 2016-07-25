printing
buttonString
	"Return a string identifying the currently pressed buttons"
	| string |
	string := ''.
	self redButtonPressed ifTrue:[string := string,'red '].
	self yellowButtonPressed ifTrue:[string := string,'yellow '].
	self blueButtonPressed ifTrue:[string := string,'blue '].
	^string