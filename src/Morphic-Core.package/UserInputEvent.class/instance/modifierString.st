printing
modifierString
	"Return a string identifying the currently pressed modifiers"
	| string |
	string := ''.
	self commandKeyPressed ifTrue:[string := string,'CMD '].
	self shiftPressed ifTrue:[string := string,'SHIFT '].
	self controlKeyPressed ifTrue:[string := string,'CTRL '].
	^string