event handling
keyStroke: evt
	"Check for cursor keys"
	| keyValue |
	owner isHandMorph ifFalse:[^self].
	keyValue := evt keyValue.
	keyValue = 28 ifTrue:[^self position: self position - (1@0)].
	keyValue = 29 ifTrue:[^self position: self position + (1@0)].
	keyValue = 30 ifTrue:[^self position: self position - (0@1)].
	keyValue = 31 ifTrue:[^self position: self position + (0@1)].
	"Special case for return"
	keyValue = 13 ifTrue:[
		"Drop the receiver and be done"
	self flag: #arNote. "Probably unnecessary"
		owner releaseKeyboardFocus: self.
		self delete].
