blinking
switchColor
	self color = self invisibleCursorColor
		ifTrue: [ self color: self actualCursorColor ]
		ifFalse: [ self beNotVisible ].
