blinking
beVisible
	self color = self invisibleCursorColor
		ifTrue: [ self color: self actualCursorColor ]