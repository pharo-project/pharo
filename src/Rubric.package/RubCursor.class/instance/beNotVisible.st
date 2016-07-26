blinking
beNotVisible
	self color = self invisibleCursorColor
		ifFalse: [ self color: self invisibleCursorColor ]