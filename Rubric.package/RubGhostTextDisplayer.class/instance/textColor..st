accessing
textColor: aColor
	textColor := aColor.
	self
		handleTextChangeWhile: [ ghostText textColor: aColor ]