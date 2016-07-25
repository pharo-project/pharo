accessing
updateTextWith: aText
	self
		handleTextChangeWhile: [ 
			ghostText updateTextWith: aText.
			ghostText textColor: self textColor ]