events
click: event
	| p |
	self closeTypeIn.
	p := self paragraph characterBlockAtPoint: event cursorPoint.
	textArea markBlock: p pointBlock: p.
	self setEmphasisHereFromText.
	self storeSelectionInText.
	textArea announce: (RubMouseClick with: event)