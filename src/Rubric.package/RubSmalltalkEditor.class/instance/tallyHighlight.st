do-its
tallyHighlight
	"Treat the current highlight as an expression; evaluate and tally it."
	
	self highlightAndEmptyCheck: [ ^ self ].
	self tally: self highlightedTextAsStream
	
