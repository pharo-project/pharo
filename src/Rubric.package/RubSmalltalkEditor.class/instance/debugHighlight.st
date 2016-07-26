do-its
debugHighlight
	"Treat the current highlight as an expression; evaluate and debugg it in a new debugger."

	self highlightAndEmptyCheck: [ ^ self ].
	self debug: self highlightedTextAsStream