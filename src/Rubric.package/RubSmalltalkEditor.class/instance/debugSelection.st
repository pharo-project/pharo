do-its
debugSelection
	"Treat the current selection as an expression; evaluate and debugg it in a new debugger."
	
	self lineSelectAndEmptyCheck: [^self].
	self debug: self selectionAsStream