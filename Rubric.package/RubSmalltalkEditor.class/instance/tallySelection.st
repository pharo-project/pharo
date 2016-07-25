do-its
tallySelection
	"Treat the current selection as an expression; evaluate and tally it."

	self lineSelectAndEmptyCheck: [ ^ self ].
	self lineSelectAndEmptyCheck: [ ^ self ].
	self tally: self selectionAsStream
	
