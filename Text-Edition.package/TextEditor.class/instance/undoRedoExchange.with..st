undoers-redoers
undoRedoExchange: aninterval with: anotherInterval
	self selectInvisiblyFrom: aninterval first to: aninterval last.
	self exchangeWith: anotherInterval
