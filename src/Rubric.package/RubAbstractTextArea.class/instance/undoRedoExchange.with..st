multi level undo
undoRedoExchange: aninterval with: anotherInterval
	self handleEdit: [self editor undoRedoExchange: aninterval with: anotherInterval]