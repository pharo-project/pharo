undo-undo
redoArray: doArray undoArray: undoArray
	self addItem: (UndoRedoRecord redoArray: doArray undoArray: undoArray)