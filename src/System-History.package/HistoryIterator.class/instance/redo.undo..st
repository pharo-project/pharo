undo-undo
redo: doMessageSend undo: undoMessageSend
	self addItem: (UndoRedoRecord redo: doMessageSend undo: undoMessageSend)