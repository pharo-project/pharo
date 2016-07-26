undo-redo
redoArray: doArray undoArray: undoArray
	self undoManager isPlugged 
		ifTrue: [self undoManager redoArray: doArray undoArray: undoArray]