accessing
recorder
	^ recorder ifNil: [recorder := UndoRedoGroup new]