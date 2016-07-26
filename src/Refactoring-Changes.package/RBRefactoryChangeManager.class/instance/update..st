updating
update: anEvent
	isPerformingRefactoring
		ifFalse: [ self clearUndoRedoList ]