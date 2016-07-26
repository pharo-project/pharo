initialization
initialize
	undo := OrderedCollection new.
	redo := OrderedCollection new.
	isPerformingRefactoring := false.
	self connectToChanges