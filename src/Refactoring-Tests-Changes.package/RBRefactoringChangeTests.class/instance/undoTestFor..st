utilities
undoTestFor: aChange
	| undo |
	undo := aChange asUndoOperation.
	self assert: (undo isKindOf: RBRefactoryChange)