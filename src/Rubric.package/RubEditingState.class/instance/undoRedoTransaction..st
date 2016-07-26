undo-redo
undoRedoTransaction: aBlock
	self undoManager openRecordGroup.
	aBlock ensure: [  self undoManager closeRecordGroup ].
