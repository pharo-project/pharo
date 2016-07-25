undo-undo
doAndAddRecord: anUndoRedoRecord
	| result |
	result := anUndoRedoRecord do.
	self addItem: anUndoRedoRecord.
	^ result