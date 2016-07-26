undo-redo private
clearUndoManager: aKeyboardEvent
	^ self editingState clearUndoManager: aKeyboardEvent.
