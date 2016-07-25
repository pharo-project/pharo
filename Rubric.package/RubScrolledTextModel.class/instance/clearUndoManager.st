undo-redo
clearUndoManager
	self reconfigureViewWith: [ :view | view textArea editingState clearUndoManager: nil ]