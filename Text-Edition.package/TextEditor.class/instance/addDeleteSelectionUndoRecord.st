undo-redo private
addDeleteSelectionUndoRecord
	| undoText redoText undoRec |
	undoText := self selection.
	redoText := self nullText.
	self editingState
		redoArray: { morph. #undoTypeIn:interval:.  {redoText. self selectionInterval} } 
		undoArray: {morph. #redoTypeIn:interval:.  {undoText. self selectionInterval first to: self selectionInterval first - 1} }.
