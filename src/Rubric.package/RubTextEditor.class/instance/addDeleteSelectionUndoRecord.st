undo-redo private
addDeleteSelectionUndoRecord
	| undoText redoText undoRec |
	undoText := self selection.
	redoText := self nullText.
	self
		redoArray:	{textArea. #undoTypeIn:interval:.	{redoText.	(self selectionInterval)}}
		undoArray:	{textArea. #redoTypeIn:interval:.	{undoText.	(self selectionInterval first to: self selectionInterval first - 1)}}