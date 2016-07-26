accessing
replace: xoldInterval with: newText and: selectingBlock 
	"Replace the text in oldInterval with newText and
	execute selectingBlock to establish the new selection.
	Create an UndoRecord to allow perfect undoing."
	| prevSel currInterval |
	self selectInterval: xoldInterval.
	prevSel := self selection.
	currInterval := self selectionInterval.
	self editingState previousInterval: currInterval.
	self zapSelectionWith: newText.
	selectingBlock value.
	((prevSel isEmpty and: [newText isEmpty]) and: [currInterval size < 1]) 
		ifFalse: [self 
			redoArray: {textArea. #redoTypeIn:interval:. {newText. currInterval}} 
			undoArray: {textArea. #undoTypeIn:interval:. {prevSel. currInterval first to: currInterval first + newText size - 1}}]