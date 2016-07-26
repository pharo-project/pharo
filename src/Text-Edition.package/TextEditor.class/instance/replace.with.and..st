accessing
replace: xoldInterval with: newText and: selectingBlock 
	"Replace the text in oldInterval with newText and
	execute selectingBlock to establish the new selection.
	Create an UndoRecord to allow perfect undoing."
	| prevSel currInterval undoRec |
	self selectInterval: xoldInterval.
	prevSel := self selection.
	currInterval := self selectionInterval.
	self editingState previousInterval: currInterval selection: self selection.
	self zapSelectionWith: newText.
	selectingBlock value.
	((prevSel isEmpty and: [newText isEmpty]) and: [currInterval size < 1]) 
		ifFalse: [self editingState
			redoArray: {morph. #redoTypeIn:interval:. {newText. currInterval}} 
			undoArray: {morph. #undoTypeIn:interval:. {prevSel. currInterval first to: currInterval first + newText size - 1}}]