accessing
replace: oldInterval with: newString and: selectingBlock 
	"Replace the text in oldInterval with newString and execute selectingBlock to establish the new selection."

	self selectionInterval = oldInterval ifFalse: [self selectInterval: oldInterval].
	self zapSelectionWith: newString.
	selectingBlock value