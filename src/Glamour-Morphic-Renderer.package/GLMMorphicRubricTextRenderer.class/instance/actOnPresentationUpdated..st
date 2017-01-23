private
actOnPresentationUpdated: ann 
	| anInterval |
	anInterval := ann presentation selectionInterval.
	textMorph setText: ann presentation formatedDisplayValue.
	textMorph setSelection: anInterval