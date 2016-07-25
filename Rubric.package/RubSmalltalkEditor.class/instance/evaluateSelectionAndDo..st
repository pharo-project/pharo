do-its
evaluateSelectionAndDo: aBlock
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result.
	If no selection is present select the current line."

	self lineSelectAndEmptyCheck: [^ ''].
	^ self 
		evaluate: self selectionForDoitAsStream 
		andDo: aBlock