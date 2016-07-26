accessing
selectionColor
	^ self textArea hasFocus
		ifTrue: [ self theme selectionColor ]
		ifFalse: [ self theme unfocusedSelectionColor alphaMixed: 0.3 with: Color veryLightGray lighter ]