accessing
selectionColor
	^ self textArea hasFocus
		ifTrue: [ self theme selectionColor ]
		ifFalse: [ self theme lessConspicuousColorFrom: (self theme unfocusedSelectionColor) ]