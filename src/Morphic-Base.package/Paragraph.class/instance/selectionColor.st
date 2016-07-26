selection
selectionColor
	^ self focused 
		ifTrue: [ self theme selectionColor]
		ifFalse: [ self theme unfocusedSelectionColor ].
