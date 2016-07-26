accessing colors
colorForSelection: primarySelection
	
	^primarySelection 
		ifTrue: [ self selectionColor ]
		ifFalse: [ self secondarySelectionColor ]