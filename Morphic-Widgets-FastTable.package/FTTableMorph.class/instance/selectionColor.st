accessing colors
selectionColor
	^ selectionColor ifNil: [ self class defaultSelectionColor ]