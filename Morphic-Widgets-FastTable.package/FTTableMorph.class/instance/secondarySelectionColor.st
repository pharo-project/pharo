accessing colors
secondarySelectionColor 
	^ secondarySelectionColor ifNil: [ self class defaultSecondarySelectionColor ]