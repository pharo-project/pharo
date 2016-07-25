accessing
secondarySelectionTextColor
	"Answer the value of selectionTextColor"

	^ secondarySelectionTextColor 
		ifNil: [ self secondarySelectionColor contrastingColor ]