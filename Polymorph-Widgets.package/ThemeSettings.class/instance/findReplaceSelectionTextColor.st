accessing
findReplaceSelectionTextColor
	"Answer the value of the selectionTextColor for find/replace"

	^ findReplaceSelectionTextColor 
		ifNil: [ self findReplaceSelectionColor contrastingColor ]