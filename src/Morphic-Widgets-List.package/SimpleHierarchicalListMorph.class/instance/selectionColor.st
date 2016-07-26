accessing
selectionColor
	"Answer the colour to use for selected items."

	^self valueOfProperty: #selectionColor ifAbsent: [] 