accessing
selectionColorToUse
	"Answer the colour to use for selected items."

	^self 
		valueOfProperty: #selectionColorToUse 
		ifAbsent: [ self theme selectionColor ]