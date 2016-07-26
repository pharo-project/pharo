accessing
selectionColorToUse
	"Answer the colour to use for selected items."

	^ selectionColorToUse ifNil: [self theme settings selectionColor] 