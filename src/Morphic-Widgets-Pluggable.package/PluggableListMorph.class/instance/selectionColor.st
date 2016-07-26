accessing
selectionColor
	"Answer the colour to use for selected items."

	^ selectionColor ifNil: [self theme selectionColor] 