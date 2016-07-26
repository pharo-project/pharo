accessing
selectionColor: aColor
	"Set the colour to use for the text selection."

	selectionColor := aColor.
	self textMorph 
		ifNotNil: [:t | t selectionColor: aColor]